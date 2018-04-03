package uk.gov.hmcts.reform.sendletter.controllers;

import com.google.common.io.Resources;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.LetterService;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest
public class SendLetterWithPdfsControllerTest {

    @Autowired private MockMvc mockMvc;

    @MockBean private LetterService letterService;
    @MockBean private AuthService authService;

    @Test
    public void should_call_new_service_method() throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");

        // when
        mockMvc
            .perform(
                post("/letters")
                    .contentType(MediaTypes.LETTER_V2)
                    .header("ServiceAuthorization", "auth-header-value")
                    .content(Resources.toString(getResource("controller/letter/v2/letter.json"), UTF_8))
            )
            .andExpect(status().isOk());

        // then
        verify(letterService).send(any(LetterWithPdfsRequest.class), anyString());
    }

    @Test
    public void should_authenticate_calls() throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");
        final String authHeader = "auth-header-value";

        // when
        mockMvc.perform(
            post("/letters")
                .contentType(MediaTypes.LETTER_V2)
                .header("ServiceAuthorization", authHeader)
                .content(Resources.toString(getResource("controller/letter/v2/letter.json"), UTF_8))
        );

        // then
        verify(authService).authenticate(eq(authHeader));
    }

    private ResultActions sendLetter(String json) throws Exception {
        return mockMvc.perform(
            post("/letters")
                .contentType(MediaTypes.LETTER_V2)
                .header("ServiceAuthorization", "auth-header-value")
                .content(json)
        );
    }
}
