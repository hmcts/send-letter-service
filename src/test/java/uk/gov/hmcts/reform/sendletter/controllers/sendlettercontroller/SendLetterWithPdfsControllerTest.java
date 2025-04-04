package uk.gov.hmcts.reform.sendletter.controllers.sendlettercontroller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.controllers.SendLetterController;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.LetterService;

import static java.lang.String.join;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadJson;

@WebMvcTest(SendLetterController.class)
class SendLetterWithPdfsControllerTest {

    @Autowired private MockMvc mockMvc;

    @MockitoBean
    private LetterService letterService;

    @MockitoBean
    private AuthService authService;

    private String validJson;

    @BeforeEach
    void setUp() throws Exception {
        this.validJson = loadJson("controller/letter/v2/letter.json");
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_call_new_service_method(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");

        // when
        sendLetter(validJson, async);

        // then
        verify(letterService).save(any(LetterWithPdfsRequest.class), anyString(), eq(async));
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_authenticate_calls(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");
        final String authHeader = "auth-header-value";

        // when
        mockMvc.perform(
            post(getPostUrl(async))
                .contentType(MediaTypes.LETTER_V2)
                .header("ServiceAuthorization", authHeader)
                .content(validJson)
        );

        // then
        verify(authService).authenticate(authHeader);
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_return_403_if_service_throws_ServiceNotConfiguredException(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");
        given(letterService.save(any(), any(), eq(async)))
                .willThrow(new ServiceNotConfiguredException("invalid service"));

        sendLetter(validJson, async)
            .andExpect(status().isForbidden());
    }

    private ResultActions sendLetter(String json, String async) throws Exception {
        return mockMvc.perform(
            post(getPostUrl(async))
                .contentType(MediaTypes.LETTER_V2)
                .header("ServiceAuthorization", "auth-header-value")
                .content(json)
        );
    }


    private String getPostUrl(String async) {
        String url = "/letters";
        if (Boolean.parseBoolean(async)) {
            url = join("", url, "?isAsync=true");
        }
        return url;
    }
}
