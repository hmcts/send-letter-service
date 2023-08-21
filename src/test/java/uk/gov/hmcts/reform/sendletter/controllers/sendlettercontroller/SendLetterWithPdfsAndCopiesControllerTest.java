package uk.gov.hmcts.reform.sendletter.controllers.sendlettercontroller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.controllers.SendLetterController;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.exception.UnauthenticatedException;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.LetterChecksumService;
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
class SendLetterWithPdfsAndCopiesControllerTest {

    @Autowired private MockMvc mockMvc;

    @MockBean private LetterService letterService;
    @MockBean private AuthService authService;
    @MockBean
    private LaunchDarklyClient launchDarklyClient;
    @MockBean
    private LetterChecksumService letterChecksumService;

    @BeforeEach
    public void beforeAll() {
        given(launchDarklyClient.isFeatureEnabled("FACT-1388")).willReturn(true);
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_call_service(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");

        // when
        sendLetter(getLetterJson("controller/letter/v3/valid_letter.json"), async);

        // then
        verify(letterService).save(any(LetterWithPdfsAndNumberOfCopiesRequest.class), anyString(), eq(async));
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_authenticate_calls(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");
        final String authHeader = "auth-header-value";

        // when
        mockMvc.perform(
            post("/letters")
                .queryParam("async", async)
                .contentType(MediaTypes.LETTER_V3)
                .header("ServiceAuthorization", authHeader)
                .content(getLetterJson("controller/letter/v3/valid_letter.json"))
        );

        // then
        verify(authService).authenticate(authHeader);
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_validate_number_of_copies(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");

        sendLetter(getLetterJson("controller/letter/v3/invalid_copies.json"), async)
            .andExpect(status().isBadRequest());
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_return_403_if_service_throws_ServiceNotConfiguredException(String async) throws Exception {
        given(authService.authenticate(anyString())).willReturn("some_service_name");
        given(letterService.save(any(), any(), eq(async)))
                .willThrow(new ServiceNotConfiguredException("invalid service"));

        sendLetter(getLetterJson("controller/letter/v3/valid_letter.json"), async)
            .andExpect(status().isForbidden());
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void should_return_401_if_service_throws_UnauthenticatedException(String async) throws Exception {
        given(authService.authenticate(anyString())).willThrow(new UnauthenticatedException("Hello"));

        sendLetter(getLetterJson("controller/letter/v3/valid_letter.json"), async)
            .andExpect(status().isUnauthorized());
    }


    private String getPostUrl(String async) {
        String url = "/letters";
        if (Boolean.parseBoolean(async)) {
            url = join("", url, "?isAsync=true");
        }
        return url;
    }

    private ResultActions sendLetter(String json, String async) throws Exception {
        return mockMvc.perform(
            post(getPostUrl(async))
                .contentType(MediaTypes.LETTER_V3)
                .header("ServiceAuthorization", "auth-header-value")
                .content(json)
        );
    }

    private String getLetterJson(String path) throws Exception {
        return loadJson(path);
    }
}
