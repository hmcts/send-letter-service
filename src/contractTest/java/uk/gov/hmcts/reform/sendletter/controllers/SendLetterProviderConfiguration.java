package uk.gov.hmcts.reform.sendletter.controllers;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.LetterService;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;

@TestConfiguration
public class SendLetterProviderConfiguration {

    @MockBean
    AuthService authService;

    @MockBean
    LetterService letterService;

    @MockBean
    AuthTokenValidator authTokenValidator;

    @Bean
    @Primary
    SendLetterController sendLetterController() {
        return new SendLetterController(letterService, authService);

    }


}
