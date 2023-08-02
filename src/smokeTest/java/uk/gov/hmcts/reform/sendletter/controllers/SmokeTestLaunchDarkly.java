package uk.gov.hmcts.reform.sendletter.controllers;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClientFactory;

import static org.assertj.core.api.Assertions.assertThat;

import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.SEND_LETTER_SERVICE_TEST;

@ExtendWith(SpringExtension.class)
class SmokeTestLaunchDarkly {

    @MockBean
    private LaunchDarklyClient lDclient;
    @MockBean
    private LaunchDarklyClientFactory lDfactory;

    @Value("${sdk-key}")
    private String sdkKey;

    @Value("${offline-mode}")
    private Boolean offlineMode;

    @BeforeEach
    void setUp() {
        lDfactory = new LaunchDarklyClientFactory();
        lDclient = new LaunchDarklyClient(lDfactory, sdkKey, offlineMode);
    }

    @Test
    void testFlagCheck() {
        Boolean test_feature = lDclient.isFeatureEnabled(SEND_LETTER_SERVICE_TEST);
        assertThat(test_feature).isTrue();
        //SEND_LETTER_SERVICE_TEST is a test flag only and needs to be set to TRUE within LD.
    }
}
