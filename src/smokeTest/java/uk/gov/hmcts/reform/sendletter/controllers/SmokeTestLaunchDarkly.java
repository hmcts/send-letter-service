package uk.gov.hmcts.reform.sendletter.controllers;

import com.launchdarkly.sdk.server.interfaces.DataSourceStatusProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClientFactory;

import static org.assertj.core.api.Assertions.assertThat;
import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.SEND_LETTER_SERVICE_TEST;

@TestPropertySource("classpath:application.properties")
@ExtendWith(SpringExtension.class)
class SmokeTestLaunchDarkly {

    @Value("${sdk-key:YYYYY}")
    private String sdkKey;

    @Value("${offline-mode:false}")
    private Boolean offlineMode;

    @Test
    void checkLaunchDarklyStatus() throws InterruptedException {
        LaunchDarklyClientFactory ldFactory = new LaunchDarklyClientFactory();
        LaunchDarklyClient ldClient = new LaunchDarklyClient(ldFactory, sdkKey, offlineMode);

        long startTime = System.currentTimeMillis();
        long endTime = startTime + 60000; // One minute in milliseconds

        DataSourceStatusProvider.Status ldStatus;

        do {
            ldStatus = ldClient.getDataSourceStatus();
            if (ldStatus.getState() == DataSourceStatusProvider.State.VALID) {
                break; // Exit the loop if status is VALID
            }
            Thread.sleep(1000); // Wait for 1 second before polling again
        } while (System.currentTimeMillis() < endTime);

        assertThat(ldStatus.getState()).isEqualTo(DataSourceStatusProvider.State.VALID);
    }

    @Test
    void checkLaunchDarklyTestFlag() {
        LaunchDarklyClientFactory ldFactory = new LaunchDarklyClientFactory();
        LaunchDarklyClient ldClient = new LaunchDarklyClient(ldFactory, sdkKey, offlineMode);

        Boolean testFeatureBoolean = ldClient.isFeatureEnabled(SEND_LETTER_SERVICE_TEST);
        assertThat(testFeatureBoolean).isTrue();
        //SEND_LETTER_SERVICE_TEST is a test flag only and needs to be set to TRUE within LD.
    }
}
