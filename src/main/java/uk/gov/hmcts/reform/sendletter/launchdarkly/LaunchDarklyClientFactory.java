package uk.gov.hmcts.reform.sendletter.launchdarkly;

import com.launchdarkly.sdk.server.LDClient;
import com.launchdarkly.sdk.server.LDConfig;
import com.launchdarkly.sdk.server.interfaces.LDClientInterface;
import org.springframework.stereotype.Service;

/**
 * A factory for creating LaunchDarkly clients.
 */
@Service
public class LaunchDarklyClientFactory {
    /**
     * Create a new LaunchDarkly client.
     * @param sdkKey The SDK key for LaunchDarkly
     * @param offlineMode Whether to use offline mode
     * @return The new LaunchDarkly client
     */
    public LDClientInterface create(String sdkKey, boolean offlineMode) {
        LDConfig config = new LDConfig.Builder()
            .offline(offlineMode)
            .build();
        return new LDClient(sdkKey, config);
    }
}
