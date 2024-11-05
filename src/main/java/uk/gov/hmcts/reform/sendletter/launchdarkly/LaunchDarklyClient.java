package uk.gov.hmcts.reform.sendletter.launchdarkly;

import com.launchdarkly.sdk.LDContext;
import com.launchdarkly.sdk.server.interfaces.DataSourceStatusProvider;
import com.launchdarkly.sdk.server.interfaces.LDClientInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * A wrapper around the LaunchDarkly client.
 */
@Service
public class LaunchDarklyClient {
    /**
     * The user to use when checking feature flags.
     */
    public final LDContext sendLetterServiceContext;

    private final LDClientInterface internalClient;

    /**
     * Constructor for the LaunchDarklyClient.
     * @param launchDarklyClientFactory The factory for creating the LaunchDarkly client
     * @param sdkKey The SDK key for LaunchDarkly
     * @param offlineMode Whether to use offline mode
     */
    @Autowired
    public LaunchDarklyClient(
        LaunchDarklyClientFactory launchDarklyClientFactory,
        @Value("${launchdarkly.sdk-key:YYYYY}") String sdkKey,
        @Value("${launchdarkly.offline-mode:false}") Boolean offlineMode
    ) {
        this.internalClient = launchDarklyClientFactory.create(sdkKey, offlineMode);
        this.sendLetterServiceContext =  LDContext.builder(sdkKey).build();
    }

    /**
     * Check if a feature is enabled.
     * @param feature The feature to check
     * @return Whether the feature is enabled
     */
    public boolean isFeatureEnabled(String feature) {
        return internalClient.boolVariation(feature, sendLetterServiceContext, false);
    }

    /**
     * Check if a feature is enabled for a user.
     * @param feature The feature to check
     * @param user The user to check for
     * @return Whether the feature is enabled
     */
    public boolean isFeatureEnabled(String feature, LDContext user) {
        return internalClient.boolVariation(feature, user, false);
    }

    /**
     * Get the status of the data source.
     * @return The status of the data source
     */
    public DataSourceStatusProvider.Status getDataSourceStatus() {
        return internalClient.getDataSourceStatusProvider().getStatus();
    }
}
