package uk.gov.hmcts.reform.sendletter.controllers;

import io.github.resilience4j.ratelimiter.annotation.RateLimiter;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

import static org.springframework.http.ResponseEntity.ok;

/**
 * Controller for feature flags.
 */
@RateLimiter(name = "default")
@RestController
public class FeatureFlagController {
    private final LaunchDarklyClient featureToggleService;

    /**
     * Construct a new FeatureFlagController.
     * @param featureToggleService the feature toggle service
     */
    public FeatureFlagController(LaunchDarklyClient featureToggleService) {
        this.featureToggleService = featureToggleService;
    }

    /**
     * Retrieves the status of a feature flag.
     * @param flag The name of the feature flag
     * @return The status of the feature flag
     */
    @GetMapping("/feature-flags/{flag}")
    public ResponseEntity<String> flagStatus(@PathVariable String flag) {
        boolean isEnabled = featureToggleService.isFeatureEnabled(flag);
        return ok(flag + " : " + isEnabled);
    }
}
