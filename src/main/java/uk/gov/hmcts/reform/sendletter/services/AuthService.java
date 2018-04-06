package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.sendletter.exception.UnauthenticatedException;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

@Component
public class AuthService {

    private final AuthTokenValidator authTokenValidator;
    private final AppInsights insights;

    public AuthService(AuthTokenValidator authTokenValidator, AppInsights insights) {
        this.authTokenValidator = authTokenValidator;
        this.insights = insights;
    }

    public String authenticate(String authHeader) {
        if (authHeader == null) {
            throw new UnauthenticatedException("Missing ServiceAuthorization header");
        } else {
            return insights.trackServiceAuthentication(() -> authTokenValidator.getServiceName(authHeader));
        }
    }
}
