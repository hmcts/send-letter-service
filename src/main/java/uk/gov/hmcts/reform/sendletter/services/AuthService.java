package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.sendletter.exception.UnauthenticatedException;

/**
 * Service for authentication.
 */
@Component
public class AuthService {

    private final AuthTokenValidator authTokenValidator;

    /**
     * Constructor.
     *
     * @param authTokenValidator the auth token validator
     */
    public AuthService(AuthTokenValidator authTokenValidator) {
        this.authTokenValidator = authTokenValidator;
    }

    /**
     * Authenticate.
     *
     * @param authHeader the auth header
     * @return the string
     */
    public String authenticate(String authHeader) {
        if (authHeader == null) {
            throw new UnauthenticatedException("Missing ServiceAuthorization header");
        } else {
            return authTokenValidator.getServiceName(authHeader);
        }
    }
}
