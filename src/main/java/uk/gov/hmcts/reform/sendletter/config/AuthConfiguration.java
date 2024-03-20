package uk.gov.hmcts.reform.sendletter.config;

import org.apache.commons.lang.NotImplementedException;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.gov.hmcts.reform.authorisation.ServiceAuthorisationApi;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.authorisation.validators.ServiceAuthTokenValidator;

import java.util.List;

/**
 * Configuration for authorisation.
 */
@Configuration
public class AuthConfiguration {

    /**
     * Create a bean of AuthTokenValidator.
     * @param s2sApi The service authorisation API
     * @return The AuthTokenValidator
     */
    @Bean
    @ConditionalOnProperty(name = "idam.s2s-auth.url")
    public AuthTokenValidator tokenValidator(ServiceAuthorisationApi s2sApi) {
        return new ServiceAuthTokenValidator(s2sApi);
    }

    /**
     * Create a bean of AuthTokenValidator.
     * @return The AuthTokenValidator
     */
    @Bean
    @ConditionalOnProperty(name = "idam.s2s-auth.url", havingValue = "false")
    public AuthTokenValidator tokenValidatorStub() {
        return new AuthTokenValidator() {
            /**
             * Validate a token.
             * @param token The token to validate
             */
            public void validate(String token) {
                throw new NotImplementedException();
            }

            /**
             * Validate a token.
             * @param token The token to validate
             * @param roles The roles to validate
             */
            public void validate(String token, List<String> roles) {
                throw new NotImplementedException();
            }

            /**
             * Get the service name.
             * @param token The token
             * @return The service name
             */
            public String getServiceName(String token) {
                return "some_service_name";
            }
        };
    }
}
