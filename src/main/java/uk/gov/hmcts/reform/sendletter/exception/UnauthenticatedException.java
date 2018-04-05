package uk.gov.hmcts.reform.sendletter.exception;

import uk.gov.hmcts.reform.logging.exception.AlertLevel;
import uk.gov.hmcts.reform.logging.exception.UnknownErrorCodeException;

/**
 * SonarQube reports as error. Max allowed - 5 parents
 */
@SuppressWarnings("squid:MaximumInheritanceDepth")
public class UnauthenticatedException extends UnknownErrorCodeException {
    public UnauthenticatedException(String message) {
        super(AlertLevel.P3, message);
    }

    /**
     * Used to wrap InvalidTokenException thrown by authorisation library.
     *
     * @param cause Original InvalidTokenException
     */
    public UnauthenticatedException(Throwable cause) {
        super(AlertLevel.P4, cause);
    }
}
