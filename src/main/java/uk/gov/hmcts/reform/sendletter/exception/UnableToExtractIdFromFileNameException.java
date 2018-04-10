package uk.gov.hmcts.reform.sendletter.exception;

import uk.gov.hmcts.reform.logging.exception.AlertLevel;
import uk.gov.hmcts.reform.logging.exception.UnknownErrorCodeException;

/**
 * SonarQube reports as error. Max allowed - 5 parents
 */
@SuppressWarnings("squid:MaximumInheritanceDepth")
public class UnableToExtractIdFromFileNameException extends UnknownErrorCodeException {

    public UnableToExtractIdFromFileNameException(Exception inner) {
        super(AlertLevel.P4, inner);
    }

    public UnableToExtractIdFromFileNameException(String message) {
        super(AlertLevel.P4, message);
    }
}
