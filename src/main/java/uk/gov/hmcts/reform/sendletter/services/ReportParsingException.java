package uk.gov.hmcts.reform.sendletter.services;

import uk.gov.hmcts.reform.logging.exception.AlertLevel;
import uk.gov.hmcts.reform.logging.exception.UnknownErrorCodeException;

/**
 * SonarQube reports as error. Max allowed - 5 parents
 */
@SuppressWarnings("squid:MaximumInheritanceDepth")
public class ReportParsingException extends UnknownErrorCodeException {
    public ReportParsingException(Throwable cause) {
        super(AlertLevel.P2, cause);
    }
}
