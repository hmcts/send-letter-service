package uk.gov.hmcts.reform.sendletter.exception;

import uk.gov.hmcts.reform.logging.exception.AlertLevel;
import uk.gov.hmcts.reform.logging.exception.UnknownErrorCodeException;

public class UnsupportedLetterRequestTypeException extends UnknownErrorCodeException {
    protected UnsupportedLetterRequestTypeException() {
        super(AlertLevel.P1, "Unsupported lette request type");
    }
}
