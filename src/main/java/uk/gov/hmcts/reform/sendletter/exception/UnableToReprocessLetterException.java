package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToReprocessLetterException extends RuntimeException {

    public UnableToReprocessLetterException(String message) {
        super(message);
    }
}
