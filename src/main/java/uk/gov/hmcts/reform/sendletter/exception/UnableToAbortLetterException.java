package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToAbortLetterException extends RuntimeException {

    public UnableToAbortLetterException(String message) {
        super(message);
    }
}
