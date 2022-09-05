package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToMarkLetterPostedException extends RuntimeException {

    public UnableToMarkLetterPostedException(String message) {
        super(message);
    }
}
