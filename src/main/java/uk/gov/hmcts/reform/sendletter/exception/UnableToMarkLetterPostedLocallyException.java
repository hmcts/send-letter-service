package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToMarkLetterPostedLocallyException extends RuntimeException {

    public UnableToMarkLetterPostedLocallyException(String message) {
        super(message);
    }
}
