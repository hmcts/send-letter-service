package uk.gov.hmcts.reform.sendletter.exception;

public class InvalidPdfException extends RuntimeException {
    public InvalidPdfException(Throwable cause) {
        super(cause);
    }
}
