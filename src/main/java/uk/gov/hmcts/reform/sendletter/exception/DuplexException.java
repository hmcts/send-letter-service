package uk.gov.hmcts.reform.sendletter.exception;

public class DuplexException extends RuntimeException {
    public DuplexException(String message, Throwable cause) {
        super(message, cause);
    }
}
