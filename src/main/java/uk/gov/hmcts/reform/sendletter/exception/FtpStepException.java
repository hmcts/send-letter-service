package uk.gov.hmcts.reform.sendletter.exception;

public class FtpStepException extends RuntimeException {
    public FtpStepException(String message, Throwable cause) {
        super(message, cause);
    }
}
