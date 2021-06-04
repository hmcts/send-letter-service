package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToGenerateSasTokenException extends RuntimeException {

    public UnableToGenerateSasTokenException(Throwable throwable) {
        super(throwable);
    }
}
