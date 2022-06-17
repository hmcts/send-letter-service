package uk.gov.hmcts.reform.sendletter.exception;

public class InvalidApiKeyException extends RuntimeException {

    public InvalidApiKeyException(String message) {
        super(message);
    }
}
