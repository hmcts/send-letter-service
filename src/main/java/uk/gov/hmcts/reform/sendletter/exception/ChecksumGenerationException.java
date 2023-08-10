package uk.gov.hmcts.reform.sendletter.exception;

public class ChecksumGenerationException extends RuntimeException {
    public ChecksumGenerationException(String message) {
        super(message);
    }
}
