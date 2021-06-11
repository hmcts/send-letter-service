package uk.gov.hmcts.reform.sendletter.exception;

public class LeaseIdNotPresentException extends RuntimeException {
    public LeaseIdNotPresentException(String message) {
        super(message);
    }
}
