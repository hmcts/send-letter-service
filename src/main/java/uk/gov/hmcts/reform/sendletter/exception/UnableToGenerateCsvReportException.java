package uk.gov.hmcts.reform.sendletter.exception;

public class UnableToGenerateCsvReportException extends RuntimeException {
    public UnableToGenerateCsvReportException(Throwable cause) {
        super(cause);
    }
}
