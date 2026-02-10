package uk.gov.hmcts.reform.sendletter.model.out;

import lombok.Data;

import java.time.LocalDate;

@Data
public class PostedReportTaskResponse {
    final String serviceName;
    final LocalDate reportDate;
    long markedPostedCount = 0;

    boolean processingFailed = false;
    String errorMessage = null;

    public void markAsFailed(String errorMessage) {
        this.processingFailed = true;
        this.errorMessage = errorMessage;
    }
}
