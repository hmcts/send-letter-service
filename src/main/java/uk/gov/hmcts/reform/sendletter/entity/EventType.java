package uk.gov.hmcts.reform.sendletter.entity;

public enum EventType {
    MANUALLY_MARKED_AS_CREATED,
    MANUALLY_MARKED_AS_NOT_SENT,
    MANUALLY_MARKED_AS_ABORTED,
    MANUALLY_MARKED_AS_POSTED_LOCALLY,
    FAILED_TO_UPLOAD
}
