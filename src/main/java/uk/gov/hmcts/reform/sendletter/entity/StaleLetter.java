package uk.gov.hmcts.reform.sendletter.entity;

import java.util.Date;
import java.util.UUID;

public class StaleLetter {

    private final UUID id;

    private final String messageId;

    private final String service;

    private final String type;

    private final Date sentToPrintAt;

    public StaleLetter(
        final UUID id,
        final String messageId,
        final String service,
        final String type,
        final Date sentToPrintAt
    ) {
        this.id = id;
        this.messageId = messageId;
        this.service = service;
        this.type = type;
        this.sentToPrintAt = sentToPrintAt;
    }

    public UUID getId() {
        return id;
    }

    public String getMessageId() {
        return messageId;
    }

    public String getService() {
        return service;
    }

    public String getType() {
        return type;
    }

    public Date getSentToPrintAt() {
        return sentToPrintAt;
    }
}
