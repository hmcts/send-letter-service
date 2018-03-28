package uk.gov.hmcts.reform.sendletter.entity;

import java.sql.Timestamp;
import java.util.UUID;

public interface StaleLetter {

    UUID getId();

    String getMessageId();

    String getService();

    String getType();

    Timestamp getSentToPrintAt();
}
