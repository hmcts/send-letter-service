package uk.gov.hmcts.reform.sendletter.entity;

import java.util.Date;
import java.util.UUID;

public interface StaleLetter {

    UUID getId();

    String getMessageId();

    String getService();

    String getType();

    Date getSentToPrintAt();
}
