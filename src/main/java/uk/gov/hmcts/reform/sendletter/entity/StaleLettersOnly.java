package uk.gov.hmcts.reform.sendletter.entity;

import java.util.Date;
import java.util.UUID;

public interface StaleLettersOnly {

    UUID getId();

    String getMessageId();

    String getService();

    String getType();

    Date getSentToPrintAt();
}
