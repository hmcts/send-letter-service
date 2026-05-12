package uk.gov.hmcts.reform.sendletter.entity.reports;

import java.time.LocalDate;

public interface ServiceLettersReport {

    LocalDate getCreatedAt();

    String getService();

    boolean isInternational();
}
