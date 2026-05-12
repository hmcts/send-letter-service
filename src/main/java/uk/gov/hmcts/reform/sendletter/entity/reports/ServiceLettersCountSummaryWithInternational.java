package uk.gov.hmcts.reform.sendletter.entity.reports;

/**
 * Service letters count with international flag.
 */
public interface ServiceLettersCountSummaryWithInternational extends ServiceLettersCountSummary {

    /**
     * Get the international flag.
     * @return The international flag
     */
    boolean isInternational();
}
