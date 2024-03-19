package uk.gov.hmcts.reform.sendletter.entity.reports;

/**
 * Service letters count summary.
 */
public interface ServiceLettersCountSummary {

    /**
     * Get the service.
     * @return The service
     */
    String getService();

    /**
     * Get the uploaded.
     * @return The uploaded
     */
    int getUploaded();
}
