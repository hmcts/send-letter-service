package uk.gov.hmcts.reform.sendletter.entity.reports;

/**
 * Service letters count.
 */
public class ServiceLettersCount implements ServiceLettersCountSummary {

    private final String service;

    private final int uploaded;

    /**
     * Constructor.
     * @param service The service
     * @param uploaded The uploaded
     */
    public ServiceLettersCount(String service, int uploaded) {
        this.service = service;
        this.uploaded = uploaded;
    }

    /**
     * Get the service.
     * @return The service
     */
    @Override
    public String getService() {
        return service;
    }

    /**
     * Get the uploaded.
     * @return The uploaded
     */
    @Override
    public int getUploaded() {
        return uploaded;
    }
}
