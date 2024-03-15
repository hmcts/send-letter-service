package uk.gov.hmcts.reform.sendletter.model.out;

public class LettersCountSummary {

    public final String serviceName;
    public final int uploaded;

    /**
     * Constructor.
     *
     * @param serviceName the service name
     * @param uploaded the uploaded
     */
    public LettersCountSummary(String serviceName, int uploaded) {
        this.serviceName = serviceName;
        this.uploaded = uploaded;
    }
}
