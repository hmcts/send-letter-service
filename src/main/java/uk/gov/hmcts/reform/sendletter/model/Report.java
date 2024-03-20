package uk.gov.hmcts.reform.sendletter.model;

public class Report {

    public final String path;
    public final byte[] content;

    /**
     * Constructor.
     *
     * @param path the path
     * @param content the content
     */
    public Report(String path, byte[] content) {
        this.path = path;
        this.content = content;
    }
}
