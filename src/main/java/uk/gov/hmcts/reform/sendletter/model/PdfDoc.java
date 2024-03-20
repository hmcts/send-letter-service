package uk.gov.hmcts.reform.sendletter.model;

public class PdfDoc {

    public final String filename;
    public final byte[] content;

    /**
     * Constructor.
     *
     * @param filename the filename
     * @param content the content
     */
    public PdfDoc(String filename, byte[] content) {
        this.filename = filename;
        this.content = content;
    }
}
