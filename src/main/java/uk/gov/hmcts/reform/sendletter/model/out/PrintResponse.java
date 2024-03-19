package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;

/**
 * This class represents the print response.
 */
public class PrintResponse implements Serializable {
    @JsonProperty("print_job")
    public final PrintJob printJob;

    @JsonProperty("upload")
    public final PrintUploadInfo printUploadInfo;

    /**
     * Constructor.
     */
    private PrintResponse() {
        printJob = null;
        printUploadInfo = null;
    }

    /**
     * Constructor.
     *
     * @param printJob the print job
     * @param printUploadInfo the print upload info
     */
    public PrintResponse(PrintJob printJob,
                         PrintUploadInfo printUploadInfo) {
        this.printJob = printJob;
        this.printUploadInfo = printUploadInfo;
    }
}
