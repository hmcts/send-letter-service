package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;

/**
 * This class represents the print upload info.
 */
public class PrintUploadInfo implements Serializable {
    @JsonProperty("upload_to_container")
    public final String uploadToContainer;

    @JsonProperty("sas")
    public final String sasToken;

    @JsonProperty("manifest_path")
    public final String manifestPath;

    /**
     * Constructor.
     */
    private PrintUploadInfo() {
        uploadToContainer = null;
        sasToken = null;
        manifestPath = null;
    }

    /**
     * Constructor.
     *
     * @param uploadToContainer the upload to container
     * @param sasToken the sas token
     * @param manifestPath the manifest path
     */
    public PrintUploadInfo(String uploadToContainer,
                           String sasToken,
                           String manifestPath) {
        this.uploadToContainer = uploadToContainer;
        this.sasToken = sasToken;
        this.manifestPath = manifestPath;
    }
}
