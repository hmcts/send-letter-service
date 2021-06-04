package uk.gov.hmcts.reform.sendletter.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;

import static com.fasterxml.jackson.annotation.JsonInclude.Include;

public class Document implements Serializable {
    private static final long serialVersionUID = -8034446485215946927L;

    @JsonProperty("file_name")
    @NotEmpty
    public final String fileName;

    @JsonProperty("upload_to_path")
    @JsonInclude(Include.NON_NULL)
    public final String uploadToPath;

    @JsonProperty("copies_required")
    @Min(1)
    public final Integer copies;

    private Document() {
        fileName = null;
        uploadToPath = null;
        copies = null;
    }

    public Document(
        String fileName,
        String uploadToPath,
        Integer copies
    ) {
        this.fileName = fileName;
        this.uploadToPath = uploadToPath;
        this.copies = copies;
    }
}

