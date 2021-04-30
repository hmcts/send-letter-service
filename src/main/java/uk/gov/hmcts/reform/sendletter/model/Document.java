package uk.gov.hmcts.reform.sendletter.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Document {
    @JsonProperty("file_name")
    public final String fileName;

    @JsonProperty("copies_required")
    public final Integer copies;

    private Document() {
        fileName = null;
        copies = null;
    }

    public Document(String fileName,
                    Integer copies) {
        this.fileName = fileName;
        this.copies = copies;
    }
}

