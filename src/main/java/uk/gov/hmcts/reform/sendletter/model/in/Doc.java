package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Doc {

    public byte[] content;
    public int copies;

    public Doc(
        @JsonProperty("content") byte[] content,
        @JsonProperty("copies") int copies
    ) {
        this.content = content;
        this.copies = copies;
    }
}
