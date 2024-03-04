package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;

import java.io.Serializable;

public class Doc implements Serializable {

    private static final long serialVersionUID = -1718267310344700595L;

    @NotEmpty
    public final byte[] content;

    @Min(1)
    @Max(100)
    public final int copies;

    public Doc(
        @JsonProperty("content") byte[] content,
        @JsonProperty("copies") int copies
    ) {
        this.content = content;
        this.copies = copies;
    }
}
