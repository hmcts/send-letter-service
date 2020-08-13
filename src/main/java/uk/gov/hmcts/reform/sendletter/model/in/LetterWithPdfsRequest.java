package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

public class LetterWithPdfsRequest implements Serializable, ILetterRequest {

    private static final long serialVersionUID = 5528476697055795883L;

    // TODO: validate pdfs are base64 encoded.
    /**
     * Base64 encoded pdfs.
     */
    @ApiModelProperty(value = "List of base64 encoded pdfs to be printed. Maximum allowed is 30", required = true)
    @Size(min = 1, max = 30)
    public final List<byte[]> documents;

    @ApiModelProperty(value = "Type to be used to print documents", required = true)
    @NotEmpty
    public final String type;

    @ApiModelProperty(value = "This field is used by BSP/Service team if there are any issues"
            + " with the letter so that service team can track it using case reference/claim id "
            + "and then can handle the case accordingly")
    @JsonProperty("additional_data")
    public final Map<String, Object> additionalData;

    public LetterWithPdfsRequest(
        @JsonProperty("documents") List<byte[]> documents,
        @JsonProperty("type") String type,
        @JsonProperty("additional_data") Map<String, Object> additionalData
    ) {
        this.documents = documents;
        this.type = type;
        this.additionalData = additionalData;
    }

    @Override
    public String getType() {
        return this.type;
    }

    @Override
    public Map<String, Object> getAdditionalData() {
        return this.additionalData;
    }
}
