package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

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
    @Schema(description = "List of base64 encoded pdfs to be printed. Maximum allowed is 30", required = true)
    @Size(min = 1, max = 30)
    public final List<byte[]> documents;

    @Schema(description = "Type to be used to print documents", required = true)
    @NotEmpty
    public final String type;

    @Schema(description = "Optional field where services can store any additional information about the letter")
    @JsonProperty("additional_data")
    @ValidRecipients
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
