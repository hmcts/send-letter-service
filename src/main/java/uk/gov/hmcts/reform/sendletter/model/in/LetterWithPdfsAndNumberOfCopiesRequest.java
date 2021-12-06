package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

public class LetterWithPdfsAndNumberOfCopiesRequest implements Serializable, ILetterRequest {

    private static final long serialVersionUID = 1471737409853792378L;

    @Valid
    @Size(min = 1, max = 30)
    public final List<Doc> documents;

    @Schema(description = "Type to be used to print documents", required = true)
    @NotEmpty
    public final String type;

    @Schema(description = "Optional field where services can store any additional information about the letter")
    public final Map<String, Object> additionalData;

    public LetterWithPdfsAndNumberOfCopiesRequest(
        @JsonProperty("documents") List<Doc> documents,
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
