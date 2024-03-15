package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * This class represents the letter request.
 */
public class LetterRequest implements Serializable, ILetterRequest {

    private static final long serialVersionUID = -7737087336283080072L;

    @Schema(description = "List of documents to be printed. Maximum allowed is 30", required = true)
    @Size(min = 1, max = 30)
    @Valid
    public final List<Document> documents;

    @Schema(description = "Type to be used to print documents", required = true)
    @NotEmpty
    public final String type;

    @Schema(description = "Optional field where services can store any additional information about the letter")
    @JsonProperty("additional_data")
    @ValidRecipients
    public final Map<String, Object> additionalData;

    /**
     * Constructor.
     *
     * @param documents the list of documents
     * @param type the type
     * @param additionalData the additional data
     */
    public LetterRequest(
        @JsonProperty("documents") List<Document> documents,
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
