package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.io.Serializable;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * This class represents the print request.
 */
public class PrintRequest implements Serializable {

    private static final long serialVersionUID = 4312487677760800172L;

    @Schema(description = "Type to be used to print documents", required = true)
    @JsonProperty
    @NotEmpty
    public final String type;

    @JsonProperty
    @NotEmpty
    @Size(min = 1, max = 30)
    public final List<@Valid Document> documents;

    @JsonProperty("case_id")
    public final String caseId;

    @JsonProperty("case_ref")
    public final String caseRef;

    @JsonProperty("letter_type")
    public final String letterType;

    /**
     * Constructor.
     *
     * @param type the type
     * @param documents the list of documents
     * @param caseId the case id
     * @param caseRef the case reference
     * @param letterType the letter type
     */
    private PrintRequest() {
        type = null;
        documents = null;
        caseId = null;
        caseRef = null;
        letterType = null;
    }

    /**
     * Constructor.
     *
     * @param type the type
     * @param documents the list of documents
     * @param caseId the case id
     * @param caseRef the case reference
     * @param letterType the letter type
     */
    public PrintRequest(
        String type,
        List<Document> documents,
        String caseId,
        String caseRef,
        String letterType
    ) {
        this.type = type;
        this.documents = documents;
        this.caseId = caseId;
        this.caseRef = caseRef;
        this.letterType = letterType;
    }
}
