package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.io.Serializable;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

public class PrintRequest implements Serializable {

    private static final long serialVersionUID = 4312487677760800172L;
    
    @ApiModelProperty(value = "Type to be used to print documents", required = true)
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


    private PrintRequest() {
        type = null;
        documents = null;
        caseId = null;
        caseRef = null;
        letterType = null;
    }

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
