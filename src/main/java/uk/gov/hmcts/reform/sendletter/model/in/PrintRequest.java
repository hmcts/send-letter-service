package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.util.List;
import javax.validation.Valid;

public class PrintRequest {
    @JsonProperty
    public final String type;

    @JsonProperty
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
