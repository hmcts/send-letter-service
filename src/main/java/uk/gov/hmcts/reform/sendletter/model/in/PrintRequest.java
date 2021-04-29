package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.util.List;
import java.util.Map;
import javax.validation.Valid;

public class PrintRequest {
    @JsonProperty
    public final List<@Valid Document> documents;

    @JsonProperty("additional_data")
    public final Map<String, String> additionalData;

    private PrintRequest() {
        additionalData = null;
        documents = null;
    }

    public PrintRequest(List<Document> documents, Map<String, String> additionalData) {
        this.documents = documents;
        this.additionalData = additionalData;
    }

}
