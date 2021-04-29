package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.validation.Valid;

public class PrintJob {
    public final UUID id;

    @JsonProperty("created_at")
    public final LocalDateTime createdAt;

    @JsonProperty("printed_at")
    public final LocalDateTime printedAt;

    @JsonProperty("sent_to_print_at")
    public final LocalDateTime sentToPrintAt;

    public final String service;

    @JsonProperty("status")
    public final PrintStatus printStatus;

    public final List<@Valid Document> documents;

    @JsonProperty("additional_data")
    public final Map<String, String> additionalData;


    private PrintJob() {
        id = null;
        createdAt = null;
        printedAt = null;
        sentToPrintAt = null;
        service = null;
        printStatus = null;
        documents = null;
        additionalData = null;
    }

    @SuppressWarnings("squid:S00107")
    public PrintJob(UUID id,
                    LocalDateTime createdAt,
                    LocalDateTime printedAt,
                    LocalDateTime sentToPrintAt,
                    String service,
                    PrintStatus printStatus,
                    List<@Valid Document> documents,
                    Map<String, String> additionalData) {
        this.id = id;
        this.createdAt = createdAt;
        this.printedAt = printedAt;
        this.sentToPrintAt = sentToPrintAt;
        this.service = service;
        this.printStatus = printStatus;
        this.documents = documents;
        this.additionalData = additionalData;
    }
}
