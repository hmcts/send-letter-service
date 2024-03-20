package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.Valid;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.io.Serializable;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

/**
 * This class represents the print job.
 */
public class PrintJob implements Serializable {
    private static final long serialVersionUID = 3711622159588476678L;

    public final UUID id;

    @JsonProperty("created_at")
    public final ZonedDateTime createdAt;

    @JsonProperty("printed_at")
    public final ZonedDateTime printedAt;

    @JsonProperty("sent_to_print_at")
    public final ZonedDateTime sentToPrintAt;

    public final String service;

    public final String type;

    @JsonProperty("container_name")
    public final String containerName;

    @JsonProperty("status")
    public final PrintStatus printStatus;

    @SuppressWarnings("squid:S1948")
    @Valid
    public final List<Document> documents;

    @JsonProperty("case_id")
    public final String caseId;

    @JsonProperty("case_ref")
    public final String caseRef;

    @JsonProperty("letter_type")
    public final String letterType;

    /**
     * Constructor.
     */
    private PrintJob() {
        id = null;
        createdAt = null;
        printedAt = null;
        sentToPrintAt = null;
        service = null;
        type = null;
        containerName = null;
        printStatus = null;
        documents = null;
        caseId = null;
        caseRef = null;
        letterType = null;
    }

    /**
     * Constructor.
     *
     * @param id the id
     * @param createdAt the created at
     * @param printedAt the printed at
     * @param sentToPrintAt the sent to print at
     * @param service the service
     * @param type the type
     * @param containerName the container name
     * @param printStatus the print status
     * @param documents the documents
     * @param caseId the case id
     * @param caseRef the case ref
     * @param letterType the letter type
     */
    @SuppressWarnings("squid:S00107")
    public PrintJob(UUID id,
                    ZonedDateTime createdAt,
                    ZonedDateTime printedAt,
                    ZonedDateTime sentToPrintAt,
                    String service,
                    String type,
                    String containerName, PrintStatus printStatus,
                    List<@Valid Document> documents,
                    String caseId,
                    String caseRef,
                    String letterType) {
        this.id = id;
        this.createdAt = createdAt;
        this.printedAt = printedAt;
        this.sentToPrintAt = sentToPrintAt;
        this.service = service;
        this.type = type;
        this.containerName = containerName;
        this.printStatus = printStatus;
        this.documents = documents;
        this.caseId = caseId;
        this.caseRef = caseRef;
        this.letterType = letterType;
    }
}
