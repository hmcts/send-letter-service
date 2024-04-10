package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.databind.JsonNode;
import io.hypersistence.utils.hibernate.type.json.JsonType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.Type;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * The type Print.
 */
@Entity
@Table(name = "prints")
public class Print {
    @Id
    private UUID id;
    private String service;
    private LocalDateTime createdAt;
    private LocalDateTime sentToPrintAt;
    private LocalDateTime printedAt;
    private boolean isFailed;
    private String type;
    @Enumerated(EnumType.STRING)
    private PrintStatus status = PrintStatus.NEW;
    private String idempotencyKey;
    // The following Type annotation instructs jpa to JSON serialize this field.
    // The column annotation instructs jpa that this field is stored as a json column
    // in our database and should be addressed with ::json in SQL fragments.
    @Type(JsonType.class)
    @Column(columnDefinition = "json")
    private JsonNode documents;
    private String caseId;
    private String caseRef;
    private String letterType;

    private Print() {
    }

    /**
     * Instantiates a new Print.
     *
     * @param id             the id
     * @param service        the service
     * @param createdAt      the created at
     * @param type           the type
     * @param idempotencyKey the idempotency key
     * @param documents      the documents
     * @param caseId         the case id
     * @param caseRef        the case ref
     * @param letterType     the letter type
     */
    @SuppressWarnings("squid:S00107")
    public Print(
        UUID id,
        String service,
        LocalDateTime createdAt,
        String type,
        String idempotencyKey,
        JsonNode documents,
        String caseId,
        String caseRef,
        String letterType
    ) {
        this.id = id;
        this.service = service;
        this.createdAt = createdAt;
        this.type = type;
        this.idempotencyKey = idempotencyKey;
        this.documents = documents;
        this.caseId = caseId;
        this.caseRef = caseRef;
        this.letterType = letterType;
    }

    public UUID getId() {
        return id;
    }

    public String getService() {
        return service;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getSentToPrintAt() {
        return sentToPrintAt;
    }

    public void setSentToPrintAt(LocalDateTime sentToPrintAt) {
        this.sentToPrintAt = sentToPrintAt;
    }

    public LocalDateTime getPrintedAt() {
        return printedAt;
    }

    public void setPrintedAt(LocalDateTime printedAt) {
        this.printedAt = printedAt;
    }

    public boolean isFailed() {
        return isFailed;
    }

    public void setFailed(boolean failed) {
        isFailed = failed;
    }

    public String getType() {
        return type;
    }

    public PrintStatus getStatus() {
        return status;
    }

    public void setStatus(PrintStatus status) {
        this.status = status;
    }

    public String getIdempotencyKey() {
        return idempotencyKey;
    }

    public JsonNode getDocuments() {
        return documents;
    }

    public String getCaseId() {
        return caseId;
    }

    public String getCaseRef() {
        return caseRef;
    }

    public String getLetterType() {
        return letterType;
    }
}
