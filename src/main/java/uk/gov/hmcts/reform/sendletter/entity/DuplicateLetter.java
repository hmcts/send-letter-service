package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.databind.JsonNode;
import io.hypersistence.utils.hibernate.type.json.JsonType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.Type;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "duplicates")
public class DuplicateLetter {
    @Id
    private UUID id;
    private String checksum;
    private String service;
    // The following Type annotation instructs jpa to JSON serialize this field.
    // The column annotation instructs jpa that this field is stored as a json column
    // in our database and should be addressed with ::json in SQL fragments.
    @Type(JsonType.class)
    @Column(columnDefinition = "json")
    private JsonNode additionalData;
    private LocalDateTime createdAt;
    private String type;
    @Type(JsonType.class)
    @Column(columnDefinition = "json")
    private JsonNode copies;
    private String isAsync;

    // For use by hibernate.
    protected DuplicateLetter() {
    }

    /**
     * Constructor for the DuplicateLetter.
     * @param id The ID
     * @param checksum The checksum
     * @param service The service
     * @param additionalData The additional data
     * @param type The type
     * @param createdAt The created at
     * @param copies The copies
     * @param isAsync The is async
     */
    public DuplicateLetter(
        UUID id,
        String checksum,
        String service,
        JsonNode additionalData,
        String type,
        LocalDateTime createdAt,
        JsonNode copies,
        String isAsync
    ) {
        this.id = id;
        this.checksum = checksum;
        this.service = service;
        this.additionalData = additionalData;
        this.createdAt = createdAt;
        this.type = type;
        this.copies = copies;
        this.isAsync = isAsync;
    }



    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getChecksum() {
        return checksum;
    }

    public void setChecksum(String checksum) {
        this.checksum = checksum;
    }

    public String getService() {
        return service;
    }

    public void setService(String service) {
        this.service = service;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public JsonNode getAdditionalData() {
        return additionalData;
    }

    public void setAdditionalData(JsonNode additionalData) {
        this.additionalData = additionalData;
    }

    public JsonNode getCopies() {
        return copies;
    }

    public void setCopies(JsonNode copies) {
        this.copies = copies;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getIsAsync() {
        return isAsync;
    }

    public void setIsAsync(String isAsync) {
        this.isAsync = isAsync;
    }
}
