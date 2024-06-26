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
 * Letter entity.
 */
@Entity
@Table(name = "letters")
public class Letter {
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
    private LocalDateTime sentToPrintAt;
    private LocalDateTime printedAt;
    private boolean isFailed;
    private String type;
    @Enumerated(EnumType.STRING)
    private LetterStatus status = LetterStatus.Created;
    private byte[] fileContent;
    private Boolean isEncrypted;
    private String encryptionKeyFingerprint;
    @Type(JsonType.class)
    @Column(columnDefinition = "json")
    private JsonNode copies;

    // For use by hibernate.
    private Letter() {
    }

    /**
     * Constructor for the Letter.
     * @param id The id of the letter
     * @param checksum The checksum of the letter
     * @param service The service of the letter
     * @param additionalData The additional data of the letter
     * @param type The type of the letter
     * @param fileContent The file content of the letter
     * @param isEncrypted The isEncrypted of the letter
     * @param encryptionKeyFingerprint The encryptionKeyFingerprint of the letter
     * @param createdAt The created at of the letter
     * @param copies The copies of the letter
     */
    public Letter(
        UUID id,
        String checksum,
        String service,
        JsonNode additionalData,
        String type,
        byte[] fileContent,
        Boolean isEncrypted,
        String encryptionKeyFingerprint,
        LocalDateTime createdAt,
        JsonNode copies
    ) {
        this.id = id;
        this.checksum = checksum;
        this.service = service;
        this.additionalData = additionalData;
        this.type = type;
        this.fileContent = fileContent;
        this.isFailed = false;
        this.isEncrypted = isEncrypted;
        this.encryptionKeyFingerprint = encryptionKeyFingerprint;
        this.createdAt = createdAt;
        this.copies = copies;
    }

    public UUID getId() {
        return id;
    }

    public String getChecksum() {
        return checksum;
    }

    public String getService() {
        return service;
    }

    public String getType() {
        return type;
    }

    public LetterStatus getStatus() {
        return status;
    }

    public void setStatus(LetterStatus status) {
        this.status = status;
    }

    public byte[] getFileContent() {
        return fileContent;
    }

    public void setFileContent(byte[] fileContent) {
        this.fileContent = fileContent;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public LocalDateTime getSentToPrintAt() {
        return sentToPrintAt;
    }

    public void setSentToPrintAt(LocalDateTime value) {
        this.sentToPrintAt = value;
    }

    public LocalDateTime getPrintedAt() {
        return printedAt;
    }

    public void setPrintedAt(LocalDateTime value) {
        this.printedAt = value;
    }

    public boolean isFailed() {
        return isFailed;
    }

    public JsonNode getAdditionalData() {
        return additionalData;
    }

    public Boolean isEncrypted() {
        return isEncrypted;
    }

    public String getEncryptionKeyFingerprint() {
        return encryptionKeyFingerprint;
    }

    public void setEncryptionKeyFingerprint(String encryptionKeyFingerprint) {
        this.encryptionKeyFingerprint = encryptionKeyFingerprint;
    }

    public JsonNode getCopies() {
        return copies;
    }

    public void setCopies(JsonNode copies) {
        this.copies = copies;
    }
}
