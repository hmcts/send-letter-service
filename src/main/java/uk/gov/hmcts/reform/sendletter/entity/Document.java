package uk.gov.hmcts.reform.sendletter.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "documents")
public class Document {
    @Id
    private UUID id;
    private UUID letterId;
    private String checksum;
    private String recipientsChecksum;
    private LocalDateTime createdAt;

    // For use by hibernate.
    private Document() {
    }

    public Document(
        UUID id,
        UUID letterId,
        String checksum,
        String recipientsChecksum,
        LocalDateTime createdAt
    ) {
        this.id = id;
        this.letterId = letterId;
        this.checksum = checksum;
        this.recipientsChecksum = recipientsChecksum;
        this.createdAt = createdAt;
    }

    public UUID getId() {
        return id;
    }

    public UUID getLetterId() {
        return letterId;
    }

    public String getChecksum() {
        return checksum;
    }

    public String getRecipientsChecksum() {
        return recipientsChecksum;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
}
