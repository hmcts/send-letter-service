package uk.gov.hmcts.reform.sendletter.entity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "documents")
public class Document {
    @Id
    private UUID id;
    private UUID letterId;
    private String checksum;
    private LocalDateTime createdAt;

    // For use by hibernate.
    private Document() {
    }

    public Document(
        UUID id,
        UUID letterId,
        String checksum,
        LocalDateTime createdAt
    ) {
        this.id = id;
        this.letterId = letterId;
        this.checksum = checksum;
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

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
}
