package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.databind.JsonNode;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "letters")
@TypeDefs({
    @TypeDef(name = "json", typeClass = JsonBinaryType.class)
})
public class Letter {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private UUID id;

    private String messageId;
    private String service;
    // The following Type annotation instructs jpa to JSON serialize this field.
    // The column annotation instructs jpa that this field is stored as a json column
    // in our database and should be addressed with ::json in SQL fragments.
    @Type(type = "json")
    @Column(columnDefinition = "json")
    private JsonNode additionalData;
    private Timestamp createdAt = Timestamp.from(Instant.now());
    private Timestamp sentToPrintAt;
    private Timestamp printedAt;
    private boolean isFailed;
    private String type;
    @Enumerated(EnumType.STRING)
    private LetterState state = LetterState.Created;
    private byte[] pdf;

    // For use by hibernate.
    private Letter() {
    }

    public Letter(
        String messageId,
        String service,
        JsonNode additionalData,
        String type,
        byte[] pdf
    ) {
        this.messageId = messageId;
        this.service = service;
        this.additionalData = additionalData;
        this.type = type;
        this.pdf = pdf;
        this.isFailed = false;
    }

    public UUID getId() {
        return id;
    }

    public String getMessageId() {
        return messageId;
    }

    public String getService() {
        return service;
    }

    public String getType() {
        return type;
    }

    public LetterState getState() {
        return state;
    }

    public void setState(LetterState state) {
        this.state = state;
    }

    public byte[] getPdf() {
        return pdf;
    }

    public Timestamp getCreatedAt() {
        return createdAt;
    }

    public Timestamp getSentToPrintAt() {
        return sentToPrintAt;
    }

    public void setSentToPrintAt(Timestamp value) {
        this.sentToPrintAt = value;
    }

    public Timestamp getPrintedAt() {
        return printedAt;
    }

    public boolean isFailed() {
        return isFailed;
    }

    public JsonNode getAdditionalData() {
        return additionalData;
    }
}
