package uk.gov.hmcts.reform.sendletter.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import java.time.Instant;

/**
 * Letter event entity.
 */
@Entity
@Table(name = "letter_events")
public class LetterEvent {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @ManyToOne
    @JoinColumn(name = "letter_id")
    private Letter letter;

    @Enumerated(EnumType.STRING)
    private EventType type;

    private String notes;

    private Instant createdAt;

    // For use by hibernate.
    private LetterEvent() {
    }

    /**
     * Constructor for the LetterEvent.
     * @param letter The letter
     * @param type The type of the event
     * @param notes The notes of the event
     * @param createdAt The creation date of the event
     */
    public LetterEvent(
        Letter letter,
        EventType type,
        String notes,
        Instant createdAt
    ) {
        this.letter = letter;
        this.type = type;
        this.notes = notes;
        this.createdAt = createdAt;
    }

    public long getId() {
        return id;
    }

    public Letter getLetter() {
        return letter;
    }

    public EventType getType() {
        return type;
    }

    public String getNotes() {
        return notes;
    }

    public Instant getCreatedAt() {
        return createdAt;
    }
}
