package uk.gov.hmcts.reform.sendletter.entity;

import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

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
