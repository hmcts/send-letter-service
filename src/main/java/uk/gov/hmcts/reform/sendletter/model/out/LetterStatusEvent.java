package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.ZonedDateTime;

public class LetterStatusEvent {

    @JsonProperty("type")
    public final String type;

    @JsonProperty("notes")
    public final String notes;

    @JsonProperty("created_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", timezone = "UTC")
    public final ZonedDateTime createdAt;

    /**
     * Constructor.
     *
     * @param type the type
     * @param notes the notes
     * @param createdAt the created at
     */
    public LetterStatusEvent(String type, String notes, ZonedDateTime createdAt) {
        this.type = type;
        this.notes = notes;
        this.createdAt = createdAt;
    }
}
