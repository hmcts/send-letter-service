package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.ZonedDateTime;

public class LetterStatusEvent {

    @JsonProperty("type")
    public final String type;

    @JsonProperty("notes")
    public final String notes;

    @JsonProperty("created_at")
    public final ZonedDateTime createdAt;

    public LetterStatusEvent(String type, String notes, ZonedDateTime createdAt) {
        this.type = type;
        this.notes = notes;
        this.createdAt = createdAt;
    }
}
