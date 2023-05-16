package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class ExtendedLetterStatus extends LetterStatus {
    @JsonProperty("events")
    public final List<LetterStatusEvent> events;

    public ExtendedLetterStatus(
        final UUID id,
        final String status,
        final String checksum,
        final ZonedDateTime createdAt,
        final ZonedDateTime sentToPrintAt,
        final ZonedDateTime printedAt,
        final Map<String, Object> additionalData,
        final Map<String, Integer> copies,
        final List<LetterStatusEvent> events
    ) {
        super(id, status, checksum, createdAt, sentToPrintAt, printedAt, additionalData, copies);
        this.events =  events;
    }
}
