package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Represents an extended letter status.
 */
public class ExtendedLetterStatus extends LetterStatus {
    @JsonProperty("events")
    public final List<LetterStatusEvent> events;

    /**
     * Constructor.
     *
     * @param id the id
     * @param status the status
     * @param checksum the checksum
     * @param createdAt the created at
     * @param sentToPrintAt the sent to print at
     * @param printedAt the printed at
     * @param additionalData the additional data
     * @param copies the copies
     * @param events the events
     */
    public ExtendedLetterStatus(
        final UUID id,
        final String status,
        final String checksum,
        final ZonedDateTime createdAt,
        final ZonedDateTime sentToPrintAt,
        final ZonedDateTime printedAt,
        final Map<String, Object> additionalData,
        final Integer copies,
        final List<LetterStatusEvent> events
    ) {
        super(id, status, checksum, createdAt, sentToPrintAt, printedAt, additionalData, copies);
        this.events =  events;
    }
}
