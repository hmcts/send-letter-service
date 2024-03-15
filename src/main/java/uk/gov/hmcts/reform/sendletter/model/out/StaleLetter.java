package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * This class represents the stale letter.
 */
public class StaleLetter {

    public final UUID id;

    public final String status;

    public final String service;

    @JsonProperty("created_at")
    public final LocalDateTime createdAt;

    @JsonProperty("sent_to_print_at")
    public final LocalDateTime sentToPrintAt;

    /**
     * Constructor.
     *
     * @param id the id
     * @param status the status
     * @param service the service
     * @param createdAt the created at
     * @param sentToPrintAt the sent to print at
     */
    public StaleLetter(
        UUID id,
        String status,
        String service,
        LocalDateTime createdAt,
        LocalDateTime sentToPrintAt
    ) {
        this.id = id;
        this.status = status;
        this.service = service;
        this.createdAt = createdAt;
        this.sentToPrintAt = sentToPrintAt;
    }
}
