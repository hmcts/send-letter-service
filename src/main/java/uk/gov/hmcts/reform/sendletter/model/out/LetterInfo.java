package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * Represents a letter info.
 */
public class LetterInfo {

    @JsonProperty("id")
    public final UUID id;

    @JsonProperty("service")
    public final String service;

    @JsonProperty("type")
    public final String type;

    @JsonProperty("status")
    public final String status;

    @JsonProperty("created_at")
    public final LocalDateTime createdAt;

    @JsonProperty("sent_to_print_at")
    public final LocalDateTime sentToPrintAt;

    @JsonProperty("printed_at")
    public final LocalDateTime printedAt;

    /**
     * Constructor.
     *
     * @param id the id
     * @param service the service
     * @param type the type
     * @param status the status
     * @param createdAt the created at
     * @param sentToPrintAt the sent to print at
     * @param printedAt the printed at
     */
    public LetterInfo(
        UUID id,
        String service,
        String type,
        String status,
        LocalDateTime createdAt,
        LocalDateTime sentToPrintAt,
        LocalDateTime printedAt
    ) {
        this.id = id;
        this.service = service;
        this.type = type;
        this.status = status;
        this.createdAt = createdAt;
        this.sentToPrintAt = sentToPrintAt;
        this.printedAt = printedAt;
    }
}
