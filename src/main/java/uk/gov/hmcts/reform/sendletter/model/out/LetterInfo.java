package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.UUID;

public class LetterInfo {

    @JsonProperty("id")
    public final UUID id;

    @JsonProperty("service")
    public final String service;

    @JsonProperty("status")
    public final String status;

    @JsonProperty("created_at")
    public final LocalDateTime createdAt;

    @JsonProperty("sent_to_print_at")
    public final LocalDateTime sentToPrintAt;

    @JsonProperty("printed_at")
    public final LocalDateTime printedAt;

    public LetterInfo(
        UUID id,
        String service,
        String status,
        LocalDateTime createdAt,
        LocalDateTime sentToPrintAt,
        LocalDateTime printedAt
    ) {
        this.id = id;
        this.service = service;
        this.status = status;
        this.createdAt = createdAt;
        this.sentToPrintAt = sentToPrintAt;
        this.printedAt = printedAt;
    }
}
