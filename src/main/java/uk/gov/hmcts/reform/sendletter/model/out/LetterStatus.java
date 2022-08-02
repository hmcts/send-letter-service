package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class LetterStatus {

    public final UUID id;

    public final String status;

    @Schema(description = "This field is deprecated, please use `checksum` instead")
    @JsonProperty("message_id")
    public final String messageId;

    @JsonProperty("checksum")
    public final String checksum;

    @JsonProperty("created_at")
    public final ZonedDateTime createdAt;

    @JsonProperty("sent_to_print_at")
    public final ZonedDateTime sentToPrintAt;

    @JsonProperty("printed_at")
    public final ZonedDateTime printedAt;

    @Schema(description = "Additional information about the letter")
    @JsonProperty("additional_data")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public final Map<String, Object> additionalData;

    @JsonProperty("copies")
    public final Integer copies;

    @JsonProperty("events")
    public final List<LetterStatusEvent> events;

    public LetterStatus(
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
        this.id = id;
        this.status = status;
        this.checksum = checksum;
        this.messageId = checksum;
        this.createdAt = createdAt;
        this.sentToPrintAt = sentToPrintAt;
        this.printedAt = printedAt;
        this.additionalData = additionalData;
        this.copies =  copies;
        this.events =  events;
    }
}
