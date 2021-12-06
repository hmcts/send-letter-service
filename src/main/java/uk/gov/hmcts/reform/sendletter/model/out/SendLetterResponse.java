package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import java.util.UUID;

public class SendLetterResponse {

    @Schema(
        name = "Letter Id",
        description = "Id of the letter sent to print"
    )
    @JsonProperty("letter_id")
    public final UUID letterId;

    public SendLetterResponse(UUID letterId) {
        this.letterId = letterId;
    }
}
