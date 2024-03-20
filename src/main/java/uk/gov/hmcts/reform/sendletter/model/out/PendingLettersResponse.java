package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * This class represents the pending letters response.
 */
public class PendingLettersResponse {

    @JsonProperty("pending_letters")
    public final List<PendingLetter> pendingLetters;

    public PendingLettersResponse(List<PendingLetter> pendingLetters) {
        this.pendingLetters = pendingLetters;
    }
}
