package uk.gov.hmcts.reform.sendletter.exception;

import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.util.UUID;

public class UnsupportedLetterStatusException extends RuntimeException {

    public UnsupportedLetterStatusException(String letterId, LetterStatus status, Throwable cause) {
        super("Letter with ID '" + letterId + "', status '" + status + "' is not supported", cause);
    }

    public UnsupportedLetterStatusException(UUID id, LetterStatus status) {
        this(id.toString(), status, null);
    }
}
