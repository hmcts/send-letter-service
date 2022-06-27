package uk.gov.hmcts.reform.sendletter.exception;

import java.util.UUID;

public class LetterNotStaleException extends RuntimeException {

    public LetterNotStaleException(String letterId, Throwable cause) {
        super("Letter with ID '" + letterId + "' is not stale", cause);
    }

    public LetterNotStaleException(UUID id) {
        this(id.toString(), null);
    }
}
