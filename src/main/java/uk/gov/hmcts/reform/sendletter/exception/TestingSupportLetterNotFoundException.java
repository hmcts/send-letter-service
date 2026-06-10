package uk.gov.hmcts.reform.sendletter.exception;

import java.util.UUID;

public class TestingSupportLetterNotFoundException extends RuntimeException {

    public TestingSupportLetterNotFoundException(String letterId, Throwable cause) {
        super("Letter with ID '" + letterId + "' not found", cause);
    }

    public TestingSupportLetterNotFoundException(UUID letterId) {
        super("Letter with ID '" + letterId + "' not found");
    }
}
