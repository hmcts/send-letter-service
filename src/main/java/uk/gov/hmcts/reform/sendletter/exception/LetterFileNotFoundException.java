package uk.gov.hmcts.reform.sendletter.exception;

import java.util.UUID;

public class LetterFileNotFoundException extends RuntimeException {

    public LetterFileNotFoundException(String filename) {
        super("File '" + filename + "' was not found on SFTP");
    }

    public LetterFileNotFoundException(UUID letterId, String filename) {
        super("Letter with ID '" + letterId + "' exists but file '" + filename + "' was not found on SFTP");
    }
}
