package uk.gov.hmcts.reform.slc.services.steps.getpdf;

import com.microsoft.applicationinsights.core.dependencies.apachecommons.io.FilenameUtils;
import uk.gov.hmcts.reform.sendletter.entity.Letter;

import java.util.UUID;

public final class FileNameHelper {

    private static final String SEPARATOR = "_";

    public static String generateName(Letter letter, String extension) {
        return letter.getType() + SEPARATOR + letter.getService() + SEPARATOR + letter.getId() + "." + extension;
    }

    public static UUID extractId(String fileName) {
        String[] parts = FilenameUtils.removeExtension(fileName).split(SEPARATOR);
        if (parts.length != 3) {
            throw new UnableToExtractIdFromFileNameException();
        } else {
            return UUID.fromString(parts[2]);
        }
    }

    private FileNameHelper() {
    }

    public static class UnableToExtractIdFromFileNameException extends RuntimeException {

    }
}
