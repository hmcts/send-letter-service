package uk.gov.hmcts.reform.slc.services.steps.getpdf;

import com.microsoft.applicationinsights.core.dependencies.apachecommons.io.FilenameUtils;
import uk.gov.hmcts.reform.sendletter.entity.Letter;

import java.util.UUID;

public final class FileNameHelper {

    private static final String SEPARATOR = "_";

    public static String generateName(Letter letter, String extension) {
        return generateName(letter.getType(), letter.getService(), letter.getId(), extension);
    }

    public static String generateName(String type, String serviceName, UUID id, String extension) {
        serviceName = serviceName.replace(SEPARATOR, "");
        return type + SEPARATOR + serviceName + SEPARATOR + id + "." + extension;
    }

    public static UUID extractId(String fileName) {
        String[] parts = FilenameUtils.removeExtension(fileName).split(SEPARATOR);
        if (parts.length < 3) {
            throw new UnableToExtractIdFromFileNameException("Invalid filename " + fileName);
        } else {
            try {
                return UUID.fromString(parts[parts.length - 1]);
            } catch (IllegalArgumentException e) {
                throw new UnableToExtractIdFromFileNameException(e);
            }
        }
    }

    private FileNameHelper() {
    }

}
