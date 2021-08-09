package uk.gov.hmcts.reform.sendletter.services.util;

import com.microsoft.applicationinsights.core.dependencies.apachecommons.io.FilenameUtils;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.exception.UnableToExtractIdFromFileNameException;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

import static java.time.format.DateTimeFormatter.ofPattern;

public final class FileNameHelper {

    public static final DateTimeFormatter dateTimeFormatter = ofPattern("ddMMyyyyHHmmss");
    private static final String SEPARATOR = "_";

    public static String generatePdfName(Letter letter) {
        return generatePdfName(letter.getType(), letter.getService(), letter.getId());
    }

    public static String generatePdfName(String type, String serviceName, UUID id) {
        String strippedService = serviceName.replace(SEPARATOR, "");
        return type + SEPARATOR + strippedService + SEPARATOR + id + ".pdf";
    }

    public static UUID extractIdFromPdfName(String fileName) {
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

    public static String generateName(Letter letter) {
        return generateName(
            letter.getType(),
            letter.getService(),
            letter.getCreatedAt(),
            letter.getId(),
            letter.isEncrypted()
        );
    }

    public static String generateName(
        String type,
        String service,
        LocalDateTime createdAtDateTime,
        UUID id,
        Boolean isEncrypted
    ) {
        return String.format(
            "%s_%s_%s_%s.%s",
            type.replace("_", ""),
            service.replace("_", ""),
            createdAtDateTime.format(dateTimeFormatter),
            id,
            isEncrypted ? "pgp" : "zip"
        );
    }

    private FileNameHelper() {
    }

}
