package uk.gov.hmcts.reform.sendletter.services.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.applicationinsights.core.dependencies.apachecommons.io.FilenameUtils;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.exception.UnableToExtractIdFromFileNameException;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.UUID;

import static java.time.format.DateTimeFormatter.ofPattern;

/**
 * Utility class for generating file names.
 */
public final class FileNameHelper {

    public static final DateTimeFormatter dateTimeFormatter = ofPattern("ddMMyyyyHHmmss");
    private static final String SEPARATOR = "_";
    private static final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Generates a PDF file name for a given letter.
     *
     * @param letter The letter
     * @return The PDF file name
     */
    public static String generatePdfName(Letter letter) {
        return generatePdfName(letter.getType(), letter.getService(), letter.getId(),
            objectMapper.convertValue(letter.getAdditionalData(), new TypeReference<>(){}));
    }

    /**
     * Generates a PDF file name for a given letter.
     * Creates the PDF file name for the letter based on letter information. It also
     * checks if the letter is related to infected blood and inserts the relevant
     * acronym into the name if so.
     * @param type       The letter type e.g. SSCS001
     * @param serviceName The service name - e.g. cmc
     * @param id         The letter ID - a UUID
     * @param additionalData more data on the letter e.g. whether it is international
     * @return The PDF file name
     */
    public static String generatePdfName(String type, String serviceName, UUID id, Map<String, Object> additionalData) {
        String strippedService = serviceName.replace(SEPARATOR, "");
        return type + infectedBloodInfix(serviceName, additionalData)
            + SEPARATOR + strippedService + SEPARATOR + id + ".pdf";
    }

    /**
     * Extracts the letter ID from a PDF file name.
     *
     * @param fileName The PDF file name
     * @return The letter ID
     */
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

    /**
     * Generates a file name for a given letter.
     *
     * @param letter The letter
     * @return The file name
     */
    public static String generateName(Letter letter) {
        return generateName(
            letter.getType(),
            letter.getService(),
            letter.getCreatedAt(),
            letter.getId(),
            letter.isEncrypted(),
            objectMapper.convertValue(letter.getAdditionalData(), new TypeReference<>(){})
        );
    }

    /**
     * Generates a file name for a given letter.
     *
     * @param type             The letter type
     * @param service          The service name
     * @param createdAtDateTime The creation date and time
     * @param id               The letter ID
     * @param isEncrypted      Whether the letter is encrypted
     * @param additionalData    Any additional data about the letter e.g. whether it is international
     * @return The file name
     */
    public static String generateName(
        String type,
        String service,
        LocalDateTime createdAtDateTime,
        UUID id,
        Boolean isEncrypted,
        Map<String, Object> additionalData
    ) {
        return String.format(
            "%s%s_%s_%s_%s.%s",
            type.replace("_", ""),
            infectedBloodInfix(service, additionalData),
            service.replace("_", ""),
            createdAtDateTime.format(dateTimeFormatter),
            id,
            Boolean.TRUE.equals(isEncrypted) ? "pgp" : "zip"
        );
    }

    /**
     * Determines if the letter type is "sscs" and if isIbca in additionalInfo is true, and returns "_IB".
     *
     * @param service The letter type
     * @param additionalData The additional data
     * @return "_IB" if the type is "sscs" and if isIbca in additionalInfo infected blood param is true,
     *      otherwise an empty string.
     *
     */
    public static String infectedBloodInfix(String service, Map<String, Object> additionalData) {
        if ("sscs".equalsIgnoreCase(service) && additionalData != null && additionalData.containsKey("isIbca")) {

            Object isIbcaValue = additionalData.get("isIbca");
            if ("true".equalsIgnoreCase(String.valueOf(isIbcaValue))) {
                return SEPARATOR + "IB";
            }
        }
        return "";
    }


    private FileNameHelper() {
    }

}
