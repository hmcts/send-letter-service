package uk.gov.hmcts.reform.slc.services.steps.zip;

import uk.gov.hmcts.reform.sendletter.entity.Letter;

import static java.time.LocalDateTime.now;
import static java.time.format.DateTimeFormatter.ofPattern;

public class ZipFileNameHelper {

    public static String generateName(Letter letter) {

        return String.format(
            "%s_%s_%s_%s.zip",
            letter.getType(),
            letter.getService().replace("_", ""),
            now().format(ofPattern("ddMMyyyyHHmmss")),
            letter.getId()
        );
    }

    private ZipFileNameHelper() {
        // utility class constructor
    }
}
