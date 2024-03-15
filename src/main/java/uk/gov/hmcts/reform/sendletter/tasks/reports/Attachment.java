package uk.gov.hmcts.reform.sendletter.tasks.reports;

import java.io.File;

/**
 * Represents an attachment to be included in the report.
 */
class Attachment {

    final String filename;
    final File file;

    /**
     * Creates a new attachment.
     *
     * @param filename the name of the file
     * @param file the file
     */
    Attachment(String filename, File file) {
        this.filename = filename;
        this.file = file;
    }
}
