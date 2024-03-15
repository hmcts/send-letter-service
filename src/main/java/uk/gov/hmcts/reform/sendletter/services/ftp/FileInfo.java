package uk.gov.hmcts.reform.sendletter.services.ftp;

import java.time.Instant;

/**
 * This class represents the file info.
 */
public class FileInfo {

    public final String path;
    public final Instant modifiedAt;

    /**
     * Constructor.
     *
     * @param path the path
     * @param modifiedAt the modified at
     */
    public FileInfo(String path, Instant modifiedAt) {
        this.path = path;
        this.modifiedAt = modifiedAt;
    }
}
