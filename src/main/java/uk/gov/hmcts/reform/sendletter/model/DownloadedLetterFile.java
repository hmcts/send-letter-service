package uk.gov.hmcts.reform.sendletter.model;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * A downloaded letter file.
 */
@Getter
@AllArgsConstructor
public class DownloadedLetterFile {

    private final String filename;
    private final byte[] content;
}
