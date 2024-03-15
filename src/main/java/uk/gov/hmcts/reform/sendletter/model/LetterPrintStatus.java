package uk.gov.hmcts.reform.sendletter.model;

import java.time.ZonedDateTime;
import java.util.UUID;

public class LetterPrintStatus {

    public final UUID id;
    public final ZonedDateTime printedAt;

    /**
     * Constructor.
     *
     * @param id the id
     * @param printedAt the printed at
     */
    public LetterPrintStatus(UUID id, ZonedDateTime printedAt) {
        this.id = id;
        this.printedAt = printedAt;
    }
}
