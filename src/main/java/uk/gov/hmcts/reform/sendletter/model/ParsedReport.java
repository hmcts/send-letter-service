package uk.gov.hmcts.reform.sendletter.model;

import java.util.List;

public class ParsedReport {

    public final String path;
    public final List<LetterPrintStatus> statuses;
    public final boolean allRowsParsed;

    /**
     * Constructor.
     *
     * @param path the path
     * @param statuses the statuses
     * @param allRowsParsed the all rows parsed
     */
    public ParsedReport(String path, List<LetterPrintStatus> statuses, boolean allRowsParsed) {
        this.path = path;
        this.statuses = statuses;
        this.allRowsParsed = allRowsParsed;
    }
}
