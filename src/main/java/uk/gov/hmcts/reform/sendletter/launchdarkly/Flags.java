package uk.gov.hmcts.reform.sendletter.launchdarkly;

public final class Flags {
    private Flags() {

    }

    public static final String SEND_LETTER_SERVICE_TEST = "send-letter-service-test";
    public static final String SEND_LETTER_SERVICE_DELETE_LETTERS_CRON = "send-letter-service-delete-letters-cron";
}
