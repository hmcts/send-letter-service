package uk.gov.hmcts.reform.sendletter;

import uk.gov.hmcts.reform.sendletter.data.TestDataFactory;
import uk.gov.hmcts.reform.sendletter.model.in.Letter;

import java.util.UUID;

public final class SampleData {

    public static Letter letter() {
        return TestDataFactory.getTestLetterRequest();
    }

    public static String uuid() {
        return UUID.randomUUID().toString();
    }

    private SampleData() {
    }
}
