package uk.gov.hmcts.reform.sendletter.model.out;

import java.util.List;

public class LettersInfoResponse {

    public final int count;
    public final List<LetterStatus> letters;

    public LettersInfoResponse(List<LetterStatus> letters) {
        this.letters = letters;
        this.count = letters.size();
    }
}
