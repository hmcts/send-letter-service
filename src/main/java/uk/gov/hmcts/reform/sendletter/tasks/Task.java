package uk.gov.hmcts.reform.sendletter.tasks;

public enum Task {
    UploadLetters,
    MarkLettersPosted,
    StaleLetters,
    Unknown;

    int getLockId() {
        return ordinal();
    }

    static Task getTaskFromLockId(int lockId) {
        for (Task value : values()) {
            if (value.getLockId() == lockId) {
                return value;
            }
        }

        return Unknown;
    }
}
