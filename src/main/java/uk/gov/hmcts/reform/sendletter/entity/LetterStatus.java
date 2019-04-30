package uk.gov.hmcts.reform.sendletter.entity;

/**
 * Represents possible letter statuses in the DB.
 * <p>
 * Note that in the repository these statuses are referenced by value, so any change to these names
 * must be reflected in those queries.
 * </p>
 */
public enum LetterStatus {
    Created,
    Uploaded,
    Posted,
    Aborted
}
