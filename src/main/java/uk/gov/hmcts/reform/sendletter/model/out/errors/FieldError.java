package uk.gov.hmcts.reform.sendletter.model.out.errors;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Represents a field error.
 */
public class FieldError {

    @JsonProperty("field_name")
    public final String fieldName;

    @JsonProperty("message")
    public final String message;

    /**
     * Constructor.
     *
     * @param fieldName the field name
     * @param message the message
     */
    public FieldError(String fieldName, String message) {
        this.fieldName = fieldName;
        this.message = message;
    }

    /**
     * Returns the field name.
     *
     * @return the field name
     */
    @Override
    public String toString() {
        return "FieldError{fieldName='" + fieldName + "', message='" + message + "'}";
    }
}
