package uk.gov.hmcts.reform.sendletter.model.out.errors;

import java.util.List;

/**
 * Represents a model validation error.
 */
public class ModelValidationError {

    public final List<FieldError> errors;

    /**
     * Constructor.
     *
     * @param errors the errors
     */
    public ModelValidationError(List<FieldError> errors) {
        this.errors = errors;
    }

    /**
     * Returns the errors.
     *
     * @return the errors
     */
    @Override
    public String toString() {
        return "ModelValidationError{errors=" + errors + "}";
    }
}
