package uk.gov.hmcts.reform.sendletter.model.in;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;

/**
 * Annotation for validating the recipients field in the AdditionalData object.
 */
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = RecipientsValidator.class)
@ResponseStatus(HttpStatus.BAD_REQUEST)
public @interface ValidRecipients {
    String message() default "Invalid recipients. Please check that the recipients attribute is included "
        + "within the additional_data field, and that it includes a list of names (an array of strings). "
        + "For example: ['Joe Blobbins', 'Gilligan Phillips']";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
