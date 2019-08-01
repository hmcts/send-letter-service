package uk.gov.hmcts.reform.sendletter.tasks;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Restriction for scheduled tasks which may impact FTP downtime rule.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface FtpConstraint {

    /**
     * Schedule name.
     */
    String name();
}
