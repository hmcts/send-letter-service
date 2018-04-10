package uk.gov.hmcts.reform.sendletter.logging;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to be used against methods to be picked up by AppInsights aspect dependency advice.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface InternalDependency {

    /**
     * Name of the internal dependency to be tracked as an event in Azure AppInsights.
     *
     * @return Name of the event
     */
    String value();
}
