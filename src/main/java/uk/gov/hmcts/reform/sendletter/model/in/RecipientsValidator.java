package uk.gov.hmcts.reform.sendletter.model.in;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Optional;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class RecipientsValidator implements ConstraintValidator<ValidRecipients, Object> {

    private static final Logger logger = LoggerFactory.getLogger(RecipientsValidator.class);
    private final LaunchDarklyClient launchDarklyClient;

    @Autowired
    public RecipientsValidator(LaunchDarklyClient launchDarklyClient) {
        this.launchDarklyClient = launchDarklyClient;
    }

    @Override
    public void initialize(ValidRecipients constraintAnnotation) {
        // Initialization logic, if any
    }

    @Override
    public boolean isValid(Object value, ConstraintValidatorContext context) {
        // Toggle: FACT-1388 for making recipients field mandatory
        if (launchDarklyClient.isFeatureEnabled("FACT-1388")) {
            if (value == null) {
                logger.error("Additional_data field is null");
                return false; // Skip validation if the value is null
            }

            // Check if the "recipients" field exists using reflection
            try {
                ArrayList recipients = (ArrayList) Optional.ofNullable(((LinkedHashMap) value).get("recipients"))
                    .orElseThrow(() -> new NoSuchFieldException("Recipients field is not present"));
                if (recipients.isEmpty()) { // Check it is not an empty string as well
                    throw new NoSuchFieldException("Recipients field is empty");
                }
                logger.debug("Additional_data field is populated and recipients are: {}", recipients);
                return true;
            } catch (NoSuchFieldException e) {
                logger.error(e.toString());
                return false; // Field does not exist or is empty
            }
        } else {
            logger.debug("toggle is turned off for FACT-1388");
            return true;
        }
    }
}