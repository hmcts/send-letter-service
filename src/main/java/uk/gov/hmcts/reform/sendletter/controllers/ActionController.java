package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.v3.oas.annotations.Operation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.exception.InvalidApiKeyException;
import uk.gov.hmcts.reform.sendletter.services.LetterActionService;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

import static org.slf4j.LoggerFactory.getLogger;
import static org.springframework.format.annotation.DateTimeFormat.ISO.DATE;
import static org.springframework.format.annotation.DateTimeFormat.ISO.TIME;
import static org.springframework.http.HttpHeaders.AUTHORIZATION;

/**
 * Controller for letter actions.
 */
@RestController
@Validated
@RequestMapping(path = "/letters")
public class ActionController {
    private static final Logger logger = getLogger(ActionController.class);

    private final StaleLetterService staleLetterService;
    private final LetterActionService letterActionService;
    private final String sendLetterApiKey;

    /**
     * Constructor for the ActionController.
     * @param staleLetterService The stale letter service
     * @param letterActionService The letter action service
     * @param apiKey The API key
     */
    public ActionController(
        StaleLetterService staleLetterService,
        LetterActionService letterActionService,
        @Value("${actions.api-key}") String apiKey
    ) {
        this.staleLetterService = staleLetterService;
        this.letterActionService = letterActionService;
        this.sendLetterApiKey = apiKey;
    }

    /**
     * Mark stale letter as not sent by ID.
     * @param authHeader The authorization header
     * @param id The letter ID
     * @return The response entity
     */
    @PutMapping(path = "/{id}/mark-not-sent")
    @Operation(summary = "Mark stale letter as not sent by ID")
    public ResponseEntity<Void> markAsNotSent(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id
    ) {
        logger.info("Marking stale letter as not sent {}", id);

        validateAuthorization(authHeader);

        staleLetterService.markStaleLetterAsNotSent(id);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Mark stale letter as created by ID.
     * @param authHeader The authorization header
     * @param id The letter ID
     * @return The response entity
     */
    @PutMapping(path = "/{id}/mark-created")
    @Operation(summary = "Mark stale letter as created by letter ID")
    public ResponseEntity<Void> markAsCreated(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id
    ) {
        logger.info("Marking stale letter status as 'Created' to re-upload to SFTP server {}", id);

        validateAuthorization(authHeader);

        letterActionService.markLetterAsCreated(id);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Mark letter as aborted by ID.
     * @param authHeader The authorization header
     * @param id The letter ID
     * @return The response entity
     */
    @PutMapping(path = "/{id}/mark-aborted")
    @Operation(summary = "Mark letter as aborted by letter ID")
    public ResponseEntity<Void> markAsAborted(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id
    ) {
        logger.info("Marking letter status as 'Aborted' {}", id);

        validateAuthorization(authHeader);

        letterActionService.markLetterAsAborted(id);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Mark letter as posted locally by ID.
     * @param authHeader The authorization header
     * @param id The letter ID
     * @return The response entity
     */
    @PutMapping(path = "/{id}/mark-posted-locally")
    @Operation(summary = "Mark letter as posted locally by letter ID")
    public ResponseEntity<Void> markAsPostedLocally(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id
    ) {
        logger.info("Marking letter status as 'PostedLocally' {}", id);

        validateAuthorization(authHeader);

        letterActionService.markLetterAsPostedLocally(id);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Mark letter as posted by ID.
     * @param authHeader The authorization header
     * @param id The letter ID
     * @param date The date
     * @param time The time
     * @return The response entity
     */
    @PutMapping(path = "/{id}/mark-posted")
    @Operation(summary = "Mark letter as posted by letter ID")
    public ResponseEntity<Void> markAsPosted(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id,
        @RequestParam(name = "date") @DateTimeFormat(iso = DATE) LocalDate date,
        @RequestParam(name = "time") @DateTimeFormat(iso = TIME) LocalTime time
    ) {
        logger.info("Marking letter status as 'Posted' {}", id);

        validateAuthorization(authHeader);

        letterActionService.markLetterAsPosted(id, date, time);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Validate the authorization header.
     * @param authorizationKey The authorization header
     */
    private void validateAuthorization(String authorizationKey) {

        if (StringUtils.isEmpty(authorizationKey)) {
            logger.error("API Key is missing");
            throw new InvalidApiKeyException("API Key is missing");
        } else if (!authorizationKey.equals("Bearer " + sendLetterApiKey)) {
            logger.error("Invalid API Key");
            throw new InvalidApiKeyException("Invalid API Key");
        }
    }
}
