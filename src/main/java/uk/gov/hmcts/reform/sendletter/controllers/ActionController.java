package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.v3.oas.annotations.Operation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.exception.InvalidApiKeyException;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.util.UUID;

import static org.slf4j.LoggerFactory.getLogger;
import static org.springframework.http.HttpHeaders.AUTHORIZATION;

@RestController
@Validated
@RequestMapping(path = "/letters")
public class ActionController {
    private static final Logger logger = getLogger(ActionController.class);

    private final StaleLetterService staleLetterService;
    private final String sendLetterApiKey;

    public ActionController(
        StaleLetterService staleLetterService,
        @Value("${actions.api-key}") String apiKey
    ) {
        this.staleLetterService = staleLetterService;
        this.sendLetterApiKey = apiKey;
    }

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

    @PutMapping(path = "/{id}/mark-created")
    @Operation(summary = "Mark stale letter as created by letter ID")
    public ResponseEntity<Void> markAsCreated(
        @RequestHeader(value = AUTHORIZATION, required = false) String authHeader,
        @PathVariable UUID id
    ) {
        logger.info("Marking stale letter status as 'Created' to re-upload to SFTP server {}", id);

        validateAuthorization(authHeader);

        staleLetterService.markStaleLetterAsCreated(id);
        return new ResponseEntity<>(HttpStatus.OK);
    }

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
