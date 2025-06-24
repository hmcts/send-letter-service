package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.Valid;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;
import uk.gov.hmcts.reform.sendletter.model.out.ExtendedLetterStatus;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;
import uk.gov.hmcts.reform.sendletter.model.out.SendLetterResponse;
import uk.gov.hmcts.reform.sendletter.model.out.v2.LetterStatusV2;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.LetterService;

import java.util.UUID;

import static org.springframework.http.ResponseEntity.ok;

/**
 * Controller for sending letters.
 */
@RestController
@Validated
@RequestMapping(
    path = "/letters",
    produces = {MediaType.APPLICATION_JSON_VALUE}
)
public class SendLetterController {

    private final LetterService letterService;
    private final AuthService authService;

    /**
     * Construct a new SendLetterController.
     * @param letterService the letter service
     * @param authService the auth service
     */
    public SendLetterController(
        LetterService letterService,
        AuthService authService
    ) {
        this.letterService = letterService;
        this.authService = authService;
    }

    /**
     * Send letter to print and post service.
     * @param serviceAuthHeader the service authorization header
     * @param isAsync the isAsync flag
     * @param letter the letter
     * @return The send letter response
     */
    @PostMapping(consumes = MediaTypes.LETTER_V2, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(description = "Send letter to print and post service")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = SendLetterResponse.class)),
            description = "Successfully sent letter"),
        @ApiResponse(responseCode = "400", description = ControllerResponseMessage.RESPONSE_400),
        @ApiResponse(responseCode = "401", description = ControllerResponseMessage.RESPONSE_401),
        @ApiResponse(responseCode = "403", description = ControllerResponseMessage.RESPONSE_403)
    })
    public ResponseEntity<SendLetterResponse> sendLetter(
        @RequestHeader(name = "ServiceAuthorization", required = false) String serviceAuthHeader,
        @RequestParam(name = "isAsync", defaultValue = "false") String isAsync,
        @Parameter(description = "Letter consisting of documents and type", required = true)
        @Valid @RequestBody LetterWithPdfsRequest letter
    ) {
        String serviceName = authService.authenticate(serviceAuthHeader);
        UUID letterId = letterService.save(letter, serviceName, isAsync);
        return ok().body(new SendLetterResponse(letterId));
    }

    /**
     * Send letter to print and post service.
     * @param serviceAuthHeader the service authorization header
     * @param isAsync the isAsync flag
     * @param letter the letter
     * @return The send letter response
     */
    @PostMapping(consumes = MediaTypes.LETTER_V3, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(description = "Send letter to print and post service")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = SendLetterResponse.class))),
        @ApiResponse(responseCode = "401", description = ControllerResponseMessage.RESPONSE_401),
        @ApiResponse(responseCode = "403", description = ControllerResponseMessage.RESPONSE_403)
    })
    public ResponseEntity<SendLetterResponse> sendLetter(
        @RequestHeader(name = "ServiceAuthorization", required = false) String serviceAuthHeader,
        @RequestParam(name = "isAsync", defaultValue = "false") String isAsync,
        @Valid @RequestBody LetterWithPdfsAndNumberOfCopiesRequest letter
    ) {
        String serviceName = authService.authenticate(serviceAuthHeader);
        UUID letterId = letterService.save(letter, serviceName, isAsync);
        return ok().body(new SendLetterResponse(letterId));
    }

    /**
     * Get letter status.
     * @param id the letter id
     * @param isAdditionalInfoRequired the isAdditionalInfoRequired flag
     * @param isDuplicate the isDuplicate flag
     * @return The letter status
     */
    @GetMapping(path = "/{id}/extended-status")
    @Operation(description = "Get extended letter status")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = SendLetterResponse.class))),
        @ApiResponse(responseCode = "404", description = "Letter not found")
    })
    public ResponseEntity<LetterStatus> getExtendedLetterStatus(
        @PathVariable String id,
        @RequestParam(name = "include-additional-info", defaultValue = "false") String isAdditionalInfoRequired,
        @RequestParam(name = "check-duplicate", defaultValue = "false") String isDuplicate
    ) {
        ExtendedLetterStatus letterStatus = letterService.getExtendedStatus(
            getLetterIdFromString(id),
            isAdditionalInfoRequired,
            isDuplicate
        );

        return ok(letterStatus);
    }

    /**
     * Get letter status.
     * @param id the letter id
     * @param isAdditionalInfoRequired the isAdditionalInfoRequired flag
     * @param isDuplicate the isDuplicate flag
     * @return The letter status
     */
    @GetMapping(path = "/{id}")
    @Operation(description = "Get letter status")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = SendLetterResponse.class))),
        @ApiResponse(responseCode = "404", description = "Letter not found")
    })
    public ResponseEntity<LetterStatus> getLetterStatus(
        @PathVariable String id,
        @RequestParam(name = "include-additional-info", defaultValue = "false") String isAdditionalInfoRequired,
        @RequestParam(name = "check-duplicate", defaultValue = "false") String isDuplicate
    ) {
        LetterStatus letterStatus = letterService.getStatus(getLetterIdFromString(id),
                isAdditionalInfoRequired, isDuplicate);
        return ok(letterStatus);
    }

    /**
     * Get letter status with copies requested.
     * @param id the letter id
     * @return The letter status
     */
    @GetMapping(path = "/v2/{id}")
    @Operation(description = "Get letter status with copies requested")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = SendLetterResponse.class)), description = "Success"),
        @ApiResponse(responseCode = "404", description = "Letter not found")
    })
    public ResponseEntity<LetterStatusV2> getLatestLetterStatus(
        @PathVariable String id
    ) {
        LetterStatusV2 letterStatus =
                letterService.getLatestStatus(getLetterIdFromString(id));
        return ok(letterStatus);
    }

    /**
     * Get letter id from string.
     * @param letterId the letter id
     * @return The letter id
     */
    private UUID getLetterIdFromString(String letterId) {
        try {
            return UUID.fromString(letterId);
        } catch (IllegalArgumentException exception) {
            throw new LetterNotFoundException(letterId, exception);
        }
    }
}
