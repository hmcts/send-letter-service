package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.PrintService;

import javax.validation.Valid;

import static uk.gov.hmcts.reform.sendletter.controllers.ControllerResponseMessage.RESPONSE_400;
import static uk.gov.hmcts.reform.sendletter.controllers.ControllerResponseMessage.RESPONSE_401;
import static uk.gov.hmcts.reform.sendletter.controllers.ControllerResponseMessage.RESPONSE_403;

@RestController
@Validated
@RequestMapping(
    path = "/print-jobs",
    produces = MediaType.APPLICATION_JSON_VALUE
)
public class PrintLetterController {

    private final PrintService printService;
    private final AuthService authService;

    public PrintLetterController(
        PrintService printService,
        AuthService authService
    ) {
        this.printService = printService;
        this.authService = authService;
    }

    @PutMapping(
        consumes = MediaTypes.PRINT_V1,
        path = {"/{id}"}
    )
    @Operation(description = "Send letter to print and post service")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = PrintResponse.class)),
            description = "Successfully sent letter"),
        @ApiResponse(responseCode = "400", description = RESPONSE_400),
        @ApiResponse(responseCode = "401", description = RESPONSE_401),
        @ApiResponse(responseCode = "403", description = RESPONSE_403)
    })
    public ResponseEntity<PrintResponse> print(
        @RequestHeader(name = "ServiceAuthorization", required = false) String serviceAuthHeader,
        @PathVariable("id") String id,
        @Valid @RequestBody PrintRequest printRequest
    ) {
        String serviceName = authService.authenticate(serviceAuthHeader);
        var printResponse = printService.save(id, serviceName, printRequest);
        return ResponseEntity.ok(printResponse);
    }
}
