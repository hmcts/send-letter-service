package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
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
    @ApiOperation(value = "Send letter to print and post service")
    @ApiResponses({
        @ApiResponse(code = 200, response = PrintResponse.class, message = "Successfully sent letter"),
        @ApiResponse(code = 400, message = RESPONSE_400),
        @ApiResponse(code = 401, message = RESPONSE_401),
        @ApiResponse(code = 403, message = RESPONSE_403)
    })
    public ResponseEntity<PrintResponse> print(
        @RequestHeader(name = "ServiceAuthorization", required = false) String serviceAuthHeader,
        @PathVariable("id") String id,
        @Valid @RequestBody PrintRequest printRequest
    ) {
        String serviceName = authService.authenticate(serviceAuthHeader);
        PrintResponse printResponse = printService.save(id, serviceName, printRequest);
        return ResponseEntity.ok(printResponse);
    }
}
