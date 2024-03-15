package uk.gov.hmcts.reform.sendletter.controllers.reports;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.model.out.StaleLetter;
import uk.gov.hmcts.reform.sendletter.model.out.StaleLetterResponse;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

/**
 * Controller for stale letters.
 */
@RestController
@RequestMapping(path = "/stale-letters")
public class StaleLetterController {

    private final StaleLetterService staleLetterService;

    /**
     * Constructor for the StaleLetterController.
     * @param staleLetterService The stale letter service
     */
    public StaleLetterController(StaleLetterService staleLetterService) {
        this.staleLetterService = staleLetterService;
    }

    /**
     * Retrieves stale letters.
     * @return The stale letters response
     */
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(description = "Retrieves stale letters")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = StaleLetterResponse.class)),
            description = "Retrieved stale letters"),
        @ApiResponse(responseCode = "500", description = "Error occurred while retrieving stale letters")
    })
    public StaleLetterResponse getStaleLetters() {
        List<StaleLetter> staleLetters =
            staleLetterService
                .getStaleLetters()
                .stream()
                .map(this::mapToStaleLetter)
                .toList();

        return new StaleLetterResponse(staleLetters);
    }

    /**
     * Downloads file with stale letters.
     * @return The csv file
     */
    @GetMapping(value = "download", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    @Operation(description = "Downloads file with stale letters")
    @ApiResponses({
        @ApiResponse(responseCode = "200",
            content = @Content(schema = @Schema(implementation = StaleLetterResponse.class)),
            description = "Retrieved stale letters file"),
        @ApiResponse(responseCode = "500", description = "Error occurred while retrieving stale letters file")
    })
    public ResponseEntity<byte[]> getFileWithStaleLetters() throws IOException {
        File csvFile = staleLetterService.getDownloadFile();
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=stale-letters.csv")
                .body(Files.readAllBytes(csvFile.toPath()));
    }

    /**
     * Maps a basic letter info to a stale letter.
     * @param dbLetter The basic letter info
     * @return The stale letter
     */
    private StaleLetter mapToStaleLetter(BasicLetterInfo dbLetter) {
        return new StaleLetter(
            dbLetter.getId(),
            dbLetter.getStatus(),
            dbLetter.getService(),
            dbLetter.getCreatedAt(),
            dbLetter.getSentToPrintAt()
        );
    }
}
