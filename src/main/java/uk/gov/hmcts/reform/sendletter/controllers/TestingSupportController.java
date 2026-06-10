package uk.gov.hmcts.reform.sendletter.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.exception.TestingSupportLetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.DownloadedLetterFile;
import uk.gov.hmcts.reform.sendletter.services.TestingSupportService;

import java.util.UUID;

import static org.springframework.http.ResponseEntity.ok;

/**
 * Controller for testing support endpoints.
 */
@RestController
@RequestMapping(path = "/testing-support")
@ConditionalOnProperty(prefix = "testing-support", name = "enabled", havingValue = "true")
public class TestingSupportController {

    private final TestingSupportService testingSupportService;

    /**
     * Construct a new TestingSupportController.
     * @param testingSupportService the testing support service
     */
    public TestingSupportController(TestingSupportService testingSupportService) {
        this.testingSupportService = testingSupportService;
    }

    /**
     * Downloads a letter file from SFTP.
     * @param id the letter id
     * @return The uploaded letter file
     */
    @GetMapping(path = "/download/{id}", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    @Operation(description = "Download letter file from SFTP")
    @ApiResponses({
        @ApiResponse(responseCode = "200", description = "Successfully downloaded letter file"),
        @ApiResponse(responseCode = "404", description = "Letter or uploaded letter file not found"),
        @ApiResponse(responseCode = "503", description = "Unable to download letter file from SFTP")
    })
    public ResponseEntity<byte[]> downloadLetterFile(@PathVariable String id) {
        UUID letterId;

        try {
            letterId = UUID.fromString(id);
        } catch (IllegalArgumentException exception) {
            throw new TestingSupportLetterNotFoundException(id, exception);
        }

        DownloadedLetterFile file = testingSupportService.downloadLetterFile(letterId);

        return ok()
            .contentType(MediaType.APPLICATION_OCTET_STREAM)
            .header(
                HttpHeaders.CONTENT_DISPOSITION,
                ContentDisposition.attachment().filename(file.getFilename()).build().toString()
            )
            .body(file.getContent());
    }
}
