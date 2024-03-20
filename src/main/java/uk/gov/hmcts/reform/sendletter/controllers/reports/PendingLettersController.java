package uk.gov.hmcts.reform.sendletter.controllers.reports;

import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.model.out.PendingLetter;
import uk.gov.hmcts.reform.sendletter.model.out.PendingLettersResponse;
import uk.gov.hmcts.reform.sendletter.services.PendingLettersService;

import java.util.List;

import static java.util.stream.Collectors.toList;

/**
 * Controller for pending letters.
 */
@RestController
public class PendingLettersController {

    private final PendingLettersService service;

    /**
     * Constructor for the PendingLettersController.
     * @param service The pending letters service
     */
    public PendingLettersController(PendingLettersService service) {
        this.service = service;
    }

    /**
     * Retrieves letters that were not uploaded to SFTP yet.
     * @return The pending letters response
     */
    @GetMapping(path = "/pending-letters")
    @Operation(description = "Retrieves letters that were not uploaded to SFTP yet.")
    public PendingLettersResponse getPendingLetters() {
        List<PendingLetter> pendingLetters =
            service
                .getPendingLetters()
                .stream()
                .map(l -> new PendingLetter(
                    l.getId(),
                    l.getService(),
                    l.getCreatedAt(),
                    l.getEncryptionKeyFingerprint()
                ))
                .collect(toList());

        return new PendingLettersResponse(pendingLetters);
    }
}
