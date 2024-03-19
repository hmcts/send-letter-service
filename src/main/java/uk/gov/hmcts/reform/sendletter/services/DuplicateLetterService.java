package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.DuplicateLetter;
import uk.gov.hmcts.reform.sendletter.entity.DuplicateRepository;

import java.util.Optional;
import java.util.UUID;

/**
 * Service for duplicate letter data.
 */
@Service
public class DuplicateLetterService {
    private DuplicateRepository duplicateRepository;

    /**
     * Constructor for the DuplicateLetterService.
     * @param duplicateRepository The repository for duplicate letter
     */
    public DuplicateLetterService(DuplicateRepository duplicateRepository) {
        this.duplicateRepository = duplicateRepository;
    }

    /**
     * Save a duplicate letter.
     * @param letter The duplicate letter
     */
    public void save(DuplicateLetter letter) {
        duplicateRepository.save(letter);
    }

    /**
     * Check for duplicate letters.
     * @param id The id
     * @return The duplicate letter
     */
    public Optional<DuplicateLetter> isDuplicate(UUID id) {
        return duplicateRepository.findById(id);
    }
}
