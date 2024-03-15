package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.ExceptionLetter;
import uk.gov.hmcts.reform.sendletter.entity.ExceptionRepository;

import java.util.Optional;
import java.util.UUID;

/**
 * Service for exception letter data.
 */
@Service
public class ExceptionLetterService {
    private ExceptionRepository exceptionRepository;

    /**
     * Constructor for the ExceptionLetterService.
     * @param exceptionRepository The repository for exception letter
     */
    public ExceptionLetterService(ExceptionRepository exceptionRepository) {
        this.exceptionRepository = exceptionRepository;
    }

    /**
     * Save an exception letter.
     * @param exceptionLetter The exception letter
     */
    public void save(ExceptionLetter exceptionLetter) {
        exceptionRepository.save(exceptionLetter);
    }

    /**
     * Check for exception letters.
     * @param id The id
     * @return The exception letter
     */
    public Optional<ExceptionLetter> isException(UUID id) {
        return exceptionRepository.findById(id);
    }
}
