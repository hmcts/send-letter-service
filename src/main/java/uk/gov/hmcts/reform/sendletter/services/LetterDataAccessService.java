package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

/**
 * Service to wrap data access layer automatically proxied via JPA interfaces.
 * {@code @Modifying} repo methods have to be called from  {@code @Transactional} service methods.
 * This service may serve same wrapping layer idea in case JPA -> JDBC happens.
 */
@Service
public class LetterDataAccessService {

    private final LetterRepository repository;

    public LetterDataAccessService(LetterRepository repository) {
        this.repository = repository;
    }

    public Optional<LetterStatus> findLetterStatus(UUID id) {
        return repository.findLetterStatus(id);
    }

    @Transactional
    public void markLetterAsPosted(UUID id, LocalDateTime printedAt) {
        repository.markLetterAsPosted(id, printedAt);
    }

    @Transactional
    public int clearFileContent(LocalDateTime createdBefore, LetterStatus status) {
        return repository.clearFileContent(createdBefore, status);
    }
}
