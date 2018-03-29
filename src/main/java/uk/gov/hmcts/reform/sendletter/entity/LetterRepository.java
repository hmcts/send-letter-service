package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;

import java.util.Iterator;
import java.util.Optional;
import java.util.UUID;
import javax.persistence.LockModeType;

public interface LetterRepository extends JpaRepository<Letter, UUID> {
    // This lockmode locks the returned rows
    // for both reading and writing.
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Iterator<Letter> findByStatus(LetterStatus status);

    Optional<Letter> findByMessageIdAndStatusOrderByCreatedAtDesc(String messageId, LetterStatus status);

    Optional<Letter> findById(UUID id);

    Optional<Letter> findByIdAndService(UUID id, String service);
}
