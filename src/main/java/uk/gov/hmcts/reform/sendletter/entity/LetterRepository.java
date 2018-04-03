package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

import java.sql.Timestamp;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import javax.persistence.LockModeType;

public interface LetterRepository extends JpaRepository<Letter, UUID> {

    // This lockmode locks the returned rows
    // for both reading and writing.
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Stream<Letter> findByStatus(LetterStatus status);

    @Query(value = "select l from Letter l where l.status = ?1 and l.sentToPrintAt < ?2")
    Stream<StaleLettersOnly> findStaleLetters(LetterStatus state, Timestamp before);

    Optional<Letter> findByMessageIdAndStatusOrderByCreatedAtDesc(String messageId, LetterStatus status);

    Optional<Letter> findById(UUID id);

    Optional<Letter> findByIdAndService(UUID id, String service);
}
