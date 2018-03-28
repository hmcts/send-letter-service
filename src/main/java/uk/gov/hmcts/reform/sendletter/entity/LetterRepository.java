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

    String STALE_LETTERS_QUERY = "select l "
        + "from Letter l "
        + "where l.state = ?1"
        + "  and l.sentToPrintAt < ?2"
        + "  and l.isFailed = false";

    // This lockmode locks the returned rows
    // for both reading and writing.
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Stream<Letter> findByState(LetterState state);

    @Query(value = STALE_LETTERS_QUERY)
    Stream<StaleLetter> findStaleLetters(LetterState state, Timestamp before);

    Optional<Letter> findByMessageIdAndStateOrderByCreatedAtDesc(String messageId, LetterState state);

    Optional<Letter> findById(UUID id);

    Optional<Letter> findByIdAndService(UUID id, String service);
}
