package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import javax.persistence.LockModeType;

public interface LetterRepository extends CrudRepository<Letter, UUID> {
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Stream<Letter> findByState(LetterState state);

    Optional<Letter> findById(UUID id);

    Optional<Letter> findByIdAndService(UUID id, String service);
}
