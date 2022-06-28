package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

@SuppressWarnings("checkstyle:LineLength")
public interface LetterEventRepository extends JpaRepository<LetterEvent, Long> {
    List<LetterEvent> findAllByLetterOrderByCreatedAt(Letter letter);
}
