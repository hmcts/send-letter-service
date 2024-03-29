package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

/**
 * Duplicate repository.
 */
@SuppressWarnings("checkstyle:LineLength")
public interface DuplicateRepository extends JpaRepository<DuplicateLetter, UUID> {

}
