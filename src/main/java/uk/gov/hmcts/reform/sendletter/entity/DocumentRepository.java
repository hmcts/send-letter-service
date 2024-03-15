package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

/**
 * Document repository.
 */
@SuppressWarnings("checkstyle:LineLength")
public interface DocumentRepository extends JpaRepository<Document, UUID> {

    /**
     * Find one by checksum.
     * @param checkSum The checksum
     * @param recipientsChecksum The recipients checksum
     * @param createdAfter The created after
     * @return The document
     */
    @Query(value = "select * from documents d "
        + " WHERE d.checksum = :checkSum "
        + " AND d.recipients_checksum = :recipientsChecksum "
        + " AND d.created_at >= :createdAfter limit 1", nativeQuery = true)
    Optional<Document> findOneCreatedAfter(@Param("checkSum") String checkSum, @Param("recipientsChecksum") String recipientsChecksum, @Param("createdAfter") LocalDateTime createdAfter);
}
