package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

@SuppressWarnings("checkstyle:LineLength")
public interface DocumentRepository extends JpaRepository<Document, UUID> {

    @Query(value = "select * from documents d "
        + " WHERE d.checksum = :checkSum "
        + " AND d.created_at >= :createdAfter limit 1", nativeQuery = true)
    Optional<Document> findCreatedAfter(@Param("checkSum") String checkSum, @Param("createdAfter") LocalDateTime createdAfter);

}
