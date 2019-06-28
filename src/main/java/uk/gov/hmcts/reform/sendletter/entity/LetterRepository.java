package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@SuppressWarnings("checkstyle:LineLength")
public interface LetterRepository extends JpaRepository<Letter, UUID> {

    List<Letter> findFirst3ByStatus(LetterStatus status);

    List<Letter> findByStatus(LetterStatus status);

    @Query("select new uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo(l.id, l.checksum, l.service, l.status, l.type, l.createdAt, l.sentToPrintAt)"
        + " from Letter l "
        + " where l.status not in ('Posted', 'Aborted')"
        + " and l.createdAt < :createdBefore"
        + " and l.type <> '" + UploadLettersTask.SMOKE_TEST_LETTER_TYPE + "'"
        + " order by l.createdAt asc")
    List<BasicLetterInfo> findStaleLetters(
        @Param("createdBefore") LocalDateTime createdBefore
    );

    Optional<Letter> findByChecksumAndStatusOrderByCreatedAtDesc(String checksum, LetterStatus status);

    Optional<Letter> findById(UUID id);

    @Query("select l.status from Letter l where l.id = :id")
    Optional<LetterStatus> findStatusById(@Param("id") UUID id);

    @Modifying(clearAutomatically = true)
    @Query(""
        + "update Letter l "
        + "set l.fileContent = null, l.status = 'Posted', l.printedAt = :printedAt "
        + "where l.id = :id"
    )
    void markAsPosted(@Param("id") UUID id, @Param("printedAt") LocalDateTime printedAt);

    Optional<Letter> findByIdAndService(UUID id, String service);

    int countByStatus(LetterStatus status);
}
