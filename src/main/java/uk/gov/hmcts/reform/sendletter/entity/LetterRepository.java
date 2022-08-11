package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

@SuppressWarnings("checkstyle:LineLength")
public interface LetterRepository extends JpaRepository<Letter, UUID> {

    @Query(value = "select * from letters l "
        + " where l.status in ('Created', 'FailedToUpload')"
        + " and l.created_at <= :createdBefore order by l.created_at asc limit 1", nativeQuery = true)
    Optional<Letter> findFirstLetterToUpload(@Param("createdBefore") LocalDateTime createdBefore);

    List<Letter> findByStatus(LetterStatus status);

    @Query("select new uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo(l.id, l.checksum, l.service, l.status, l.type, l.encryptionKeyFingerprint, l.createdAt, l.sentToPrintAt, l.printedAt)"
        + " from Letter l "
        + " where l.status not in ('Posted', 'Aborted', 'NotSent')"
        + " and l.createdAt < :createdBefore"
        + " and l.type <> '" + UploadLettersTask.SMOKE_TEST_LETTER_TYPE + "'"
        + " order by l.createdAt asc")
    List<BasicLetterInfo> findStaleLetters(
        @Param("createdBefore") LocalDateTime createdBefore
    );


    Stream<BasicLetterInfo> findByStatusNotInAndTypeNotAndCreatedAtBetweenOrderByCreatedAtAsc(Collection<LetterStatus> letterStatuses,
                                                               String type, LocalDateTime from, LocalDateTime to);

    @Query("select new uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo(l.id, l.checksum, l.service, l.status, l.type, l.encryptionKeyFingerprint, l.createdAt, l.sentToPrintAt, l.printedAt)"
        + " from Letter l "
        + " where l.status = 'Created'"
        + " and l.type <> '" + UploadLettersTask.SMOKE_TEST_LETTER_TYPE + "'"
        + " order by l.createdAt asc")
    List<BasicLetterInfo> findPendingLetters();

    Stream<BasicLetterInfo> findByCreatedAtBeforeAndStatusAndTypeNot(LocalDateTime createdBefore, LetterStatus status, String type);

    @Query("select new uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo(l.id, l.checksum, l.service, l.status, l.type, l.encryptionKeyFingerprint, l.createdAt, l.sentToPrintAt, l.printedAt)"
        + " from Letter l "
        + " where date(l.createdAt) = :createdAt "
        + " and l.type <> '" + UploadLettersTask.SMOKE_TEST_LETTER_TYPE + "'"
        + " order by l.createdAt asc")
    List<BasicLetterInfo> findCreatedAt(@Param("createdAt") LocalDate createdAt);

    Optional<Letter> findByChecksumAndStatusOrderByCreatedAtDesc(String checksum, LetterStatus status);

    Optional<Letter> findById(UUID id);

    @Query("SELECT l.status FROM Letter l WHERE l.id = :id")
    Optional<LetterStatus> findLetterStatus(@Param("id") UUID id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l SET l.status = 'Posted', l.printedAt = :printedAt, l.fileContent = null WHERE l.id = :id")
    int markLetterAsPosted(@Param("id") UUID id, @Param("printedAt") LocalDateTime printedAt);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l"
        + " SET l.fileContent = null "
        + " WHERE l.createdAt < :createdBefore "
        + " AND l.status = :status"
    )
    int clearFileContent(
        @Param("createdBefore") LocalDateTime createdBefore,
        @Param("status") LetterStatus status
    );

    int countByStatus(LetterStatus status);


    Stream<BasicLetterInfo> findByStatusAndCreatedAtBetweenOrderByCreatedAtAsc(LetterStatus status, LocalDateTime from, LocalDateTime to);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l"
            + " SET l.status = 'NotSent'"
            + " WHERE l.id = :id AND l.status = 'Uploaded'"
    )
    int markStaleLetterAsNotSent(@Param("id") UUID id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l"
        + " SET l.status = 'Created'"
        + " WHERE l.id = :id AND l.status in ('Uploaded', 'FailedToUpload')"
    )
    int markLetterAsCreated(@Param("id") UUID id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l"
        + " SET l.status = 'Aborted'"
        + " WHERE l.id = :id AND l.status <> 'Posted'"
    )
    int markLetterAsAborted(@Param("id") UUID id);

    @Modifying(clearAutomatically = true)
    @Query("UPDATE Letter l"
        + " SET l.status = 'PostedLocally'"
        + " WHERE l.id = :id AND l.status = 'Uploaded'"
    )
    int markLetterAsPostedLocally(@Param("id") UUID id);
}
