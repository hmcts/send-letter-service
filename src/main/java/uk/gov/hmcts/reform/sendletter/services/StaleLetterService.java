package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.EventType;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterEvent;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotStaleException;
import uk.gov.hmcts.reform.sendletter.services.date.DateCalculator;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;
import uk.gov.hmcts.reform.sendletter.util.CsvWriter;

import java.io.File;
import java.io.IOException;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import static java.time.temporal.ChronoUnit.DAYS;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_CREATED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_NOT_SENT;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.UTC;

@Service
public class StaleLetterService {
    private static final Logger log = LoggerFactory.getLogger(StaleLetterService.class);
    private static final ZoneId SERVICE_TIME_ZONE_ID = ZoneId.of(EUROPE_LONDON);
    private static final ZoneId DB_TIME_ZONE_ID = ZoneId.of(UTC);

    private final DateCalculator dateCalculator;
    private final LetterRepository letterRepository;
    private final LetterEventRepository letterEventRepository;
    private final int minStaleLetterAgeInBusinessDays;
    private final LocalTime ftpDowntimeStart;
    private final Clock clock;

    public static final  List<LetterStatus> LETTER_STATUS_TO_IGNORE =
            List.of(LetterStatus.Posted, LetterStatus.Aborted);

    public StaleLetterService(
        DateCalculator dateCalculator,
        LetterRepository letterRepository,
        LetterEventRepository letterEventRepository,
        @Value("${stale-letters.min-age-in-business-days}") int minStaleLetterAgeInBusinessDays,
        @Value("${ftp.downtime.from}") String ftpDowntimeStart,
        Clock clock
    ) {
        this.dateCalculator = dateCalculator;
        this.letterRepository = letterRepository;
        this.letterEventRepository = letterEventRepository;
        this.minStaleLetterAgeInBusinessDays = minStaleLetterAgeInBusinessDays;
        this.ftpDowntimeStart = LocalTime.parse(ftpDowntimeStart);
        this.clock = clock;
    }

    public List<BasicLetterInfo> getStaleLetters() {
        LocalDateTime localDateTime = calculateCutOffCreationDate()
                .withZoneSameInstant(DB_TIME_ZONE_ID)
                .toLocalDateTime();
        log.info("Stale letters before {} ", localDateTime);
        return letterRepository.findStaleLetters(localDateTime);
    }

    @Transactional
    public File getWeeklyStaleLetters() throws IOException {
        LocalDateTime localDateTime = calculateCutOffCreationDate()
                .withZoneSameInstant(DB_TIME_ZONE_ID)
                .toLocalDateTime();
        log.info("Stale letters before {} ", localDateTime);
        try (Stream<BasicLetterInfo> weeklyStaleLetters =
                letterRepository.findByStatusNotInAndTypeNotAndCreatedAtBetweenOrderByCreatedAtAsc(
                        LETTER_STATUS_TO_IGNORE, UploadLettersTask.SMOKE_TEST_LETTER_TYPE,
                        localDateTime.minusDays(6), localDateTime)) {
            return CsvWriter.writeStaleLettersReport(weeklyStaleLetters);
        }
    }

    @Transactional
    public int markStaleLetterAsNotSent(UUID id) {
        log.info("Marking stale letter as not sent {} as being stale", id);

        prepareChangingLetterStatus(
                id,
                MANUALLY_MARKED_AS_NOT_SENT,
                "Letter marked manually as not sent as being stale"
        );

        return letterRepository.markStaleLetterAsNotSent(id);
    }

    @Transactional
    public int markStaleLetterAsCreated(UUID id) {
        log.info("Marking the letter id {} as created to re-upload to FTP server", id);

        prepareChangingLetterStatus(
                id,
                MANUALLY_MARKED_AS_CREATED,
                "Letter marked manually as created for reprocessing"
        );

        return letterRepository.markStaleLetterAsCreated(id, LocalDateTime.now());
    }

    private void prepareChangingLetterStatus(UUID id, EventType manuallyMarkedAsNotSent, String notes) {
        Optional<Letter> letterOpt = letterRepository.findById(id);

        if (letterOpt.isEmpty()) {
            throw new LetterNotFoundException(id);
        }

        Letter letter = letterOpt.get();

        checkIfLetterIsStale(letter);

        createLetterEvent(
                letter,
                manuallyMarkedAsNotSent,
                notes
        );
    }

    private void checkIfLetterIsStale(Letter letter) {
        LocalDateTime localDateTime = calculateCutOffCreationDate()
                .withZoneSameInstant(DB_TIME_ZONE_ID)
                .toLocalDateTime();
        if (letter.getStatus() != Uploaded || !letter.getCreatedAt().isBefore(localDateTime)) {
            throw new LetterNotStaleException(letter.getId());
        }
    }

    private void createLetterEvent(Letter letter, EventType type, String notes) {
        log.info("Creating letter event {} for letter {}, notes {}", type, letter.getId(), notes);

        LetterEvent letterEvent = new LetterEvent(letter, type, notes, Instant.now());

        letterEventRepository.save(letterEvent);
    }

    /**
     * Calculates the cut-off creation date for stale letters.
     *
     * @return Cut-off creation date for stale letters. Only letters older than that
     *         can be considered stale.
     */
    private ZonedDateTime calculateCutOffCreationDate() {
        ZonedDateTime now =
            ZonedDateTime
                .now(clock)
                .withZoneSameInstant(SERVICE_TIME_ZONE_ID);

        // If on a given day a letter was created within FTP downtime window or later,
        // it should be treated like it was created the next day.
        LocalTime ftpDowntimeAdjustedTime =
            now.toLocalTime().isBefore(ftpDowntimeStart)
                ? now.toLocalTime()
                : ftpDowntimeStart;

        ZonedDateTime todayWithFtpDowntimeAdjustedTime =
            now
                .truncatedTo(DAYS)
                .plusNanos(ftpDowntimeAdjustedTime.toNanoOfDay());

        return dateCalculator.subtractBusinessDays(
            todayWithFtpDowntimeAdjustedTime,
            minStaleLetterAgeInBusinessDays
        );
    }

    public File getDownloadFile() throws IOException {
        List<BasicLetterInfo> staleLetters = getStaleLetters();
        return CsvWriter.writeStaleLettersToCsv(staleLetters);
    }
}
