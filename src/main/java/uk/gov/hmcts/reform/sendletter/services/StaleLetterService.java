package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.services.date.DateCalculator;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;
import uk.gov.hmcts.reform.sendletter.util.CsvWriter;

import java.io.File;
import java.io.IOException;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import static java.time.temporal.ChronoUnit.DAYS;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.UTC;

@Service
public class StaleLetterService {
    private static final Logger log = LoggerFactory.getLogger(StaleLetterService.class);
    private static final ZoneId SERVICE_TIME_ZONE_ID = ZoneId.of(EUROPE_LONDON);
    private static final ZoneId DB_TIME_ZONE_ID = ZoneId.of(UTC);

    private final DateCalculator dateCalculator;
    private final LetterRepository letterRepository;
    private final int minStaleLetterAgeInBusinessDays;
    private final LocalTime ftpDowntimeStart;
    private final Clock clock;

    public static final  List<LetterStatus> LETTER_STATUS_TO_IGNORE =
            List.of(new LetterStatus[]{LetterStatus.Posted, LetterStatus.Aborted});


    public StaleLetterService(
        DateCalculator dateCalculator,
        LetterRepository letterRepository,
        @Value("${stale-letters.min-age-in-business-days}") int minStaleLetterAgeInBusinessDays,
        @Value("${ftp.downtime.from}") String ftpDowntimeStart,
        Clock clock
    ) {
        this.dateCalculator = dateCalculator;
        this.letterRepository = letterRepository;
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

    public File getWeeklyStaleLetters() throws IOException {
        LocalDateTime localDateTime = calculateCutOffCreationDate()
                .withZoneSameInstant(DB_TIME_ZONE_ID)
                .toLocalDateTime();
        log.info("Stale letters before {} ", localDateTime);
        try (Stream<Letter> weeklyStaleLetters =
                letterRepository.findByStatusNotInAndTypeNotAndCreatedAtBetweenOrderByCreatedAtAsc(
                        LETTER_STATUS_TO_IGNORE, UploadLettersTask.SMOKE_TEST_LETTER_TYPE,
                        localDateTime.minusDays(6), localDateTime)) {
            return CsvWriter.writeStaleLettersReport(weeklyStaleLetters);
        }
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
