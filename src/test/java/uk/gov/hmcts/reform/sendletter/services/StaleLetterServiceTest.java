package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterEvent;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotStaleException;
import uk.gov.hmcts.reform.sendletter.services.date.DateCalculator;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.io.File;
import java.io.IOException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static java.time.ZoneOffset.UTC;
import static java.time.temporal.ChronoUnit.DAYS;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_CREATED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_NOT_SENT;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;
import static uk.gov.hmcts.reform.sendletter.services.StaleLetterService.LETTER_STATUS_TO_IGNORE;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@ExtendWith(MockitoExtension.class)
class StaleLetterServiceTest {

    @Mock
    private DateCalculator dateCalculator;

    @Mock
    private LetterRepository letterRepository;

    @Mock
    private LetterEventRepository letterEventRepository;

    @Mock
    private Clock clock;

    @Test
    void getStaleLetters_should_call_date_calculator_with_current_time_when_before_ftp_downtime_start() {
        // given
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        String ftpDowntimeStartTime = "16:00";
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));

        setCurrentTimeAndZone(now);

        // when
        staleLetterService(ftpDowntimeStartTime, minStaleLetterAgeInBusinessDays).getStaleLetters();

        // then
        verify(dateCalculator).subtractBusinessDays(now, minStaleLetterAgeInBusinessDays);
    }

    @Test
    void getStaleLetters_should_call_date_calculator_with_ftp_downtime_start_time_when_after() {
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        // given
        String ftpDowntimeStartTime = "16:00";
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(16, 1) // currently it's after FTP downtime start time
            .atZone((ZoneId.of(EUROPE_LONDON)));

        setCurrentTimeAndZone(now);

        // when
        staleLetterService(ftpDowntimeStartTime, minStaleLetterAgeInBusinessDays).getStaleLetters();

        // then
        ZonedDateTime ftpDowntimeStartToday = now
            .truncatedTo(DAYS)
            .plusNanos(LocalTime.parse(ftpDowntimeStartTime).toNanoOfDay());

        verify(dateCalculator).subtractBusinessDays(
            ftpDowntimeStartToday,
            minStaleLetterAgeInBusinessDays
        );
    }

    @Test
    void getStaleLetters_should_return_call_the_repository_with_calculated_cut_off_date() {
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        // given
        ZonedDateTime cutOffTime = LocalDateTime.parse("2019-05-01T15:34:56.123").atZone(ZoneId.of(EUROPE_LONDON));

        given(dateCalculator.subtractBusinessDays(any(), anyInt())).willReturn(cutOffTime);

        // when
        staleLetterService("16:00", 2).getStaleLetters();

        // then
        verify(letterRepository).findStaleLetters(
            // as the DB stores UTC-based local datetimes, we expect conversion to happen
            cutOffTime.withZoneSameInstant(UTC).toLocalDateTime()
        );
    }

    @Test
    void should_return_weeky_stale_letters_repository_with_calculated_cut_off_date() throws IOException {
        // given
        setUpDateTime();

        ZonedDateTime cutOffTime = LocalDateTime.parse("2019-05-01T15:34:56.123").atZone(ZoneId.of(EUROPE_LONDON));

        given(dateCalculator.subtractBusinessDays(any(), anyInt())).willReturn(cutOffTime);

        // when
        File weeklyStaleLetters = staleLetterService("16:00", 2).getWeeklyStaleLetters();

        // then
        verify(letterRepository)
            .findByStatusNotInAndTypeNotAndCreatedAtBetweenOrderByCreatedAtAsc(
                eq(LETTER_STATUS_TO_IGNORE), eq(UploadLettersTask.SMOKE_TEST_LETTER_TYPE), isA(LocalDateTime.class),
                // as the DB stores UTC-based local datetimes, we expect conversion to happen
                eq(cutOffTime.withZoneSameInstant(UTC).toLocalDateTime()));

        assertThat(weeklyStaleLetters).isNotEmpty();

    }

    @Test
    void getStaleLetters_should_return_all_letters_returned_by_repository() {
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        reset(letterRepository);

        List<BasicLetterInfo> repositoryLetters = Arrays.asList(
            mock(BasicLetterInfo.class),
            mock(BasicLetterInfo.class),
            mock(BasicLetterInfo.class)
        );

        given(letterRepository.findStaleLetters(any())).willReturn(repositoryLetters);

        // when
        List<BasicLetterInfo> staleLetters =
            staleLetterService("16:00", 2).getStaleLetters();

        // then
        assertThat(staleLetters).hasSameElementsAs(repositoryLetters);
    }

    @Test
    void getStaleLetters_should_file_with_all_letters_returned_by_repository() throws IOException {
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        reset(letterRepository);

        UUID[] uuids = {UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID()};
        LocalDateTime[] localDateTimes = {LocalDateTime.now(), LocalDateTime.now().plusHours(1),
            LocalDateTime.now().plusHours(2)};
        //given
        List<BasicLetterInfo> repositoryLetters = Arrays.asList(
            new BasicLetterInfo(uuids[0], null, "Test", Uploaded,
                null, null, localDateTimes[0], localDateTimes[0], null),
            new BasicLetterInfo(uuids[1], null, "Test", Uploaded,
                null, null, localDateTimes[1], localDateTimes[1], null),
            new BasicLetterInfo(uuids[2], null, "Test", Uploaded,
                null, null, localDateTimes[2], localDateTimes[2], null)
        );

        given(letterRepository.findStaleLetters(any())).willReturn(repositoryLetters);

        // when
        StaleLetterService staleLetterService = staleLetterService("16:00", 2);
        File downloadFile = staleLetterService.getDownloadFile();

        // then
        assertThat(downloadFile).isNotEmpty();
    }

    @Test
    void should_get_stale_letter_file_when_record_present() throws IOException {
        setUpDateTime();
        given(letterRepository.findStaleLetters(any())).willReturn(emptyList());
        reset(letterRepository);

        given(letterRepository.findStaleLetters(any())).willReturn(Collections.emptyList());

        // when
        StaleLetterService staleLetterService = staleLetterService("16:00", 2);
        File downloadFile = staleLetterService.getDownloadFile();

        // then
        assertThat(downloadFile).isNotEmpty();

    }

    @Test
    void should_update_stale_letter_when_record_present() {
        // given
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));
        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays + 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Uploaded);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(letterRepository.markStaleLetterAsNotSent(letterId)).willReturn(1);

        String ftpDowntimeStartTime = "16:00";

        // when
        int res = staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsNotSent(letterId);

        // then
        assertThat(res).isEqualTo(1);

        ArgumentCaptor<LetterEvent> letterEventArgumentCaptor = ArgumentCaptor.forClass(LetterEvent.class);
        verify(letterEventRepository).save(letterEventArgumentCaptor.capture());
        assertThat(letterEventArgumentCaptor.getValue().getLetter()).isEqualTo(letter);
        assertThat(letterEventArgumentCaptor.getValue().getType()).isEqualTo(MANUALLY_MARKED_AS_NOT_SENT);
        assertThat(letterEventArgumentCaptor.getValue().getNotes())
            .isEqualTo("Letter marked manually as not sent as being stale");

        verify(letterRepository).markStaleLetterAsNotSent(letterId);
        verifyNoMoreInteractions(letterRepository, letterEventRepository);
    }

    @Test
    void should_throw_exception_when_record_not_present() {
        // given
        String ftpDowntimeStartTime = "16:00";
        int minStaleLetterAgeInBusinessDays = 123;

        UUID letterId = UUID.randomUUID();

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.empty());

        // when
        // then
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsNotSent(letterId))
            .isInstanceOf(LetterNotFoundException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_throw_exception_when_letter_not_uploaded() {
        // given
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));
        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays + 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Created);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));

        String ftpDowntimeStartTime = "16:00";

        // when
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsNotSent(letterId))
            .isInstanceOf(LetterNotStaleException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_throw_exception_when_letter_not_stale() {
        // given
        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));

        int minStaleLetterAgeInBusinessDays = 123;

        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays - 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Uploaded);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));

        String ftpDowntimeStartTime = "16:00";

        // when
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsNotSent(letterId))
            .isInstanceOf(LetterNotStaleException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_update_stale_letter_status_as_created_when_letter_exists() {
        // given
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));
        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays + 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Uploaded);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(letterRepository.markLetterAsCreated(letterId)).willReturn(1);

        String ftpDowntimeStartTime = "16:00";

        // when
        int res = staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsCreated(letterId);

        // then
        assertThat(res).isEqualTo(1);

        ArgumentCaptor<LetterEvent> letterEventArgumentCaptor = ArgumentCaptor.forClass(LetterEvent.class);
        verify(letterEventRepository).save(letterEventArgumentCaptor.capture());
        assertThat(letterEventArgumentCaptor.getValue().getLetter()).isEqualTo(letter);
        assertThat(letterEventArgumentCaptor.getValue().getType()).isEqualTo(MANUALLY_MARKED_AS_CREATED);
        assertThat(letterEventArgumentCaptor.getValue().getNotes())
            .isEqualTo("Letter marked manually as created for reprocessing");

        verify(letterRepository).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterRepository, letterEventRepository);
    }

    @Test
    void should_throw_exception_when_record_not_present1() {
        // given
        String ftpDowntimeStartTime = "16:00";
        int minStaleLetterAgeInBusinessDays = 123;

        UUID letterId = UUID.randomUUID();

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.empty());

        // when
        // then
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsCreated(letterId))
            .isInstanceOf(LetterNotFoundException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_throw_exception_when_re_uploading_the_letter_but_status_not_uploaded() {
        // given
        int minStaleLetterAgeInBusinessDays = 123;

        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));
        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays + 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Created);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));

        String ftpDowntimeStartTime = "16:00";

        // when
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsCreated(letterId))
            .isInstanceOf(LetterNotStaleException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_throw_exception_when_re_uploading_the_letter_but_letter_not_stale() {
        // given
        ZonedDateTime now = LocalDate.parse("2019-05-03")
            .atTime(15, 59) // currently it's before FTP downtime
            .atZone((ZoneId.of(EUROPE_LONDON)));

        int minStaleLetterAgeInBusinessDays = 123;

        given(dateCalculator.subtractBusinessDays(now, minStaleLetterAgeInBusinessDays))
            .willReturn(now.minusDays(minStaleLetterAgeInBusinessDays));
        setCurrentTimeAndZone(now);

        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
            letterId,
            letterId.toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now.minusDays(minStaleLetterAgeInBusinessDays - 1).toLocalDateTime(),
            null
        );
        letter.setStatus(Uploaded);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));

        String ftpDowntimeStartTime = "16:00";

        // when
        assertThatThrownBy(() -> staleLetterService(
            ftpDowntimeStartTime,
            minStaleLetterAgeInBusinessDays
        ).markStaleLetterAsCreated(letterId))
            .isInstanceOf(LetterNotStaleException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    private void setUpDateTime() {
        given(dateCalculator.subtractBusinessDays(any(), anyInt())).willReturn(ZonedDateTime.now());

        ZonedDateTime now = ZonedDateTime.now(ZoneId.of(EUROPE_LONDON));
        setCurrentTimeAndZone(now);
    }

    private StaleLetterService staleLetterService(String ftpDowntimeStart, int minStaleLetterAgeInBusinessDays) {
        return new StaleLetterService(
            dateCalculator,
            letterRepository,
            letterEventRepository,
            minStaleLetterAgeInBusinessDays,
            ftpDowntimeStart,
            clock
        );
    }

    private void setCurrentTimeAndZone(ZonedDateTime dateTime) {
        ZoneId zoneId = ZoneId.of(uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON);
        given(clock.instant()).willReturn(dateTime.toInstant());
        given(clock.getZone()).willReturn(zoneId);
    }
}
