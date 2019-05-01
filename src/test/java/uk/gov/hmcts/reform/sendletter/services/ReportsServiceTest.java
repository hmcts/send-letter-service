package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.LettersCountSummaryRepository;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCount;
import uk.gov.hmcts.reform.sendletter.model.out.LettersCountSummary;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@ExtendWith(MockitoExtension.class)
class ReportsServiceTest {

    @Mock
    LettersCountSummaryRepository repository;

    @Mock
    private ZeroRowFiller zeroRowFiller;

    @Mock
    ServiceFolderMapping serviceFolderMapping;

    private ReportsService service;

    @BeforeEach
    void setUp() {
        this.service = new ReportsService(this.repository, serviceFolderMapping, zeroRowFiller);

        when(this.zeroRowFiller.fill(any()))
            .thenAnswer(invocation -> invocation.getArgument(0)); // return data unchanged
    }

    @Test
    void should_return_letters_count_for_the_date() {
        LocalDate date = LocalDate.now();
        LocalTime timeFrom = LocalTime.of(17, 0, 0);
        LocalTime timeTo = LocalTime.of(16, 0, 0);

        //given
        given(repository.countByDate(formatDateTime(date.minusDays(1), timeFrom), formatDateTime(date, timeTo)))
            .willReturn(asList(
                new ServiceLettersCount("aService", 10),
                new ServiceLettersCount("bService", 20)
            ));

        given(serviceFolderMapping.getFolderFor("aService")).willReturn(Optional.of("ServiceA"));
        given(serviceFolderMapping.getFolderFor("bService")).willReturn(Optional.of("ServiceB"));

        //when
        List<LettersCountSummary> result = service.getCountFor(date);

        //then
        assertThat(result)
            .isNotEmpty()
            .hasSize(2)
            .usingFieldByFieldElementComparator()
            .containsExactlyInAnyOrder(
                new LettersCountSummary("ServiceA", 10),
                new LettersCountSummary("ServiceB", 20));
    }

    @Test
    void should_return_letters_count_excluding_the_test_service() {
        LocalDate date = LocalDate.now();
        LocalTime timeFrom = LocalTime.of(17, 0, 0);
        LocalTime timeTo = LocalTime.of(16, 0, 0);

        //given
        given(repository.countByDate(formatDateTime(date.minusDays(1), timeFrom), formatDateTime(date, timeTo)))
            .willReturn(asList(
                new ServiceLettersCount("aService", 10),
                new ServiceLettersCount("send_letter_tests", 20)
            ));
        given(serviceFolderMapping.getFolderFor("aService")).willReturn(Optional.of("ServiceA"));
        given(serviceFolderMapping.getFolderFor("send_letter_tests")).willReturn(Optional.of("BULKPRINT"));

        //when
        List<LettersCountSummary> result = service.getCountFor(date);

        //then
        assertThat(result).isNotEmpty()
            .hasSize(1)
            .usingFieldByFieldElementComparator()
            .containsExactlyInAnyOrder(new LettersCountSummary("ServiceA", 10));
    }

    @Test
    void should_return_empty_list_for_the_date_when_repo_returns_empty_collection() {
        LocalDate date = LocalDate.now();
        LocalTime timeFrom = LocalTime.of(17, 0, 0);
        LocalTime timeTo = LocalTime.of(16, 0, 0);

        //given
        given(repository.countByDate(formatDateTime(date.minusDays(1), timeFrom), formatDateTime(date, timeTo)))
            .willReturn(Collections.emptyList());

        //when
        List<LettersCountSummary> result = service.getCountFor(date);

        //then
        assertThat(result).isEmpty();
    }

    private LocalDateTime formatDateTime(LocalDate date, LocalTime time) {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.of(EUROPE_LONDON));
        String formattedDateTime = zonedDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
        return LocalDateTime.parse(formattedDateTime);
    }

}
