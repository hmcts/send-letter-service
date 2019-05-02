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
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ReportsServiceTest {

    @Mock
    LettersCountSummaryRepository repository;

    @Mock
    private ZeroRowFiller zeroRowFiller;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    private ReportsService service;

    @BeforeEach
    void setUp() {
        this.service = new ReportsService(this.repository, serviceFolderMapping, zeroRowFiller, "16:00", "17:00");

        when(this.zeroRowFiller.fill(any()))
            .thenAnswer(invocation -> invocation.getArgument(0)); // return data unchanged
    }

    @Test
    void should_return_letters_count_for_the_date() {
        LocalDate date = LocalDate.of(2019, 4, 25);
        LocalTime timeFrom = LocalTime.parse("17:00");
        LocalTime timeTo = LocalTime.parse("16:00");

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
        LocalDate date = LocalDate.of(2019, 4, 25);
        LocalTime timeFrom = LocalTime.parse("17:00");
        LocalTime timeTo = LocalTime.parse("16:00");

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
    void should_return_letters_count_excluding_the_nulls() {
        LocalDate date = LocalDate.of(2019, 4, 25);
        LocalTime timeFrom = LocalTime.parse("17:00");
        LocalTime timeTo = LocalTime.parse("16:00");

        //given
        given(repository.countByDate(formatDateTime(date.minusDays(1), timeFrom), formatDateTime(date, timeTo)))
            .willReturn(asList(
                new ServiceLettersCount("aService", 10),
                new ServiceLettersCount(null, 2)
            ));
        given(serviceFolderMapping.getFolderFor("aService")).willReturn(Optional.of("ServiceA"));

        //when
        List<LettersCountSummary> result = service.getCountFor(date);

        //then
        assertThat(result).isNotEmpty()
            .hasSize(1)
            .usingFieldByFieldElementComparator()
            .containsExactlyInAnyOrder(new LettersCountSummary("ServiceA", 10));
    }

    @Test
    void should_map_empty_list_from_repo() {
        LocalDate date = LocalDate.of(2019, 4, 25);
        LocalTime timeFrom = LocalTime.parse("17:00");
        LocalTime timeTo = LocalTime.parse("16:00");

        //given
        given(repository.countByDate(formatDateTime(date.minusDays(1), timeFrom), formatDateTime(date, timeTo)))
            .willReturn(Collections.emptyList());

        //when
        List<LettersCountSummary> result = service.getCountFor(date);

        //then
        assertThat(result).isEmpty();
    }

    private LocalDateTime formatDateTime(LocalDate date, LocalTime time) {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.from(ZoneOffset.UTC));
        return zonedDateTime.toLocalDateTime();
    }

}
