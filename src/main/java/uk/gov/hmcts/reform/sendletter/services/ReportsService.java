package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.LettersCountSummaryRepository;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;
import uk.gov.hmcts.reform.sendletter.model.out.LettersCountSummary;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang.StringUtils.isNotBlank;

@Service
public class ReportsService {

    private static final String TEST_SERVICE = "BULKPRINT";

    private final LettersCountSummaryRepository repo;

    private final ServiceFolderMapping serviceFolderMapping;

    private final ZeroRowFiller zeroRowFiller;

    private final String timeFromHour;

    private final String timeToHour;

    public ReportsService(
        LettersCountSummaryRepository repo,
        ServiceFolderMapping serviceFolderMapping,
        ZeroRowFiller zeroRowFiller,
        @Value("${ftp.downtime.from}") String downtimeFromHour,
        @Value("${ftp.downtime.to}") String downtimeToHour
    ) {
        this.repo = repo;
        this.serviceFolderMapping = serviceFolderMapping;
        this.zeroRowFiller = zeroRowFiller;
        this.timeFromHour = downtimeToHour;
        this.timeToHour = downtimeFromHour;
    }

    public List<LettersCountSummary> getCountFor(LocalDate date) {
        LocalDateTime dateTimeFrom = formatDateTime(date.minusDays(1), LocalTime.parse(timeFromHour));
        LocalDateTime dateTimeTo = formatDateTime(date, LocalTime.parse(timeToHour));

        return zeroRowFiller.fill(
            repo.countByDate(dateTimeFrom, dateTimeTo).stream().map(this::fromDb).collect(toList()))
            .stream()
            .filter(
                summary -> isNotBlank(summary.service) && !summary.service.equals(TEST_SERVICE)
            ) //excludes nulls, empty values and test service
            .collect(toList());
    }

    private LettersCountSummary fromDb(ServiceLettersCountSummary dbSummary) {
        return new LettersCountSummary(
            serviceFolderMapping.getFolderFor(dbSummary.getService()).orElse(null),
            dbSummary.getUploaded());
    }

    private LocalDateTime formatDateTime(LocalDate date, LocalTime time) {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.from(ZoneOffset.UTC));
        return zonedDateTime.toLocalDateTime();
    }
}
