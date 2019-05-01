package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.LettersCountSummaryRepository;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;
import uk.gov.hmcts.reform.sendletter.model.out.LettersCountSummary;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Service
public class ReportsService {

    private static final String TEST_SERVICE = "BULKPRINT";

    private final LettersCountSummaryRepository repo;

    private final ServiceFolderMapping serviceFolderMapping;

    private final ZeroRowFiller zeroRowFiller;

    public ReportsService(
        LettersCountSummaryRepository repo,
        ServiceFolderMapping serviceFolderMapping,
        ZeroRowFiller zeroRowFiller
    ) {
        this.repo = repo;
        this.serviceFolderMapping = serviceFolderMapping;
        this.zeroRowFiller = zeroRowFiller;
    }

    public List<LettersCountSummary> getCountFor(LocalDate date) {
        LocalDateTime dateTimeFrom = formatDateTime(date.minusDays(1), LocalTime.of(17, 0, 0));
        LocalDateTime dateTimeTo = formatDateTime(date, LocalTime.of(16, 0, 0));

        return zeroRowFiller.fill(
            repo.countByDate(dateTimeFrom, dateTimeTo).stream().map(this::fromDb).collect(toList()))
            .stream()
            .filter(summary -> !summary.service.equals(TEST_SERVICE))
            .collect(toList()); //exclude test service
    }

    private LettersCountSummary fromDb(ServiceLettersCountSummary dbSummary) {
        return new LettersCountSummary(
            serviceFolderMapping.getFolderFor(dbSummary.getService()).orElse(null),
            dbSummary.getUploaded());
    }

    private LocalDateTime formatDateTime(LocalDate date, LocalTime time) {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.of(EUROPE_LONDON));
        String formattedDateTime = zonedDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
        return LocalDateTime.parse(formattedDateTime);
    }
}
