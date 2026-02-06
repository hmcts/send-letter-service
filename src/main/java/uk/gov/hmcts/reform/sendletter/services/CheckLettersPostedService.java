package uk.gov.hmcts.reform.sendletter.services;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.config.ReportsServiceConfig;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.entity.Report;
import uk.gov.hmcts.reform.sendletter.entity.ReportRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.out.CheckPostedTaskResponse;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class CheckLettersPostedService {

    private final StaleLetterService staleLetterService;
    private final LetterService letterService;
    private final LetterActionService letterActionService;
    private final ReportRepository reportRepository;
    private final ReportsServiceConfig reportsServiceConfig;

    public CheckPostedTaskResponse checkLetters() {
        int count = 0;
        List<BasicLetterInfo> letters = staleLetterService.getStaleLetters(
            List.of(LetterStatus.Uploaded),
            LocalDateTime.now(ZoneOffset.UTC).minusDays(7)
        );
        for (BasicLetterInfo letter : letters) {
            Optional<String> reportCode = calculateReportCode(letter);
            if (reportCode.isPresent()) {
                LocalDate printDate = letter.getSentToPrintAt().toLocalDate();
                Optional<Report> report =
                    reportRepository.findFirstByServiceAndReportDate(letter.getService(), printDate);
                if (report.isEmpty()) {
                    // We have a letter that's older than 7 days, and for which no report
                    // for its specific service was received/processed for that date.
                    letterActionService.markLetterAsNoReportAborted(letter.getId());
                }

            } else {
                log.warn("Unable to determine report code for service {}", letter.getService());
            }
        }
        return new CheckPostedTaskResponse(count);
    }

    private Optional<String> calculateReportCode(BasicLetterInfo letter) {
        try {
            uk.gov.hmcts.reform.sendletter.model.out.LetterStatus status =
                letterService.getStatus(letter.getId(), Boolean.TRUE.toString(), Boolean.FALSE.toString());
            return Optional.ofNullable(reportsServiceConfig.getReportCode(letter.getService(), status));
        } catch (LetterNotFoundException e) {
            log.warn("Letter not found for id {} during posted check", letter.getId());
        }
        return Optional.empty();
    }

}
