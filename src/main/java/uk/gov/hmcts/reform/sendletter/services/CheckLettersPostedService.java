package uk.gov.hmcts.reform.sendletter.services;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.config.ReportsServiceConfig;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.entity.ReportRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.out.CheckPostedTaskResponse;

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
            try {
                if (!reportExistsForLetter(letter)) {
                    count += letterActionService.markLetterAsNoReportAborted(letter.getId());
                }
            } catch (LetterNotFoundException e) {
                log.warn("Letter not found for id {} during posted check", letter.getId());
            }
        }
        return new CheckPostedTaskResponse(count);
    }

    private boolean reportExistsForLetter(BasicLetterInfo letter) {
        uk.gov.hmcts.reform.sendletter.model.out.LetterStatus status =
            letterService.getStatus(letter.getId(), Boolean.TRUE.toString(), Boolean.FALSE.toString());
        String reportCode = reportsServiceConfig.getReportCode(letter.getService(), status);
        if (reportCode != null) {
            return reportRepository.findFirstByReportCodeAndReportDateAndIsInternational(
                reportCode,
                letter.getSentToPrintAt().toLocalDate(),
                calculateIsInternational(status)
            ).isPresent();
        }
        log.warn("Unable to determine report code for service {}, assuming no report.", letter.getService());
        return false;
    }

    private boolean calculateIsInternational(final uk.gov.hmcts.reform.sendletter.model.out.LetterStatus status) {
        return Optional.ofNullable(status.additionalData)
            .map(m -> m.get("isInternational"))
            .map(Object::toString) // probably unnecessary
            .map(Boolean::valueOf)
            .orElse(false);
    }

}
