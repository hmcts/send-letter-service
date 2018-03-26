package uk.gov.hmcts.reform.sendletter.tasks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.ZonedDateTime;

public class FailedToPrintTask {
    private static final LocalTime STALE_CUT_OFF_TIME = LocalTime.of(17, 0, 0);

    private static final Logger logger = LoggerFactory.getLogger(FailedToPrintTask.class);

    private final LetterRepository repo;
    private final AppInsights insights;

    @Autowired
    public FailedToPrintTask(LetterRepository repo, AppInsights insights) {
        this.repo = repo;
        this.insights = insights;
    }

    public void run() {
        logger.trace("Running job");

        Timestamp staleCutOff = Timestamp.from(
            ZonedDateTime.now()
                .minusDays(1)
                .with(STALE_CUT_OFF_TIME)
                .toInstant()
        );

        repo.findByStateAndSentToPrintAtBeforeAndIsFailedFalse(LetterState.Uploaded, staleCutOff)
            .forEach(insights::trackNotPrintedLetter);
    }
}
