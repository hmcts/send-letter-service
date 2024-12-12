package uk.gov.hmcts.reform.sendletter.tasks;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

import java.util.Optional;

import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.SEND_LETTER_SERVICE_DELETE_LETTERS_CRON;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

/**
 * Deletes old letters from the database based on a hardcoded SQL query.
 * This task runs in all environments.
 */
@Component
public class DeleteOldLettersTask {

    private static final Logger logger = LoggerFactory.getLogger(DeleteOldLettersTask.class);

    private static final String TASK_NAME = "DeleteOldLetters";

    private final JdbcTemplate jdbcTemplate;

    private final LaunchDarklyClient launchDarklyClient;

    private final Integer BATCH_SIZE = 1000;

    private final String DELETE_QUERY = "SELECT batch_delete_letters(?);";

    /**
     * Constructor for the DeleteOldLettersTask.
     * @param jdbcTemplate The JDBC template for running SQL queries.
     */
    public DeleteOldLettersTask(JdbcTemplate jdbcTemplate, LaunchDarklyClient launchDarklyClient) {
        this.jdbcTemplate = jdbcTemplate;
        this.launchDarklyClient = launchDarklyClient;
    }

    /**
     * Run the task to delete old letters from the database.
     */
    @SchedulerLock(name = TASK_NAME)
    @Scheduled(cron = "0 0 17 ? * SAT", zone = EUROPE_LONDON) // Every Saturday at 5 PM
    //@Scheduled(cron = "0 */2 * * * ?", zone = EUROPE_LONDON) // Every 2 minutes for testing
    public void run() {
        logger.info("Starting {} task", TASK_NAME);
        if (launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)) {
            logger.info("Flag enabled. Task {} running", TASK_NAME);

            int totalRowsDeleted = 0;
            int rowsDeleted;

            try {
                do {
                    rowsDeleted = Optional.ofNullable(
                        jdbcTemplate.queryForObject(DELETE_QUERY, new Object[]{BATCH_SIZE}, Integer.class)
                    ).orElse(0);
                    totalRowsDeleted += rowsDeleted;
                    logger.info("Batch deleted: {} rows, Total deleted: {} rows", rowsDeleted, totalRowsDeleted);
                } while (rowsDeleted > 0);
                logger.info("{} task completed. Total rows deleted: {}", TASK_NAME, totalRowsDeleted);
            } catch (Exception e) {
                logger.error("Error occurred during {} task", TASK_NAME, e);
            }
        } else {
            logger.info("Flag disabled. Task {} did not run", TASK_NAME);
        }

    }
}
