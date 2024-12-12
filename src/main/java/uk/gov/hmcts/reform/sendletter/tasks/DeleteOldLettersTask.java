package uk.gov.hmcts.reform.sendletter.tasks;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

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


    private static final String DELETE_QUERY = """
        WITH letters_to_delete AS (
        SELECT id -- selecting indexed id column is all we need. also is faster then doing 'select *'
        FROM letters
        WHERE created_at < (
        CASE
            WHEN service = 'civil_general_applications' THEN NOW() - INTERVAL '6 YEARS'
            WHEN service = 'civil_service' THEN NOW() - INTERVAL '6 YEARS'
            WHEN service = 'cmc_claim_store' THEN NOW() - INTERVAL '2 YEARS'
            WHEN service = 'divorce_frontend' THEN NOW() - INTERVAL '3 MONTHS'
            WHEN service = 'finrem_case_orchestration' THEN NOW() - INTERVAL '3 MONTHS'
            WHEN service = 'finrem_document_generator' THEN NOW() - INTERVAL '3 MONTHS'
            WHEN service = 'fpl_case_service' THEN NOW() - INTERVAL '2 YEARS'
            WHEN service = 'nfdiv_case_api' THEN NOW() - INTERVAL '3 MONTHS'
            WHEN service = 'prl_cos_api' THEN NOW() - INTERVAL '18 YEARS'
            WHEN service = 'probate_backend' THEN NOW() - INTERVAL '1 YEAR'
            WHEN service = 'send_letter_tests' THEN NOW() - INTERVAL '2 YEARS'
            WHEN service = 'sscs' THEN NOW() - INTERVAL '3 MONTHS'
            -- no default case
        END
        )
        -- For below don't delete old unprocessed / things that need investigating at some point
        AND status IN ('Posted', 'PostedLocally', 'Aborted', 'Skipped')
        ORDER BY created_at ASC -- Prioritize oldest rows first
        LIMIT 30000 -- Limit so it doesn't delete too many at once but needs to be high enough to make a difference
    )
    DELETE FROM letters
    USING letters_to_delete
    WHERE letters.id = letters_to_delete.id;
        """;

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
            try {
                int rowsDeleted = jdbcTemplate.update(DELETE_QUERY);
                logger.info("{} task completed. {} rows deleted", TASK_NAME, rowsDeleted);
            } catch (Exception e) {
                logger.error("Error occurred during {} task", TASK_NAME, e);
            }
        } else {
            logger.info("Flag disabled. Task {} did not run", TASK_NAME);
        }

    }
}
