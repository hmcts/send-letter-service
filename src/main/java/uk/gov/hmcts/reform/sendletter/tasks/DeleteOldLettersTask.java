package uk.gov.hmcts.reform.sendletter.tasks;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
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

    @Value("${delete-old-letters.batch-size:1000}")
    private int batchSize;

    @Value("${delete-old-letters.civil-general-applications-interval:6 years}")
    private String civilGeneralApplicationsInterval;

    @Value("${delete-old-letters.civil-service-interval:6 years}")
    private String civilServiceInterval;

    @Value("${delete-old-letters.cmc-claim-store-interval:2 years}")
    private String cmcClaimStoreInterval;

    @Value("${delete-old-letters.divorce-frontend-interval:3 months}")
    private String divorceFrontendInterval;

    @Value("${delete-old-letters.finrem-case-orchestration-interval:3 months}")
    private String finremCaseOrchestrationInterval;

    @Value("${delete-old-letters.finrem-document-generator-interval:3 months}")
    private String finremDocumentGeneratorInterval;

    @Value("${delete-old-letters.fpl-case-service-interval:2 years}")
    private String fplCaseServiceInterval;

    @Value("${delete-old-letters.nfdiv-case-api-interval:3 months}")
    private String nfdivCaseApiInterval;

    @Value("${delete-old-letters.prl-cos-api-interval:18 years}")
    private String prlCosApiInterval;

    @Value("${delete-old-letters.probate-backend-interval:1 year}")
    private String probateBackendInterval;

    @Value("${delete-old-letters.send-letter-tests-interval:2 years}")
    private String sendLetterTestsInterval;

    @Value("${delete-old-letters.sscs-interval:3 months}")
    private String sscsInterval;

    // See V028__Add_batch_delete_letters_function.sql for sql function
    private final String deleteQuery = """
       SELECT batch_delete_letters(
          ?, -- Batch size
          CAST(? AS INTERVAL), -- civil_general_applications_interval
          CAST(? AS INTERVAL), -- civil_service_interval
          CAST(? AS INTERVAL), -- cmc_claim_store_interval
          CAST(? AS INTERVAL), -- divorce_frontend_interval
          CAST(? AS INTERVAL), -- finrem_case_orchestration_interval
          CAST(? AS INTERVAL), -- finrem_document_generator_interval
          CAST(? AS INTERVAL), -- fpl_case_service_interval
          CAST(? AS INTERVAL), -- nfdiv_case_api_interval
          CAST(? AS INTERVAL), -- prl_cos_api_interval
          CAST(? AS INTERVAL), -- probate_backend_interval
          CAST(? AS INTERVAL), -- send_letter_tests_interval
          CAST(? AS INTERVAL)  -- sscs_interval
        );
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
     * Deletes old letters from the database in batches based on the batch_delete_letters query.
     */
    @SchedulerLock(name = TASK_NAME)
    @Scheduled(cron = "${delete-old-letters.cron:0 0 17 ? * SAT}", zone = EUROPE_LONDON) // default: Every Saturday at 5 PM
    public void run() {
        logger.info("Starting {} task", TASK_NAME);
        if (launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)) {
            logger.info("Flag enabled. Task {} running", TASK_NAME);

            int totalRowsDeleted = 0;
            int rowsDeleted;

            try {
                do {
                    rowsDeleted = Optional.ofNullable(
                        jdbcTemplate.queryForObject(
                            deleteQuery,
                            new Object[]{
                                batchSize,
                                civilGeneralApplicationsInterval, // civil_general_applications_interval
                                civilServiceInterval,             // civil_service_interval
                                cmcClaimStoreInterval,            // cmc_claim_store_interval
                                divorceFrontendInterval,          // divorce_frontend_interval
                                finremCaseOrchestrationInterval,  // finrem_case_orchestration_interval
                                finremDocumentGeneratorInterval,  // finrem_document_generator_interval
                                fplCaseServiceInterval,           // fpl_case_service_interval
                                nfdivCaseApiInterval,             // nfdiv_case_api_interval
                                prlCosApiInterval,                // prl_cos_api_interval
                                probateBackendInterval,           // probate_backend_interval
                                sendLetterTestsInterval,          // send_letter_tests_interval
                                sscsInterval                      // sscs_interval
                            },
                            Integer.class
                        )
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
