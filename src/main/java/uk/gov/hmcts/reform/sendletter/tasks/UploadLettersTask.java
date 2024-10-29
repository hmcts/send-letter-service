package uk.gov.hmcts.reform.sendletter.tasks;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import net.schmizz.sshj.sftp.SFTPClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.services.LetterEventService;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileToSend;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Objects;
import java.util.Optional;

import static java.time.LocalDateTime.now;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

/**
 * Task to upload letters to SFTP.
 */
@Component
@ConditionalOnProperty(value = "scheduling.enabled", matchIfMissing = true)
public class UploadLettersTask {

    private static final Logger logger = LoggerFactory.getLogger(UploadLettersTask.class);
    public static final int BATCH_SIZE = 10;
    public static final String SMOKE_TEST_LETTER_TYPE = "smoke_test";
    private static final String TASK_NAME = "UploadLetters";
    public static final String INTERNATIONAL_FOLDER = "/International";

    private final LetterRepository repo;
    private final FtpClient ftp;
    private final IFtpAvailabilityChecker availabilityChecker;
    private final LetterEventService letterEventService;
    private final ServiceFolderMapping serviceFolderMapping;
    private final int dbPollDelay;

    /**
     * Constructor for the UploadLettersTask.
     * @param repo The letter repository
     * @param ftp The FTP client
     * @param availabilityChecker The FTP availability checker
     * @param letterEventService The service for letter event
     * @param serviceFolderMapping The service folder mapping
     * @param dbPollDelay The database poll delay
     */
    public UploadLettersTask(
        LetterRepository repo,
        FtpClient ftp,
        IFtpAvailabilityChecker availabilityChecker,
        LetterEventService letterEventService,
        ServiceFolderMapping serviceFolderMapping,
        @Value("${tasks.upload-letters.db-poll-delay}") int dbPollDelay
    ) {
        this.repo = repo;
        this.ftp = ftp;
        this.availabilityChecker = availabilityChecker;
        this.letterEventService = letterEventService;
        this.serviceFolderMapping = serviceFolderMapping;
        this.dbPollDelay = dbPollDelay;
    }

    /**
     * Run the task to upload letters to SFTP.
     */
    @SchedulerLock(name = TASK_NAME)
    @Scheduled(fixedDelayString = "${tasks.upload-letters.interval-ms}")
    public void run() {
        logger.info("Started '{}' task with db-poll-delay of {} ", TASK_NAME, dbPollDelay);

        if (!availabilityChecker.isFtpAvailable(now(ZoneId.of(EUROPE_LONDON)).toLocalTime())) {
            logger.info("Not processing '{}' task due to FTP downtime window", TASK_NAME);
        } else {
            if (repo.countByStatus(LetterStatus.Created) > 0) {
                int uploadCount = processLetters();
                logger.info("Completed '{}' task. Uploaded {} letters", TASK_NAME, uploadCount);
            } else {
                logger.info("Completed '{}' task. No letters to upload.", TASK_NAME);
            }
        }
    }

    /**
     * Process letters to upload.
     * @return The number of letters uploaded
     */
    private int processLetters() {
        return ftp.runWith(client -> {
            int uploadCount = 0;
            for (int i = 0; i < BATCH_SIZE; i++) {
                Optional<Letter> letterOpt
                        = repo.findFirstLetterCreated(LocalDateTime.now().minusMinutes(dbPollDelay));

                if (letterOpt.isPresent()) {
                    Letter letter = letterOpt.get();
                    logger.info("letterOpt, id: {}, service: {}, type: {}, additionalData: {}, createdAt: {}",
                        letter.getId(),
                        letter.getService(),
                        letter.getType(),
                        letter.getAdditionalData(),
                        letter.getCreatedAt()
                    );
                    try {
                        boolean uploaded = processLetter(letter, client);
                        if (uploaded) {
                            uploadCount++;
                        }
                    } catch (Exception ex) {
                        if (!(ex.getCause() instanceof IOException)) {
                            logger.info("Error uploading letter {}", letter.getId(), ex);
                            letterEventService.failLetterUpload(letter, ex);
                        }
                        break;
                    }
                } else {
                    break;
                }
            }

            return uploadCount;
        });
    }

    /**
     * Process a letter to upload.
     * @param letter The letter to upload
     * @param sftpClient The SFTP client
     * @return True if the letter was uploaded, otherwise false
     */
    private boolean processLetter(Letter letter, SFTPClient sftpClient) {

        Optional<String> serviceFolder = serviceFolderMapping.getFolderFor(letter.getService());

        if (serviceFolder.isPresent()) {
            String grabbedServiceFolder = serviceFolder.get();
            if (letter.getAdditionalData() != null
                && letter.getAdditionalData().has("isInternational")
                && letter.getAdditionalData().get("isInternational").asBoolean()) {
                grabbedServiceFolder = serviceFolder.get() + INTERNATIONAL_FOLDER;
            }
            logger.info("Service folder found for letter service: {}", grabbedServiceFolder);
            uploadLetter(letter, grabbedServiceFolder, sftpClient);
            letter.setStatus(LetterStatus.Uploaded);
            letter.setSentToPrintAt(now());
            repo.saveAndFlush(letter);
            return true;

        } else {
            logger.error("Folder for service {} not found. Skipping letter {}", letter.getService(), letter.getId());

            letter.setStatus(LetterStatus.Skipped);
            repo.saveAndFlush(letter);

            return false;
        }
    }

    /**
     * Upload a letter to SFTP.
     * @param letter The letter to upload
     * @param folder The folder to upload to
     * @param sftpClient The SFTP client
     */
    private void uploadLetter(Letter letter, String folder, SFTPClient sftpClient) {
        FileToSend file = new FileToSend(
            FileNameHelper.generateName(letter),
            letter.getFileContent(),
            isSmokeTest(letter)
        );

        ftp.upload(file, folder, sftpClient);

        logger.info(
            String.format(
                "Uploaded letter id: %s, checksum: %s, file name: %s, folder: %s, additional data: %s",
                letter.getId(),
                letter.getChecksum(),
                file.filename,
                folder,
                letter.getAdditionalData()

            )
        );
    }

    /**
     * Check if the letter is a smoke test.
     * @param letter The letter to check
     * @return True if the letter is a smoke test, otherwise false
     */
    private boolean isSmokeTest(Letter letter) {
        return Objects.equals(letter.getType(), SMOKE_TEST_LETTER_TYPE);
    }
}
