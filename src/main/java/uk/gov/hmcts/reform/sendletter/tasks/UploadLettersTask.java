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
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
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
import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.FACT_1593_INTERNATIONAL_POST_FLAG;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Component
@ConditionalOnProperty(value = "scheduling.enabled", matchIfMissing = true)
public class UploadLettersTask {

    private static final Logger logger = LoggerFactory.getLogger(UploadLettersTask.class);
    public static final int BATCH_SIZE = 10;
    public static final String SMOKE_TEST_LETTER_TYPE = "smoke_test";
    private static final String TASK_NAME = "UploadLetters";
    private static final String INTERNATIONAL_FOLDER = "/international";

    private final LetterRepository repo;
    private final FtpClient ftp;
    private final IFtpAvailabilityChecker availabilityChecker;
    private final LetterEventService letterEventService;
    private final ServiceFolderMapping serviceFolderMapping;
    private final LaunchDarklyClient launchDarklyClient;
    private final int dbPollDelay;

    public UploadLettersTask(
        LetterRepository repo,
        FtpClient ftp,
        IFtpAvailabilityChecker availabilityChecker,
        LetterEventService letterEventService,
        ServiceFolderMapping serviceFolderMapping,
        LaunchDarklyClient launchDarklyClient,
        @Value("${tasks.upload-letters.db-poll-delay}") int dbPollDelay
    ) {
        this.repo = repo;
        this.ftp = ftp;
        this.availabilityChecker = availabilityChecker;
        this.letterEventService = letterEventService;
        this.serviceFolderMapping = serviceFolderMapping;
        this.launchDarklyClient = launchDarklyClient;
        this.dbPollDelay = dbPollDelay;
    }

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

    private int processLetters() {
        return ftp.runWith(client -> {
            int uploadCount = 0;

            for (int i = 0; i < BATCH_SIZE; i++) {
                Optional<Letter> letterOpt
                        = repo.findFirstLetterCreated(LocalDateTime.now().minusMinutes(dbPollDelay));

                if (letterOpt.isPresent()) {
                    Letter letter = letterOpt.get();
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

    private boolean processLetter(Letter letter, SFTPClient sftpClient) {

        Optional<String> serviceFolder = serviceFolderMapping.getFolderFor(letter.getService());

        if (serviceFolder.isPresent()) {
            String grabbedServiceFolder = serviceFolder.get();
            if (launchDarklyClient.isFeatureEnabled(FACT_1593_INTERNATIONAL_POST_FLAG)) {
                if (letter.getAdditionalData() != null
                    && letter.getAdditionalData().has("isInternational")
                    && letter.getAdditionalData().get("isInternational").asBoolean()) {
                    grabbedServiceFolder = serviceFolder.get() + INTERNATIONAL_FOLDER;
                }
            }
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

    private boolean isSmokeTest(Letter letter) {
        return Objects.equals(letter.getType(), SMOKE_TEST_LETTER_TYPE);
    }
}
