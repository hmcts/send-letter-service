package uk.gov.hmcts.reform.sendletter.tasks;


import com.azure.core.util.Context;
import com.azure.storage.blob.models.BlobRequestConditions;
import com.azure.storage.blob.models.DeleteSnapshotsOptionType;
import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import net.schmizz.sshj.sftp.SFTPClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.blob.BlobReader;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.exception.LeaseIdNotPresentException;
import uk.gov.hmcts.reform.sendletter.model.in.BlobInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileToSend;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.io.IOException;
import java.time.ZoneId;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import static java.time.LocalDateTime.now;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Component
@ConditionalOnProperty(value = "scheduling.enabled", matchIfMissing = true)
public class UploadBlobsTask {

    private static final Logger logger     = LoggerFactory.getLogger(UploadBlobsTask.class);
    public static final  int    BATCH_SIZE = 10;
    public static final String SMOKE_TEST_LETTER_TYPE = "smoke_test";
    private static final String TASK_NAME = "UploadBlobs";

    private final PrintRepository         printRepository;
    private final FtpClient               ftp;
    private final IFtpAvailabilityChecker availabilityChecker;
    private final ServiceFolderMapping serviceFolderMapping;
    private final BlobReader           blobReader;

    public UploadBlobsTask(
        PrintRepository printRepository,
        FtpClient ftp,
        IFtpAvailabilityChecker availabilityChecker,
        ServiceFolderMapping serviceFolderMapping,
        BlobReader blobReader
    ) {
        this.printRepository = printRepository;
        this.ftp = ftp;
        this.availabilityChecker = availabilityChecker;
        this.serviceFolderMapping = serviceFolderMapping;
        this.blobReader =  blobReader;
    }

    @SchedulerLock(name = TASK_NAME)
    @Scheduled(fixedDelayString = "${tasks.upload-letters.interval-ms}")
    public void run() {
        logger.info("Started '{}' task ", TASK_NAME);

        if (!availabilityChecker.isFtpAvailable(now(ZoneId.of(EUROPE_LONDON)).toLocalTime())) {
            logger.info("Not processing '{}' task due to FTP downtime window", TASK_NAME);
        } else {
            if (printRepository.countByStatus(PrintStatus.NEW) > 0) {
                int uploadCount = processLetters();
                logger.info("Completed '{}' task. Uploaded {} letters", TASK_NAME, uploadCount);
            } else {
                logger.info("Completed '{}' task. No letters to upload.", TASK_NAME);
            }
        }
    }

    private int processLetters() {
        return ftp.runWith(client -> {
            var uploadCount = 0;
            for (var i = 0; i < BATCH_SIZE; i++) {
                Optional<BlobInfo> mayBeBlobInfo = blobReader.retrieveBlobToProcess();
                if (mayBeBlobInfo.isPresent()) {
                    var blobInfo = mayBeBlobInfo.get();
                    var blobClient = blobInfo.getBlobClient();
                    String blobName = blobClient.getBlobName();

                    var blobId = blobName.split("\\.")[0].split("_")[3];

                    Optional<Print> printBlob = printRepository.findById(UUID.fromString(blobId));
                    if (printBlob.isPresent()) {
                        var uploaded = false;
                        uploaded = processBlob(printBlob.get(), blobInfo, client);

                        if (uploaded) {
                            uploadCount++;
                        }
                    }
                } else {
                    break;
                }
            }
            return uploadCount;
        });
    }

    private boolean processBlob(Print print, BlobInfo blobInfo,  SFTPClient sftpClient) {

        Optional<String> serviceFolder = serviceFolderMapping.getFolderFor(print.getService());

        if (serviceFolder.isPresent()) {

            uploadBlob(print, blobInfo, serviceFolder.get(), sftpClient);

            print.setStatus(PrintStatus.UPLOADED);
            print.setSentToPrintAt(now());
            printRepository.saveAndFlush(print);

            return true;
        } else {
            logger.error("Folder for service {} not found. Skipping letter {}",
                print.getService(), print.getId());

            print.setStatus(PrintStatus.SKIPPED);
            printRepository.saveAndFlush(print);

            return false;
        }
    }

    private void uploadBlob(Print print, BlobInfo blobInfo, String folder,
                            SFTPClient sftpClient) {

        var blobClient = blobInfo.getBlobClient();
        var blobName = blobClient.getBlobName();

        try (var blobInputStream = blobClient.openInputStream()) {
            byte[] fileContent = blobInputStream.readAllBytes();

            var file = new FileToSend(
                blobName,
                fileContent,
                isSmokeTest(print)
            );

            ftp.upload(file, folder, sftpClient);

            logger.info(
                "Uploaded blob id: {}, file name: {}",
                print.getId(),
                file.filename
            );

            String leaseId = blobInfo.getLeaseId()
                .orElseThrow(() ->
                    new LeaseIdNotPresentException("Lease not present"));
            blobClient.deleteWithResponse(
                DeleteSnapshotsOptionType.INCLUDE,
                new BlobRequestConditions().setLeaseId(leaseId),
                null,
                Context.NONE);
            logger.info("Deleted blob : {}", blobName);
        } catch (IOException e) {
            logger.error("Error in uploading blob to ftp server : {}", blobName);
        }
    }

    private boolean isSmokeTest(Print print) {
        return Objects.equals(print.getType(), SMOKE_TEST_LETTER_TYPE);
    }
}
