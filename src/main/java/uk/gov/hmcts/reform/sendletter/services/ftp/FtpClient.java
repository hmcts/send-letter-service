package uk.gov.hmcts.reform.sendletter.services.ftp;

import net.schmizz.sshj.SSHClient;
import net.schmizz.sshj.sftp.RemoteResourceInfo;
import net.schmizz.sshj.sftp.SFTPClient;
import net.schmizz.sshj.sftp.SFTPFileTransfer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;
import uk.gov.hmcts.reform.sendletter.exception.FtpException;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.model.InMemoryDownloadedFile;
import uk.gov.hmcts.reform.sendletter.model.Report;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Locale;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.stream.Collectors.toList;

@Component
@EnableConfigurationProperties(FtpConfigProperties.class)
public class FtpClient {

    private static final Logger logger = LoggerFactory.getLogger(FtpClient.class);

    private final FtpConfigProperties configProperties;

    private final Supplier<SSHClient> sshClientSupplier;

    private final AppInsights insights;

    // region constructor
    public FtpClient(
        Supplier<SSHClient> sshClientSupplier,
        FtpConfigProperties configProperties,
        AppInsights insights
    ) {
        this.sshClientSupplier = sshClientSupplier;
        this.configProperties = configProperties;
        this.insights = insights;
    }
    // endregion

    public void upload(FileToSend file, String serviceFolder, SFTPClient sftpClient) {
        String folder = file.isSmokeTest
            ? configProperties.getSmokeTestTargetFolder()
            : String.join("/", configProperties.getTargetFolder(), serviceFolder);

        String path = String.join("/", folder, file.getName());
        Instant start = Instant.now();
        boolean isSuccess = false;

        try {
            sftpClient.getFileTransfer().upload(file, path);

            logger.info(
                "File uploaded. Time: {}, Size: {}, Folder: {}",
                ChronoUnit.MILLIS.between(start, Instant.now()) + "ms",
                file.content.length / 1024 + "KB",
                serviceFolder
            );

            isSuccess = true;
        } catch (IOException exc) {
            logger.error("Error uploading file. Path: {}", path, exc);
            throw new FtpException("Unable to upload file.", exc);
        } finally {
            insights.trackFtpUpload(Duration.between(start, Instant.now()), isSuccess);
        }
    }

    /**
     * Downloads ALL files from reports directory.
     */
    public List<Report> downloadReports() {
        Instant now = Instant.now();

        return runWith(sftp -> {
            boolean isSuccess = false;

            try {
                SFTPFileTransfer transfer = sftp.getFileTransfer();

                List<Report> reports = sftp.ls(configProperties.getReportsFolder())
                    .stream()
                    .filter(this::isReportFile)
                    .map(file -> {
                        InMemoryDownloadedFile inMemoryFile = new InMemoryDownloadedFile();
                        try {
                            transfer.download(file.getPath(), inMemoryFile);
                            return new Report(file.getPath(), inMemoryFile.getBytes());
                        } catch (IOException exc) {
                            throw new FtpException("Unable to download file " + file.getName(), exc);
                        }
                    })
                    .collect(toList());

                isSuccess = true;

                return reports;
            } catch (IOException exc) {
                throw new FtpException("Error while downloading reports", exc);
            } finally {
                insights.trackFtpReportDownload(Duration.between(now, Instant.now()), isSuccess);
            }
        });
    }

    public void deleteReport(String reportPath) {
        Instant now = Instant.now();

        runWith(sftp -> {
            boolean isSuccess = false;

            try {
                sftp.rm(reportPath);

                isSuccess = true;

                return null;
            } catch (Exception exc) {
                throw new FtpException("Error while deleting report: " + reportPath, exc);
            } finally {
                insights.trackFtpReportDeletion(Duration.between(now, Instant.now()), isSuccess);
            }
        });
    }

    public List<FileInfo> listLetters(String serviceFolder) {
        return runWith(sftp -> {
            try {
                String path = String.join("/", configProperties.getTargetFolder(), serviceFolder);
                return sftp
                    .ls(path)
                    .stream()
                    .map(it -> new FileInfo(
                            it.getPath(),
                            Instant.ofEpochSecond(it.getAttributes().getMtime())
                        )
                    )
                    .collect(toList());
            } catch (Exception exc) {
                throw new FtpException("Error while listing files for: " + serviceFolder, exc);
            }
        });
    }

    public void deleteFile(String filePath) {
        runWith(sftp -> {
            try {
                sftp.rm(filePath);
                return null;
            } catch (Exception exc) {
                throw new FtpException("Error while deleting file: " + filePath, exc);
            }
        });
    }

    public void testConnection() {
        runWith(sftpClient -> null);
    }

    public <T> T runWith(Function<SFTPClient, T> action) {
        SSHClient ssh = null;

        try {
            ssh = sshClientSupplier.get();

            ssh.addHostKeyVerifier(configProperties.getFingerprint());
            ssh.connect(configProperties.getHostname(), configProperties.getPort());

            ssh.authPublickey(
                configProperties.getUsername(),
                ssh.loadKeys(
                    configProperties.getPrivateKey(),
                    configProperties.getPublicKey(),
                    null
                )
            );

            try (SFTPClient sftp = ssh.newSFTPClient()) {
                return action.apply(sftp);
            }
        } catch (IOException exc) {
            throw new FtpException("Unable to upload file.", exc);
        } finally {
            try {
                if (ssh != null) {
                    ssh.disconnect();
                }
            } catch (IOException e) {
                logger.warn("Error closing ssh connection.", e);
            }
        }
    }

    private boolean isReportFile(RemoteResourceInfo resourceInfo) {
        return resourceInfo.isRegularFile()
            && resourceInfo.getName().toLowerCase(Locale.getDefault()).endsWith(".csv");
    }

}
