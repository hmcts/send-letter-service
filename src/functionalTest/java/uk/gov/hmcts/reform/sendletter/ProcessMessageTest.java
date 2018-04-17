package uk.gov.hmcts.reform.sendletter;

import net.schmizz.sshj.sftp.RemoteFile;
import net.schmizz.sshj.sftp.RemoteResourceInfo;
import net.schmizz.sshj.sftp.SFTPClient;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.junit.Test;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static org.apache.commons.lang.time.DateUtils.addMilliseconds;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;
import static org.assertj.core.util.DateUtil.now;

public class ProcessMessageTest extends FunctionalTestSuite {

    @Test
    public void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_one_document_with_even_pages()
        throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleLetterRequestJson("letter_single_document_two_pages")
        );

        try (SFTPClient sftp = getSftpClient()) {
            RemoteResourceInfo sftpFile = waitForFileOnSftp(sftp, letterId);

            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 2);
            }
        }
    }

    @Test
    public void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_one_document_with_odd_pages()
        throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleLetterRequestJson("letter_single_document_one_page.json")
        );

        try (SFTPClient sftp = getSftpClient()) {
            RemoteResourceInfo sftpFile = waitForFileOnSftp(sftp, letterId);

            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 2);
            }
        }
    }

    @Test
    public void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_two_documents_with_even_pages()
        throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleLetterRequestJson("letter_two_documents_even_pages.json")
        );

        try (SFTPClient sftp = getSftpClient()) {
            RemoteResourceInfo sftpFile = waitForFileOnSftp(sftp, letterId);

            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 4);
            }
        }
    }

    @Test
    public void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_two_documents_with_odd_pages()
        throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleLetterRequestJson("letter_two_documents_odd_pages.json")
        );

        try (SFTPClient sftp = getSftpClient()) {
            RemoteResourceInfo sftpFile = waitForFileOnSftp(sftp, letterId);

            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 4);
            }
        }
    }

    private void validatePdfFile(String letterId, SFTPClient sftp, RemoteResourceInfo sftpFile, int noOfDocuments)
        throws IOException {
        try (RemoteFile zipFile = sftp.open(sftpFile.getPath())) {
            PdfFile pdfFile = unzipFile(zipFile);
            assertThat(pdfFile.name).matches(getPdfFileNamePattern(letterId));

            PDDocument pdfDocument = PDDocument.load(pdfFile.content);
            assertThat(pdfDocument.getNumberOfPages()).isEqualTo(noOfDocuments);
        }
    }

    private RemoteResourceInfo waitForFileOnSftp(
        SFTPClient sftp, String letterId
    ) throws IOException, InterruptedException {
        Date waitUntil = addMilliseconds(now(), maxWaitForFtpFileInMs);

        List<RemoteResourceInfo> matchingFiles;

        while (!now().after(waitUntil)) {
            matchingFiles = sftp.ls(ftpTargetFolder, file -> file.getName().contains(letterId));

            if (matchingFiles.size() == 1) {
                return matchingFiles.get(0);
            } else if (matchingFiles.size() > 1) {
                String failMessage = String.format(
                    "Expected one file with name containing '%s'. Found %d",
                    letterId,
                    matchingFiles.size()
                );

                fail(failMessage);
            } else {
                Thread.sleep(1000);
            }
        }

        throw new AssertionError("The expected file didn't appear on SFTP server");
    }

    private PdfFile unzipFile(RemoteFile zipFile) throws IOException {
        try (ZipInputStream zipStream = getZipInputStream(zipFile)) {
            ZipEntry firstEntry = zipStream.getNextEntry();
            byte[] pdfContent = readAllBytes(zipStream);

            ZipEntry secondEntry = zipStream.getNextEntry();
            assertThat(secondEntry).as("second file in zip").isNull();

            String pdfName = firstEntry.getName();

            return new PdfFile(pdfName, pdfContent);
        }
    }

    private static class PdfFile {
        public final String name;
        public final byte[] content;

        public PdfFile(String name, byte[] content) {
            this.name = name;
            this.content = content;
        }
    }
}
