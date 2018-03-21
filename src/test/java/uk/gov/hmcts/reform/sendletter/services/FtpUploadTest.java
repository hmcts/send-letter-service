package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.io.Files;
import org.junit.Before;
import org.junit.Test;
import uk.gov.hmcts.reform.sendletter.MockSftpServer;
import uk.gov.hmcts.reform.sendletter.helper.FtpHelper;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.PdfDoc;
import uk.gov.hmcts.reform.slc.services.steps.sftpupload.FtpClient;

import java.io.File;
import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;

public class FtpUploadTest {

    @Before
    public void before() throws IOException {
    }

    @Test
    public void connects_to_ftp() throws Exception {
        try (MockSftpServer test = MockSftpServer.create()) {
            FtpHelper.getClient().testConnection();
        }
    }

    @Test
    public void uploads_file() throws Exception {
        PdfDoc doc = new PdfDoc("hello.txt", "world".getBytes());
        try (MockSftpServer server = MockSftpServer.create()) {
            FtpClient client = FtpHelper.getClient();
            client.upload(doc);
            File[] files = server.pdfFolder.listFiles();
            assertThat(files.length).isEqualTo(1);
            String content = new String(Files.toByteArray(files[0]));
            assertThat(content).isEqualTo("world");
        }
    }
}
