package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import com.google.common.io.Resources;
import net.schmizz.sshj.SSHClient;
import org.junit.Before;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;
import uk.gov.hmcts.reform.sendletter.MockSftpServer;
import uk.gov.hmcts.reform.slc.config.FtpConfigProperties;
import uk.gov.hmcts.reform.slc.logging.AppInsights;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.PdfDoc;
import uk.gov.hmcts.reform.slc.services.steps.sftpupload.FtpClient;

import java.io.File;
import java.io.IOException;
import java.util.function.Supplier;

import static org.assertj.core.api.Assertions.assertThat;

public class FTPUploadTest {

    final String subFolder = "moj";
    File testRootFolder;
    File testFolder;

    @Before
    public void before() throws IOException {
        TemporaryFolder tmp = new TemporaryFolder();
        tmp.create();
        testRootFolder = tmp.getRoot();
        testFolder = new File(tmp.getRoot(), subFolder);
        testFolder.mkdir();
    }

    @Test
    public void connects_to_ftp() throws Exception {
        try (MockSftpServer test = new MockSftpServer(testFolder)) {
            getFtpClient().testConnection();
        }
    }

    @Test
    public void uploads_file() throws Exception {
        PdfDoc doc = new PdfDoc("hello.txt", "world".getBytes());
        try (MockSftpServer test = new MockSftpServer(testRootFolder)) {
            FtpClient client = getFtpClient();
            client.upload(doc);
            File[] files = testFolder.listFiles();
            assertThat(files.length).isEqualTo(1);
            String content = new String(Files.toByteArray(files[0]));
            assertThat(content).isEqualTo("world");
        }
    }

    private FtpClient getFtpClient() throws IOException {
        AppInsights insights = Mockito.mock(AppInsights.class);
        Supplier<SSHClient> s = () -> {
            SSHClient client = new SSHClient();
            client.addHostKeyVerifier((a, b, c) -> true);
            return client;
        };
        return new FtpClient(s, getFtpConfig(), insights);
    }

    private FtpConfigProperties getFtpConfig() throws IOException {
        FtpConfigProperties p = new FtpConfigProperties();
        p.setHostname("localhost");
        p.setPort(MockSftpServer.port);
        p.setPublicKey(Resources.toString(Resources.getResource("keypair.pub"), Charsets.UTF_8));
        p.setPrivateKey(Resources.toString(Resources.getResource("keypair"), Charsets.UTF_8));
        p.setUsername("irrelevant");
        p.setFingerprint("SHA1:2Fo8c/96zv32xc8GZWbOGYOlRak=");
        p.setTargetFolder(subFolder);
        return p;
    }
}
