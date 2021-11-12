package uk.gov.hmcts.reform.sendletter.helper;

import net.schmizz.sshj.SSHClient;
import net.schmizz.sshj.transport.verification.HostKeyVerifier;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.config.RetryConfig;
import uk.gov.hmcts.reform.sendletter.services.LocalSftpServer;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.util.ResourceLoader;

import java.security.PublicKey;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

import static java.util.Collections.singletonList;

public final class FtpHelper {

    // Prevent instantiation.
    private FtpHelper() {
    }

    // Instantiate an FtpClient with host key verification disabled,
    // so it will connect to a local ftp server without verifying the
    // server's public key.

    private static FtpClient getClient(int port, boolean verified) throws Exception {
        Supplier<SSHClient> s = () -> {
            SSHClient client = new SSHClient();
            client.addHostKeyVerifier(new HostKeyVerifier() {
                @Override
                public boolean verify(String hostname, int port, PublicKey key) {
                    return verified;
                }

                @Override
                public List<String> findExistingAlgorithms(String hostname, int port) {
                    return Collections.emptyList();
                }
            });
            return client;
        };
        RetryConfig config = new RetryConfig();
        return new FtpClient(s, getFtpConfig(port), config.retryTemplate(2, 2000));
    }

    public static FtpClient getFailingClient(int port) throws Exception {
        return getClient(port, false);
    }

    public static FtpClient getSuccessfulClient(int port) throws Exception {
        return getClient(port, true);
    }

    private static FtpConfigProperties getFtpConfig(int port) throws Exception {
        FtpConfigProperties p = new FtpConfigProperties();
        p.setHostname("localhost");
        p.setPort(port);
        p.setPublicKey(ResourceLoader.loadJson("keypair.pub"));
        p.setPrivateKey(ResourceLoader.loadJson("keypair"));
        p.setUsername("irrelevant");
        p.setFingerprint("SHA1:2Fo8c/96zv32xc8GZWbOGYOlRak=");
        p.setTargetFolder(LocalSftpServer.LETTERS_FOLDER_NAME);
        p.setReportsFolder(LocalSftpServer.REPORT_FOLDER_NAME);
        ServiceFolderMapping serviceFolderMapping = new ServiceFolderMapping();
        serviceFolderMapping.setService("bulkprint");
        serviceFolderMapping.setFolder("BULKPRINT");
        p.setServiceFolders(singletonList(serviceFolderMapping));
        return p;
    }
}
