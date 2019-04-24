package uk.gov.hmcts.reform.sendletter.services.ftp;

import net.schmizz.sshj.SSHClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.function.Function;

/**
 * Class to manage {@link SSHClient} with current state of service.
 * For backwards compatibility the overridden {@link SshClient#close()}
 * is written to mimic previous pseudo-complex usage in {@link FtpClient#runWith(Function)}
 */
public class SshClient extends SSHClient {

    private static final Logger logger = LoggerFactory.getLogger(SshClient.class);

    public SshClient() {
        super();
    }

    @Override
    public void close() {
        try {
            super.close();
        } catch (IOException exception) {
            logger.warn("Error closing ssh connection.", exception);
        }
    }
}
