package uk.gov.hmcts.reform.sendletter.config;

import net.schmizz.sshj.SSHClient;
import net.schmizz.sshj.transport.verification.HostKeyVerifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import uk.gov.hmcts.reform.sendletter.helper.FakeFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;

import java.security.PublicKey;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

@Configuration
public class SftpConfig {

    @Bean
    @Primary
    public Supplier<SSHClient> sshClient() {
        // Provide clients that do not verify
        // host name and key for local testing.
        return () -> {
            SSHClient client = new SSHClient();
            client.addHostKeyVerifier(new HostKeyVerifier() {
                @Override
                public boolean verify(String hostname, int port, PublicKey key) {
                    return true;
                }

                @Override
                public List<String> findExistingAlgorithms(String hostname, int port) {
                    return Collections.emptyList();
                }
            });
            return client;
        };
    }

    @Bean
    @Primary
    public IFtpAvailabilityChecker ftpAvailabilityChecker() {
        return new FakeFtpAvailabilityChecker();
    }
}
