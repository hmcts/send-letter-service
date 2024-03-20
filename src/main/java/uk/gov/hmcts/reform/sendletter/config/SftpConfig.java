package uk.gov.hmcts.reform.sendletter.config;

import net.schmizz.sshj.SSHClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;

import java.util.function.Supplier;

/**
 * Configuration for SFTP.
 */
@Configuration
public class SftpConfig {

    /**
     * Create a Supplier of SSHClient.
     * @return The Supplier of SSHClient
     */
    @Bean
    public Supplier<SSHClient> sshClient() {
        return SSHClient::new;
    }

    /**
     * Create a IFtpAvailabilityChecker.
     * @param downtimeFromHour The downtime from hour
     * @param downtimeToHour The downtime to hour
     * @return The IFtpAvailabilityChecker
     */
    @Bean
    public IFtpAvailabilityChecker ftpAvailabilityChecker(
        @Value("${ftp.downtime.from}") String downtimeFromHour,
        @Value("${ftp.downtime.to}") String downtimeToHour
    ) {
        return new FtpAvailabilityChecker(downtimeFromHour, downtimeToHour);
    }
}
