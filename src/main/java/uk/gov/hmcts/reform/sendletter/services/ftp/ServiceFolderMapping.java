package uk.gov.hmcts.reform.sendletter.services.ftp;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;

import java.util.Collection;
import java.util.Optional;

/**
 * This class represents the service folder mapping.
 */
@Component
public class ServiceFolderMapping {

    private final FtpConfigProperties configProperties;

    /**
     * Constructor.
     *
     * @param configProperties the config properties
     */
    public ServiceFolderMapping(FtpConfigProperties configProperties) {
        this.configProperties = configProperties;
    }

    /**
     * Gets folders.
     *
     * @return the folders
     */
    public Collection<String> getFolders() {
        return configProperties.getServiceFolders().values();
    }

    /**
     * Gets folder for.
     *
     * @param serviceName the service name
     * @return the folder for
     */
    public Optional<String> getFolderFor(String serviceName) {
        return Optional.ofNullable(configProperties.getServiceFolders().get(serviceName));
    }
}
