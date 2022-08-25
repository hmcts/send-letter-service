package uk.gov.hmcts.reform.sendletter.services.ftp;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;

import java.util.Collection;
import java.util.Optional;

@Component
public class ServiceFolderMapping {

    private final FtpConfigProperties configProperties;

    public ServiceFolderMapping(FtpConfigProperties configProperties) {
        this.configProperties = configProperties;
    }

    public Collection<String> getFolders() {
        return configProperties.getServiceFolders().values();
    }

    public Collection<String> getServices() {
        return configProperties.getServiceFolders().keySet();
    }

    public Optional<String> getFolderFor(String serviceName) {
        return Optional.ofNullable(configProperties.getServiceFolders().get(serviceName));
    }
}
