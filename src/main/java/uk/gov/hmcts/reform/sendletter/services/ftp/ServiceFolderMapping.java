package uk.gov.hmcts.reform.sendletter.services.ftp;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties.ServiceNameFolderMapping;

import java.util.Collection;
import java.util.Optional;

import static java.util.stream.Collectors.toList;

@Component
public class ServiceFolderMapping {

    private final FtpConfigProperties configProperties;

    public ServiceFolderMapping(FtpConfigProperties configProperties) {
        this.configProperties = configProperties;
    }

    public Collection<String> getFolders() {
        return configProperties.getServicesConfig()
            .stream()
            .map(ServiceNameFolderMapping::getFolder).collect(toList());
    }

    public Optional<String> getFolderFor(String serviceName) {
        return configProperties.getServicesConfig()
            .stream().filter(config -> config.getDisplayName().equals(serviceName))
            .map(ServiceNameFolderMapping::getFolder)
            .findFirst();
    }
}
