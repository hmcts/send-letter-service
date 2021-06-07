package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import uk.gov.hmcts.reform.sendletter.exception.ServiceConfigNotFoundException;

import java.util.List;

@ConfigurationProperties("accesstoken")
public class AccessTokenProperties {
    private List<TokenConfig> serviceConfig;

    public List<TokenConfig> getServiceConfig() {
        return serviceConfig;
    }

    public void setServiceConfig(List<TokenConfig> serviceConfig) {
        this.serviceConfig = serviceConfig;
    }

    public String getContainerForGivenType(String containerType) {
        return getServiceConfig().stream()
            .filter(tokenConfig -> tokenConfig.getContainerType().equals(containerType))
            .map(AccessTokenProperties.TokenConfig::getNewContainerName)
            .findFirst()
            .orElseThrow(() ->
                new ServiceConfigNotFoundException(
                    "No service configuration found for container " + containerType));
    }

    public static class TokenConfig {
        private String serviceName;
        private int validity;
        private String newContainerName;
        private String containerType;

        public String getServiceName() {
            return serviceName;
        }

        public void setServiceName(String serviceName) {
            this.serviceName = serviceName;
        }

        public int getValidity() {
            return validity;
        }

        public void setValidity(int validity) {
            this.validity = validity;
        }

        public String getNewContainerName() {
            return newContainerName;
        }

        public void setNewContainerName(String newContainerName) {
            this.newContainerName = newContainerName;
        }

        public String getContainerType() {
            return containerType;
        }

        public void setContainerType(String containerType) {
            this.containerType = containerType;
        }
    }
}
