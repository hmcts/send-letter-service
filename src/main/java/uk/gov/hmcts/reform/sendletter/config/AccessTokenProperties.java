package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;

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

    public String getContainerName(String serviceName) {
        return this.getServiceConfig().stream()
            .filter(tokenConfig -> tokenConfig.getServiceName().equalsIgnoreCase(serviceName))
            .findFirst()
            .orElseThrow(
                () -> new ServiceNotConfiguredException(
                    "No configuration found for service " + serviceName)
            ).getNewContainerName();
    }

    public static class TokenConfig {
        private String serviceName;
        private int validity;
        private String newContainerName;

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
    }
}
