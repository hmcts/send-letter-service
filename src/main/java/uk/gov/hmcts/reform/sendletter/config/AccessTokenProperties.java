package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;

import java.util.List;

/**
 * Configuration properties for access token.
 */
@ConfigurationProperties("accesstoken")
public class AccessTokenProperties {
    private List<TokenConfig> serviceConfig;

    /**
     * Get the service configuration.
     * @return The service configuration
     */
    public List<TokenConfig> getServiceConfig() {
        return serviceConfig;
    }

    /**
     * Set the service configuration.
     * @param serviceConfig The service configuration
     */
    public void setServiceConfig(List<TokenConfig> serviceConfig) {
        this.serviceConfig = serviceConfig;
    }

    /**
     * Get the container name for a service.
     * @param serviceName The name of the service
     * @return The validity of the token
     */
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

        /**
         * Get the name of the service.
         * @return The name of the service
         */
        public String getServiceName() {
            return serviceName;
        }

        /**
         * Set the name of the service.
         * @param serviceName The name of the service
         */
        public void setServiceName(String serviceName) {
            this.serviceName = serviceName;
        }

        /**
         * Get the validity of the token.
         * @return The validity of the token
         */
        public int getValidity() {
            return validity;
        }

        /**
         * Set the validity of the token.
         * @param validity The validity of the token
         */
        public void setValidity(int validity) {
            this.validity = validity;
        }

        /**
         * Get the new container name.
         * @return The new container name
         */
        public String getNewContainerName() {
            return newContainerName;
        }

        /**
         * Set the new container name.
         * @param newContainerName The new container name
         */
        public void setNewContainerName(String newContainerName) {
            this.newContainerName = newContainerName;
        }
    }
}
