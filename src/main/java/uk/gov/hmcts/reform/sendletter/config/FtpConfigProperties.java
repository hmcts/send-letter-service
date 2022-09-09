package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toMap;

@ConfigurationProperties(prefix = "ftp")
public class FtpConfigProperties {

    private String hostname;

    private int port;

    private String fingerprint;

    private String username;

    private String publicKey;

    private String privateKey;

    private String targetFolder;

    private String smokeTestTargetFolder;

    private String reportsFolder;

    private Map<String, String> serviceFolders;

    public static class ServiceFolderMapping {

        private String service;
        private String folder;
        private boolean enabled = true;

        public ServiceFolderMapping() {
            // Spring needs it.
        }

        public String getService() {
            return service;
        }

        public void setService(String service) {
            this.service = service;
        }

        public String getFolder() {
            return folder;
        }

        public void setFolder(String folder) {
            this.folder = folder;
        }

        public boolean isEnabled() {
            return enabled;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        // endregion
    }

    public String getHostname() {
        return hostname;
    }

    public void setHostname(String hostname) {
        this.hostname = hostname;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getFingerprint() {
        return fingerprint;
    }

    public void setFingerprint(String fingerprint) {
        this.fingerprint = fingerprint;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPublicKey() {
        return publicKey;
    }

    public void setPublicKey(String publicKey) {
        this.publicKey = publicKey;
    }

    public String getPrivateKey() {
        return privateKey;
    }

    public void setPrivateKey(String privateKey) {
        String wrapper = "-----";

        // this is done so private key is loaded to library without throwing an exception
        // causing application to crash.
        // origin: platform specific. noticed only when building aks dockerised platform
        // by definition private key must be wrapped with newlines
        this.privateKey = privateKey == null ? privateKey : privateKey
            .replace(wrapper + " ", wrapper + "\n")
            .replace(" " + wrapper, "\n" + wrapper);
    }

    public String getTargetFolder() {
        return targetFolder;
    }

    public void setTargetFolder(String targetFolder) {
        this.targetFolder = targetFolder;
    }

    public String getSmokeTestTargetFolder() {
        return smokeTestTargetFolder;
    }

    public void setSmokeTestTargetFolder(String smokeTestTargetFolder) {
        this.smokeTestTargetFolder = smokeTestTargetFolder;
    }

    public String getReportsFolder() {
        return reportsFolder;
    }

    public void setReportsFolder(String reportsFolder) {
        this.reportsFolder = reportsFolder;
    }

    public Map<String, String> getServiceFolders() {
        return serviceFolders;
    }

    public void setServiceFolders(List<ServiceFolderMapping> serviceFolders) {
        this.serviceFolders = serviceFolders
            .stream()
            .filter(ServiceFolderMapping::isEnabled)
            .collect(toMap(ServiceFolderMapping::getService, ServiceFolderMapping::getFolder));
    }
}
