package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

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

    private List<Mapping> serviceFolders;

    public static class Mapping {

        private String service;
        private String folder;

        // region constructor, getters and setters
        public Mapping(String service, String folder) {
            this.service = service;
            this.folder = folder;
        }

        public Mapping() {
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
        this.privateKey = privateKey;
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

    public List<Mapping> getServiceFolders() {
        return serviceFolders;
    }

    public void setServiceFolders(List<Mapping> serviceFolders) {
        this.serviceFolders = serviceFolders;
    }
}
