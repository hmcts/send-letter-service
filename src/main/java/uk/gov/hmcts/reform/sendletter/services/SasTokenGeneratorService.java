package uk.gov.hmcts.reform.sendletter.services;

import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.sas.BlobContainerSasPermission;
import com.azure.storage.blob.sas.BlobServiceSasSignatureValues;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties.TokenConfig;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToGenerateSasTokenException;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;

@Configuration
@EnableConfigurationProperties(AccessTokenProperties.class)
@Service
public class SasTokenGeneratorService {

    private static final Logger LOG = LoggerFactory.getLogger(SasTokenGeneratorService.class);

    private final BlobServiceClient blobServiceClient;
    private final AccessTokenProperties accessTokenProperties;
    private static final String PERMISSION_WRITE_LIST = "wlr";

    public SasTokenGeneratorService(
            BlobServiceClient blobServiceClient,
            AccessTokenProperties accessTokenProperties
    ) {
        this.blobServiceClient = blobServiceClient;
        this.accessTokenProperties = accessTokenProperties;
    }

    public String getAccountUrl() {
        return blobServiceClient.getAccountUrl();
    }

    public String generateSasToken(String serviceName) {
        return generateSasToken(serviceName, "new");
    }

    public String generateSasToken(String serviceName, String containerType) {
        var config = getTokenConfigForService(serviceName, containerType);
        LOG.info("SAS Token request received for container '{}'", config.getNewContainerName());

        try {
            return blobServiceClient
                    .getBlobContainerClient(config.getNewContainerName())
                    .generateSas(createSharedAccessPolicy(config));
        } catch (Exception e) {
            throw new UnableToGenerateSasTokenException(e);
        }
    }

    public String getContainerName(String serviceName) {
        return getContainerName(serviceName, "new");
    }

    public String getContainerName(String serviceName, String containerType) {
        return getTokenConfigForService(serviceName, containerType).getNewContainerName();
    }

    private BlobServiceSasSignatureValues createSharedAccessPolicy(TokenConfig config) {

        return new BlobServiceSasSignatureValues(
                OffsetDateTime.now(ZoneOffset.UTC).plusSeconds(config.getValidity()),
                BlobContainerSasPermission.parse(PERMISSION_WRITE_LIST)
        );
    }

    private TokenConfig getTokenConfigForService(final String serviceName, final String containerType) {
        return accessTokenProperties.getServiceConfig().stream()
            .filter(tokenConfig -> (tokenConfig.getServiceName().equalsIgnoreCase(serviceName)
                && tokenConfig.getContainerType().equalsIgnoreCase(containerType)))
            .findFirst()
            .orElseThrow(
                    () -> new ServiceNotConfiguredException(
                            "No configuration found for service " + serviceName)
            );
    }

}
