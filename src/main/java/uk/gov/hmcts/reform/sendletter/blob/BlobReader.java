package uk.gov.hmcts.reform.sendletter.blob;

import com.azure.storage.blob.BlobServiceClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.model.in.BlobInfo;

import java.util.Optional;

@Component
public class BlobReader {

    private static final Logger LOG = LoggerFactory.getLogger(BlobReader.class);
    private static final String SERVICE_NAME = "send_letter_service";

    private final BlobServiceClient blobServiceClient;
    private final AccessTokenProperties accessTokenProperties;
    private final LeaseClientProvider leaseClientProvider;
    private final int leaseTime;
    private final String sourceContainer;

    public BlobReader(
            BlobServiceClient blobServiceClient,
            AccessTokenProperties accessTokenProperties,
            LeaseClientProvider leaseClientProvider,
            @Value("${storage.leaseTime}") int leaseTime) {
        this.blobServiceClient =  blobServiceClient;
        this.accessTokenProperties = accessTokenProperties;
        this.leaseClientProvider = leaseClientProvider;
        this.leaseTime = leaseTime;
        this.sourceContainer = this.accessTokenProperties.getContainerName(SERVICE_NAME);
    }

    public Optional<BlobInfo> retrieveBlobToProcess() {
        LOG.info("About to read blob from container {}", sourceContainer);
        var containerClient = blobServiceClient.getBlobContainerClient(sourceContainer);

        return containerClient.listBlobs().stream()
                .map(blobItem ->
                    new BlobInfo(
                            containerClient.getBlobClient(blobItem.getName())
                    )
                )
                .filter(blobInfo -> {
                    this.acquireLease(blobInfo);
                    return blobInfo.isLeased();
                })
                .findFirst();

    }

    private void acquireLease(BlobInfo blobInfo) {
        try {
            var blobLeaseClient = leaseClientProvider.get(blobInfo.getBlobClient());
            String leaseId = blobLeaseClient.acquireLease(leaseTime);
            blobInfo.setLeaseId(leaseId);
        } catch (Exception e) {
            LOG.error("Unable to acquire lease for blob {}",
                    blobInfo.getBlobClient().getBlobName());
        }
    }
}
