package uk.gov.hmcts.reform.sendletter.model.in;

import com.azure.storage.blob.BlobClient;

import java.util.Optional;

/**
 * This class represents the information of a blob.
 */
public class BlobInfo {
    private final BlobClient blobClient;
    private Optional<String> leaseId;

    /**
     * Constructor.
     *
     * @param blobClient the blob client
     */
    public BlobInfo(BlobClient blobClient) {
        this.blobClient = blobClient;
        this.leaseId = Optional.empty();
    }

    public BlobClient getBlobClient() {
        return blobClient;
    }

    public Optional<String> getLeaseId() {
        return leaseId;
    }

    public boolean isLeased() {
        return leaseId.isPresent();
    }

    public void setLeaseId(String leaseId) {
        this.leaseId = Optional.ofNullable(leaseId);
    }
}
