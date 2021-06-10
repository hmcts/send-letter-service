package uk.gov.hmcts.reform.sendletter.blob;

import com.azure.storage.blob.BlobContainerClient;

public interface BlobContainerClientProvider {
    BlobContainerClient get(String containerName);
}
