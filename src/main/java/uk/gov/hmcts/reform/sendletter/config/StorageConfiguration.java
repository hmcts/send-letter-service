package uk.gov.hmcts.reform.sendletter.config;

import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.specialized.BlobLeaseClientBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.gov.hmcts.reform.sendletter.blob.BlobContainerClientProvider;
import uk.gov.hmcts.reform.sendletter.blob.LeaseClientProvider;


@Configuration
public class StorageConfiguration {

    @Bean
    public BlobServiceClient getStorageClient(
        @Value("${storage.connection}") String connection) {
        return new BlobServiceClientBuilder()
                .connectionString(connection)
                .buildClient();
    }

    @Bean
    public BlobContainerClientProvider getBlobServiceClientProvider(
        @Value("${storage.connection}") String connection) {
        return containerName -> {
            var blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(connection)
                .buildClient();
            return blobServiceClient.getBlobContainerClient(containerName);
        };
    }

    @Bean
    public LeaseClientProvider getLeaseClientProvider() {
        return blobClient -> new BlobLeaseClientBuilder().blobClient(blobClient).buildClient();
    }
}
