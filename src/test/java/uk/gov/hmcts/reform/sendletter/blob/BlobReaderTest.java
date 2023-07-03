package uk.gov.hmcts.reform.sendletter.blob;

import com.azure.core.http.rest.PagedIterable;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.models.BlobItem;
import com.azure.storage.blob.specialized.BlobLeaseClient;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties.TokenConfig;
import uk.gov.hmcts.reform.sendletter.model.in.BlobInfo;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;

import static java.util.List.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class BlobReaderTest {
    @Mock
    private BlobServiceClient blobServiceClient;
    private AccessTokenProperties accessTokenProperties;
    @Mock
    private BlobContainerClient blobContainerClient;
    @Mock
    private PagedIterable<BlobItem> mockedPagedIterable;
    @Mock
    private BlobItem mockedBlobItemFirst;
    @Mock
    private BlobItem mockedBlobItemSecond;
    @Mock
    private BlobItem mockedBlobItemThird;
    @Mock
    private BlobLeaseClient blobLeaseClient;
    @Mock
    private BlobClient blobClient;

    @BeforeEach
    void setUp() {
        createAccessTokenConfig();
    }

    private void createAccessTokenConfig() {
        BiFunction<String, String, TokenConfig> tokenFunction = (service, container) -> {
            TokenConfig tokenConfig = new TokenConfig();
            tokenConfig.setValidity(300);
            tokenConfig.setNewContainerName(container);
            tokenConfig.setServiceName(service);
            return tokenConfig;
        };
        accessTokenProperties = new AccessTokenProperties();
        accessTokenProperties.setServiceConfig(
                of(
                    tokenFunction.apply("send_letter_service", "encrypted")
                )
        );
    }


}
