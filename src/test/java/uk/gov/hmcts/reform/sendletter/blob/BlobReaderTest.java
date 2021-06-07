package uk.gov.hmcts.reform.sendletter.blob;

import com.azure.core.http.rest.PagedIterable;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
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
    private BlobManager blobManager;
    @Mock
    private LeaseClientProvider leaseClientProvider;
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

    private BlobReader blobReader;

    @BeforeEach
    void setUp() {
        createAccessTokenConfig();
        blobReader = new BlobReader(
                blobManager,
                accessTokenProperties,
                leaseClientProvider,
                20
        );
    }

    @Test
    void should_return_leased_blobinfo_when_lease_acquired() {
        given(blobManager.getContainerClient("encrypted"))
                .willReturn(blobContainerClient);
        given(blobContainerClient.listBlobs()).willReturn(mockedPagedIterable);
        given(mockedBlobItemFirst.getName()).willReturn("mockedBlobItemFirst");
        given(mockedBlobItemSecond.getName()).willReturn("mockedBlobItemSecond");
        given(mockedBlobItemThird.getName()).willReturn("mockedBlobItemThird");

        var blobItems = List.of(
                mockedBlobItemFirst,
                mockedBlobItemSecond,
                mockedBlobItemThird);

        var stream = blobItems.stream();

        given(mockedPagedIterable.stream())
                .willReturn(stream);

        given(leaseClientProvider.get(blobClient))
                .willReturn(blobLeaseClient);
        given(blobContainerClient.getBlobClient(anyString()))
                .willReturn(blobClient);
        String leasedId = "leased";
        given(blobLeaseClient.acquireLease(anyInt()))
                .willThrow(new RuntimeException("First already leased"))
                .willThrow(new RuntimeException("Second already leased"))
                .willReturn(leasedId);
        Optional<BlobInfo> mayBeBlobInfo = blobReader.retrieveBlobToProcess();
        assertThat(mayBeBlobInfo).isPresent();

        BlobInfo blobInfo = mayBeBlobInfo.get();
        assertThat(blobInfo.isLeased()).isTrue();

        verify(blobManager).getContainerClient("encrypted");
        verify(blobContainerClient, times(3))
                .getBlobClient(anyString());
        verify(leaseClientProvider, times(3))
                .get(blobClient);
        verify(blobLeaseClient, times(3))
                .acquireLease(20);
    }

    private void createAccessTokenConfig() {
        BiFunction<String, String, TokenConfig> tokenFunction = (type, container) -> {
            TokenConfig tokenConfig = new TokenConfig();
            tokenConfig.setValidity(300);
            tokenConfig.setContainerType(type);
            tokenConfig.setNewContainerName(container);
            tokenConfig.setServiceName("send_letter_service");
            return tokenConfig;
        };
        accessTokenProperties = new AccessTokenProperties();
        accessTokenProperties.setServiceConfig(
                of(
                    tokenFunction.apply("encrypted", "encrypted")
                )
        );
    }


}
