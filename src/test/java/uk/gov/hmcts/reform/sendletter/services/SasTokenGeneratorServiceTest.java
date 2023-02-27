package uk.gov.hmcts.reform.sendletter.services;

import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.common.StorageSharedKeyCredential;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToGenerateSasTokenException;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
class SasTokenGeneratorServiceTest {

    private SasTokenGeneratorService sasTokenGeneratorService;

    @BeforeEach
    void beforeEach() {
        AccessTokenProperties accessTokenProperties = getAccessTokenProperties();
        StorageSharedKeyCredential storageSharedKeyCredentials = new StorageSharedKeyCredential(
            "testAccount",
            "testkey"
        );
        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
            .credential(storageSharedKeyCredentials)
            .endpoint("http://test.account")
            .buildClient();

        sasTokenGeneratorService = new SasTokenGeneratorService(
          blobServiceClient,
          accessTokenProperties
        );

    }

    @Test
    void should_return_sas_token_when_service_is_configured() {
        String token = sasTokenGeneratorService.generateSasToken("sscs");
        Map<String, String> tokenData = Arrays.stream(token.split("&"))
            .map(data -> {
                String[] split = data.split("=");
                return Map.entry(split[0], split[1]);
            })
            .collect(Collectors.toMap(Map.Entry::getKey,
                Map.Entry::getValue));

        assertThat(tokenData.get("sig")).isNotNull();//this is a generated hash of the resource string
        assertThat(tokenData.get("se")).startsWith(LocalDate.now().toString());//the expiry date/time for the signature
        assertThat(tokenData.get("sv")).contains("2021-12-02");//azure api version is latest
        assertThat(tokenData.get("sp")).contains("rwl");//access permissions(write-w,list-l)
        assertThat(tokenData.get("sr")).isNotNull();
    }

    @Test
    void should_throw_unable_to_generate_sas_token_exception_when_unable_to_create_sas_token() {
        BlobServiceClient blobServiceClient = mock(BlobServiceClient.class);
        given(blobServiceClient.getBlobContainerClient("new-sscs"))
            .willThrow(new RuntimeException("Invalid service"));

        sasTokenGeneratorService = new SasTokenGeneratorService(
            blobServiceClient,
            getAccessTokenProperties()
        );

        assertThatThrownBy(() ->
            sasTokenGeneratorService.generateSasToken("sscs")
        ).isInstanceOf(UnableToGenerateSasTokenException.class)
            .hasMessage("java.lang.RuntimeException: Invalid service");

    }

    @Test
    void should_throw_service_not_configured_exception_when_service_is_unknow() {
        assertThatThrownBy(() ->
            sasTokenGeneratorService.getContainerName("unkown")
        ).isInstanceOf(ServiceNotConfiguredException.class)
            .hasMessage("No configuration found for service unkown");

    }

    @Test
    void should_return_container_name_when_configured_service_name_passed() {
        String containerName = sasTokenGeneratorService.getContainerName("sscs");
        assertThat(containerName).isEqualTo("new-sscs");
    }

    @Test
    void should_return_account_url() {
        String accountUrl = sasTokenGeneratorService.getAccountUrl();
        assertThat(accountUrl).isEqualTo("http://test.account");
    }

    private AccessTokenProperties getAccessTokenProperties() {
        AccessTokenProperties.TokenConfig tokenConfig = new AccessTokenProperties.TokenConfig();
        tokenConfig.setServiceName("sscs");
        tokenConfig.setNewContainerName("new-sscs");
        tokenConfig.setValidity(300);
        AccessTokenProperties accessTokenProperties = new AccessTokenProperties();
        accessTokenProperties.setServiceConfig(List.of(tokenConfig));
        return accessTokenProperties;
    }
}
