package uk.gov.hmcts.reform.sendletter.services;

import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.common.StorageSharedKeyCredential;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

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
        assertThat(tokenData.get("sv")).contains("2020-04-08");//azure api version is latest
        assertThat(tokenData.get("sp")).contains("rwl");//access permissions(write-w,list-l)
        assertThat(tokenData.get("sr")).isNotNull();
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
