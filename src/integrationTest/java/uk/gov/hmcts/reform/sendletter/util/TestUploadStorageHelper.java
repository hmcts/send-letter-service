package uk.gov.hmcts.reform.sendletter.util;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import org.testcontainers.containers.DockerComposeContainer;

import java.io.File;

public class TestUploadStorageHelper {

    private static TestUploadStorageHelper INSTANCE;
    public static final String CONTAINER_NAME = "encrypted";

    private static DockerComposeContainer<?> dockerComposeContainer;
    private static String dockerHost;
    public static final String STORAGE_CONN_STRING = "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;"
        + "AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;"
        + "BlobEndpoint=http://%s:%d/devstoreaccount1;";
    public static BlobServiceClient blobServiceClient;
    private BlobContainerClient testContainer;

    private TestUploadStorageHelper() {
        // empty constructor
    }

    public static TestUploadStorageHelper getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new TestUploadStorageHelper();
        }
        return INSTANCE;
    }

    private static void createDocker() {
        dockerComposeContainer = new DockerComposeContainer<>(
            new File("src/integrationTest/resources/docker/docker-compose.yml")
        ).withExposedService("azure-storage", 10000)
        .withLocalCompose(true);
        dockerComposeContainer.start();
        dockerHost = dockerComposeContainer.getServiceHost("azure-storage", 10000);
    }

    private static void initializeStorage() {
        blobServiceClient = new BlobServiceClientBuilder()
            .connectionString(String.format(STORAGE_CONN_STRING, dockerHost, 10000))
            .buildClient();
    }

    public static void initialize() {
        createDocker();
        initializeStorage();
    }

    public static void stopDocker() {
        dockerComposeContainer.stop();
    }

    public BlobContainerClient createContainer() {
        testContainer = blobServiceClient.getBlobContainerClient(CONTAINER_NAME);
        testContainer.create();
        return testContainer;
    }

    public void deleteContainer() {
        testContainer.delete();
    }
}
