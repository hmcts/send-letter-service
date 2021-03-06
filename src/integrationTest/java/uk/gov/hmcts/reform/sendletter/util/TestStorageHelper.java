package uk.gov.hmcts.reform.sendletter.util;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import org.testcontainers.containers.DockerComposeContainer;

import java.io.File;

/*
 * The Microsoft Azure storage emulator provides a local environment
 * that emulates the Azure Blob, Queue, and Table services for development purposes.
 *  Using the storage emulator, you can test your application against the
 * storage services locally, without creating an Azure subscription or incurring any costs.
 *  When you're satisfied with how your application is working in the emulator,
 *  you can switch to using an Azure storage account in the cloud.
 *
 * Note: No need to modify the secret, it was hardcoded in container
 * https://hub.docker.com/r/microsoft/azure-storage-emulator/
 * */
public class TestStorageHelper {

    private static TestStorageHelper INSTANCE;

    public static final String CONTAINER_NAME = "new-bulkprint";


    private static DockerComposeContainer<?> dockerComposeContainer;
    private static String dockerHost;
    public static final String STORAGE_CONN_STRING = "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;"
        + "AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;"
        + "BlobEndpoint=http://%s:%d/devstoreaccount1;";
    public static BlobServiceClient blobServiceClient;
    private BlobContainerClient testContainer;

    private TestStorageHelper() {
        // empty constructor
    }

    public static TestStorageHelper getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new TestStorageHelper();
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

    public void createBulkprintContainer() {
        testContainer = blobServiceClient.getBlobContainerClient(CONTAINER_NAME);
        testContainer.create();
    }

    public void deleteBulkprintContainer() {
        testContainer.delete();
    }
}
