package uk.gov.hmcts.reform.sendletter.util;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.utility.DockerImageName;

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
public class TestUploadStorageHelper {

    private static TestUploadStorageHelper INSTANCE;
    public static final String CONTAINER_NAME = "encrypted";

    private static GenericContainer<?> dockerComposeContainer =
        new GenericContainer<>(DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:3.29.0"))
            .withExposedPorts(10000);
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
        dockerComposeContainer.withEnv("executable", "blob");
        dockerComposeContainer.withNetworkAliases("azurite");
        dockerComposeContainer.start();
        dockerHost = dockerComposeContainer.getHost();
    }

    private static void initializeStorage() {
        blobServiceClient = new BlobServiceClientBuilder()
            .connectionString(String.format(
                STORAGE_CONN_STRING,
                dockerHost,
                dockerComposeContainer.getMappedPort(10000)
            ))
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
