package uk.gov.hmcts.reform.sendletter.tasks;

import com.azure.storage.blob.BlobContainerClient;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.util.StreamUtils;
import uk.gov.hmcts.reform.sendletter.blob.BlobReader;
import uk.gov.hmcts.reform.sendletter.blob.LeaseClientProvider;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.helper.FtpHelper;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.services.LocalSftpServer;
import uk.gov.hmcts.reform.sendletter.services.PrintService;
import uk.gov.hmcts.reform.sendletter.services.SasTokenGeneratorService;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.util.TestStorageHelper;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.time.LocalTime;
import java.util.Optional;
import java.util.UUID;
import javax.persistence.EntityManager;

import static com.google.common.base.Charsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static uk.gov.hmcts.reform.sendletter.entity.PrintStatus.UPLOADED;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@SpringBootTest
class IUploadBlobsTaskTest {

    private static final String TEST_BLOB =
        "BULKPRINT001_sendlettertests_07062021101650_faa987b8-5d43-457e-bdaa-37fb824f7d5f.pgp";
    @Autowired
    private PrintRepository printRepository;
    @Autowired
    private AccessTokenProperties accessTokenProperties;
    @Autowired
    private LeaseClientProvider leaseClientProvider;

    private BlobReader blobReader;
    private PrintService printService;
    private ObjectMapper mapper;

    @Autowired
    private EntityManager entityManager;

    @Mock
    private FtpAvailabilityChecker availabilityChecker;

    @Mock
    ServiceFolderMapping serviceFolderMapping;
    BlobContainerClient container;

    @BeforeAll
    public static void initializeStorage() {
        TestStorageHelper.initialize();
    }

    @AfterAll
    public static void tearDownContainer() {
        TestStorageHelper.stopDocker();
    }


    @BeforeEach
    void setUp() {
        when(availabilityChecker.isFtpAvailable(any(LocalTime.class))).thenReturn(true);
        when(serviceFolderMapping.getFolderFor(any())).thenReturn(Optional.of(LocalSftpServer.SERVICE_FOLDER));

        TestStorageHelper.getInstance().createBulkprintContainer();
        container = TestStorageHelper.getInstance().createContainer();
        printRepository.deleteAll();
        var sasTokenGeneratorService = new SasTokenGeneratorService(
            TestStorageHelper.blobServiceClient,
            accessTokenProperties
        );
        mapper = new ObjectMapper();
        printService = new PrintService(printRepository, mapper, sasTokenGeneratorService);
        blobReader =  new BlobReader(TestStorageHelper.getInstance().getBlobServiceClientProvider(),
            accessTokenProperties, leaseClientProvider, 20);

    }

    @AfterEach
    void afterEach() {
        printRepository.deleteAll();
        TestStorageHelper.getInstance().deleteBulkprintContainer();
    }

    @Test
    void uploads_blob_to_sftp_and_sets_letter_status_to_uploaded() throws Exception {

        var json = StreamUtils.copyToString(
            new ClassPathResource("print_job.json").getInputStream(), UTF_8);
        var service = "sscs";
        var uuid = UUID.fromString("faa987b8-5d43-457e-bdaa-37fb824f7d5f");
        var printRequest = mapper.readValue(json, PrintRequest.class);
        printService.save(uuid.toString(), service, printRequest);

        var blobClient = container.getBlobClient(TEST_BLOB);
        byte[] bytes = "anything".getBytes(StandardCharsets.UTF_8);
        blobClient.upload(new ByteArrayInputStream(bytes), bytes.length);

        var task = new UploadBlobsTask(
            printRepository,
            FtpHelper.getSuccessfulClient(LocalSftpServer.port),
            availabilityChecker,
            serviceFolderMapping,
            blobReader);

        // Invoke the upload job.
        try (var server = LocalSftpServer.create()) {
            task.run();

            // file should exist in SFTP site.
            File[] files = server.lettersFolder.listFiles();
            assertThat(files.length).isEqualTo(1);

            // Ensure the letter is marked as uploaded in the database.
            // Clear the JPA cache to force a read.
            entityManager.clear();
            var printObj = printRepository.findById(uuid).get();
            assertThat(printObj.getStatus()).isEqualTo(UPLOADED);
            assertThat(printObj.getSentToPrintAt()).isNotNull();
            assertThat(printObj.getPrintedAt()).isNull();
        }
    }

}
