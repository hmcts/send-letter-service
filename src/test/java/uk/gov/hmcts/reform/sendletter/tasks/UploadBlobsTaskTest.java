package uk.gov.hmcts.reform.sendletter.tasks;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.specialized.BlobInputStream;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import net.schmizz.sshj.sftp.SFTPClient;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.blob.BlobReader;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.model.in.BlobInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileToSend;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.util.DigestUtils.md5DigestAsHex;
import static org.springframework.util.SerializationUtils.serialize;
import static uk.gov.hmcts.reform.sendletter.entity.PrintStatus.NEW;
import static uk.gov.hmcts.reform.sendletter.entity.PrintStatus.SKIPPED;
import static uk.gov.hmcts.reform.sendletter.entity.PrintStatus.UPLOADED;

@ExtendWith(MockitoExtension.class)
class UploadBlobsTaskTest {

    @Mock
    private PrintRepository repository;

    @Mock
    private FtpClient ftpClient;

    @Mock
    private SFTPClient sftpClient;

    @Mock
    private FtpAvailabilityChecker availabilityChecker;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    @Mock
    private BlobReader      blobReader;
    @Mock
    private BlobClient      blobClient;
    @Mock
    private BlobClient      blobClient2;
    @Mock
    private BlobClient      blobClient3;
    @Mock
    private BlobInputStream blobInputStream;
    @Mock
    private BlobInputStream blobInputStream3;

    private final ArgumentCaptor<FileToSend> captureFileToSend = ArgumentCaptor.forClass(FileToSend.class);

    @Captor
    private ArgumentCaptor<Function<SFTPClient, Integer>> captureRunWith;

    private static final ObjectMapper objectMapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        given(availabilityChecker.isFtpAvailable(any(LocalTime.class))).willReturn(true);
        given(ftpClient.runWith(any())).willReturn(0);// value is a counter of uploaded letters
    }

    @AfterEach
    void tearDown() {
        reset(availabilityChecker, repository, ftpClient);
    }

    @Test
    void should_not_start_process_if_ftp_is_not_available() {
        reset(availabilityChecker, ftpClient);
        given(availabilityChecker.isFtpAvailable(any(LocalTime.class))).willReturn(false);

        task().run();

        verify(ftpClient, never()).runWith(any());
        verify(repository, never()).countByStatus(NEW);
    }

    @Test
    void should_skip_letter_if_folder_for_its_service_is_not_configured() throws IOException {

        // given
        given(repository.countByStatus(NEW)).willReturn(1);

        BlobInfo blobInfo = new BlobInfo(blobClient);
        blobInfo.setLeaseId("LEASE_ID");
        given(blobReader.retrieveBlobToProcess())
            .willReturn(Optional.of(blobInfo))
            .willReturn(Optional.empty());

        String blob = "BULKPRINT001_sendlettertests_07062021101650_faa987b8-5d43-457e-bdaa-37fb824f7d5f.pgp";
        given(blobClient.getBlobName()).willReturn(blob);

        UUID uuid = UUID.fromString("faa987b8-5d43-457e-bdaa-37fb824f7d5f");
        Print printEntity = getPrintEntity(uuid, "sendlettertests");
        given(repository.findById(any())).willReturn(Optional.of(printEntity));

        given(blobClient.openInputStream()).willReturn(blobInputStream);
        given(blobInputStream.readAllBytes()).willReturn("anything".getBytes(StandardCharsets.UTF_8));
        // and
        given(serviceFolderMapping.getFolderFor(printEntity.getService())).willReturn(Optional.of("folder_A"));

        // when
        task().run();

        // and
        verify(ftpClient).runWith(captureRunWith.capture());

        // when
        int uploadAttempts = captureRunWith
            .getAllValues()
            .stream()
            .mapToInt(function -> function.apply(sftpClient))
            .sum();

        // and
        assertThat(uploadAttempts).isEqualTo(1);
        assertThat(printEntity.getStatus()).isEqualTo(UPLOADED);
        verify(ftpClient).upload(any(), eq("folder_A"), any());
        verifyNoMoreInteractions(ftpClient);
        verify(blobClient).deleteWithResponse(any(), any(), any(), any());
    }

    @Test
    void should_skip_letter_if_folder_for_its_service_is_not_configured_v2() throws IOException {
        // given
        given(repository.countByStatus(NEW)).willReturn(3);

        BlobInfo blobInfo1 = new BlobInfo(blobClient);
        BlobInfo blobInfo2 = new BlobInfo(blobClient2);
        BlobInfo blobInfo3 = new BlobInfo(blobClient3);

        blobInfo1.setLeaseId("LEASE_ID");
        blobInfo2.setLeaseId("LEASE_ID");
        blobInfo3.setLeaseId("LEASE_ID");

        given(blobReader.retrieveBlobToProcess())
            .willReturn(Optional.of(blobInfo1))
            .willReturn(Optional.of(blobInfo2))
            .willReturn(Optional.of(blobInfo3))
            .willReturn(Optional.empty());

        String blob1 = "BULKPRINT001_sendlettertests_07062021101650_faa987b8-5d43-457e-bdaa-37fb824f7d5f.pgp";
        given(blobClient.getBlobName()).willReturn(blob1);

        String blob2 = "BULKPRINT001_sendlettertests_07062021101650_f2d9348e-0301-4439-9738-920c58fcc90f.pgp";
        given(blobClient2.getBlobName()).willReturn(blob2);

        String blob3 = "BULKPRINT001_sendlettertests_07062021101650_68b72d70-a561-4515-9593-4d6df47e6ef6.pgp";
        given(blobClient3.getBlobName()).willReturn(blob3);

        UUID uuid1 = UUID.fromString("faa987b8-5d43-457e-bdaa-37fb824f7d5f");
        Print printEntity1 = getPrintEntity(uuid1, "sendlettertests");

        UUID uuid2 = UUID.fromString("f2d9348e-0301-4439-9738-920c58fcc90f");
        Print printEntity2 = getPrintEntity(uuid2, null);

        UUID uuid3 = UUID.fromString("68b72d70-a561-4515-9593-4d6df47e6ef6");
        Print printEntity3 = getPrintEntity(uuid3, "sendlettertests");

        given(repository.findById(any()))
            .willReturn(Optional.of(printEntity1))
            .willReturn(Optional.of(printEntity2))
            .willReturn(Optional.of(printEntity3));

        given(blobClient.openInputStream()).willReturn(blobInputStream);
        given(blobClient3.openInputStream()).willReturn(blobInputStream3);

        given(blobInputStream.readAllBytes()).willReturn("anything".getBytes(StandardCharsets.UTF_8));
        given(blobInputStream3.readAllBytes()).willReturn("anything3".getBytes(StandardCharsets.UTF_8));
        // and
        given(serviceFolderMapping.getFolderFor(printEntity1.getService())).willReturn(Optional.of("folder_A"));
        given(serviceFolderMapping.getFolderFor(printEntity2.getService())).willReturn(Optional.empty());
        given(serviceFolderMapping.getFolderFor(printEntity3.getService())).willReturn(Optional.of("folder_C"));


        // when
        task().run();

        // and
        verify(ftpClient).runWith(captureRunWith.capture());

        // when
        int uploadAttempts = captureRunWith
            .getAllValues()
            .stream()
            .mapToInt(function -> function.apply(sftpClient))
            .sum();

        // then
        assertThat(uploadAttempts).isEqualTo(2);
        assertThat(printEntity1.getStatus()).isEqualTo(UPLOADED);
        assertThat(printEntity2.getStatus()).isEqualTo(SKIPPED);
        assertThat(printEntity3.getStatus()).isEqualTo(UPLOADED);

        verify(ftpClient, times(2)).upload(captureFileToSend.capture(), any(), any());
        verifyNoMoreInteractions(ftpClient);
        verify(blobClient).deleteWithResponse(any(), any(), any(), any());
        verify(blobClient3).deleteWithResponse(any(), any(), any(), any());
    }

    private UploadBlobsTask task() {
        return new UploadBlobsTask(
            repository,
            ftpClient,
            availabilityChecker,
            serviceFolderMapping,
            blobReader
        );
    }

    private JsonNode getDocuments() throws JsonProcessingException {
        return objectMapper.readTree(
            "[{\n"
                + "\"file_name\": \"mypdf.pdf\",\n"
                + "\"copies_required\": 2\n"
                + "},\n"
                + "{\n"
                + "\"file_name\": \"1.pdf\",\n"
                + "\"copies_required\": 1\n"
                + " }\n"
                + "]");
    }

    private Print getPrintEntity(UUID uuid, String service)
        throws JsonProcessingException {
        String idempotencyKey = md5DigestAsHex(Objects.requireNonNull(serialize(uuid)));
        LocalDateTime currentDateTime = LocalDateTime.now();
        return SampleData.printEntity(
            uuid,
            service,
            currentDateTime,
            "BULKPRINT001",
            idempotencyKey,
            getDocuments(),
            "caseId",
            "caseRef",
            "letterType"
        );
    }
}
