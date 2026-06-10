package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.FtpDownloadException;
import uk.gov.hmcts.reform.sendletter.exception.FtpException;
import uk.gov.hmcts.reform.sendletter.exception.LetterFileNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.TestingSupportLetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.DownloadedLetterFile;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class TestingSupportServiceTest {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
    private static final LocalDateTime CREATED_AT = LocalDateTime.of(2026, 6, 10, 12, 30);
    private static final String SERVICE = "cmc";
    private static final String SERVICE_FOLDER = "cmc-folder";
    private static final String TARGET_FOLDER = "target";
    private static final String SMOKE_TEST_TARGET_FOLDER = "smoke";

    @Mock
    private LetterRepository letterRepository;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    @Mock
    private FtpClient ftpClient;

    @Mock
    private FtpConfigProperties ftpConfigProperties;

    private TestingSupportService testingSupportService;

    @BeforeEach
    void setUp() {
        testingSupportService = new TestingSupportService(
            letterRepository,
            serviceFolderMapping,
            ftpClient,
            ftpConfigProperties
        );
    }

    @Test
    void should_download_letter_file_from_service_folder() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, "letter_type", null, false);
        byte[] content = "letter-content".getBytes();
        String filename = FileNameHelper.generateName(letter);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(serviceFolderMapping.getFolderFor(SERVICE)).willReturn(Optional.of(SERVICE_FOLDER));
        given(ftpConfigProperties.getTargetFolder()).willReturn(TARGET_FOLDER);
        given(ftpClient.downloadFile(TARGET_FOLDER + "/" + SERVICE_FOLDER + "/" + filename)).willReturn(content);

        DownloadedLetterFile file = testingSupportService.downloadLetterFile(letterId);

        assertThat(file.getFilename()).isEqualTo(filename);
        assertThat(file.getContent()).isEqualTo(content);
    }

    @Test
    void should_download_international_letter_file_from_international_service_folder() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, "letter_type", OBJECT_MAPPER.createObjectNode().put("isInternational", true),
            false);
        byte[] content = "letter-content".getBytes();
        String filename = FileNameHelper.generateName(letter);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(serviceFolderMapping.getFolderFor(SERVICE)).willReturn(Optional.of(SERVICE_FOLDER));
        given(ftpConfigProperties.getTargetFolder()).willReturn(TARGET_FOLDER);
        given(ftpClient.downloadFile(
            TARGET_FOLDER + "/" + SERVICE_FOLDER + UploadLettersTask.INTERNATIONAL_FOLDER + "/" + filename
        )).willReturn(content);

        DownloadedLetterFile file = testingSupportService.downloadLetterFile(letterId);

        assertThat(file.getFilename()).isEqualTo(filename);
        assertThat(file.getContent()).isEqualTo(content);
    }

    @Test
    void should_download_smoke_test_letter_file_from_smoke_test_folder() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, UploadLettersTask.SMOKE_TEST_LETTER_TYPE, null, false);
        byte[] content = "letter-content".getBytes();
        String filename = FileNameHelper.generateName(letter);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(ftpConfigProperties.getSmokeTestTargetFolder()).willReturn(SMOKE_TEST_TARGET_FOLDER);
        given(ftpClient.downloadFile(SMOKE_TEST_TARGET_FOLDER + "/" + filename)).willReturn(content);

        DownloadedLetterFile file = testingSupportService.downloadLetterFile(letterId);

        assertThat(file.getFilename()).isEqualTo(filename);
        assertThat(file.getContent()).isEqualTo(content);
        verifyNoInteractions(serviceFolderMapping);
    }

    @Test
    void should_throw_letter_not_found_when_letter_does_not_exist() {
        UUID letterId = UUID.randomUUID();

        given(letterRepository.findById(letterId)).willReturn(Optional.empty());

        assertThatThrownBy(() -> testingSupportService.downloadLetterFile(letterId))
            .isInstanceOf(TestingSupportLetterNotFoundException.class)
            .hasMessage("Letter with ID '" + letterId + "' not found");

        verifyNoInteractions(serviceFolderMapping, ftpClient, ftpConfigProperties);
    }

    @Test
    void should_throw_letter_file_not_found_when_sftp_file_does_not_exist() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, "letter_type", null, false);
        String filename = FileNameHelper.generateName(letter);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(serviceFolderMapping.getFolderFor(SERVICE)).willReturn(Optional.of(SERVICE_FOLDER));
        given(ftpConfigProperties.getTargetFolder()).willReturn(TARGET_FOLDER);
        given(ftpClient.downloadFile(TARGET_FOLDER + "/" + SERVICE_FOLDER + "/" + filename))
            .willThrow(new LetterFileNotFoundException(filename));

        assertThatThrownBy(() -> testingSupportService.downloadLetterFile(letterId))
            .isInstanceOf(LetterFileNotFoundException.class)
            .hasMessage("Letter with ID '" + letterId + "' exists but file '" + filename + "' was not found on SFTP");
    }

    @Test
    void should_wrap_ftp_download_errors() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, "letter_type", null, false);
        String filename = FileNameHelper.generateName(letter);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(serviceFolderMapping.getFolderFor(SERVICE)).willReturn(Optional.of(SERVICE_FOLDER));
        given(ftpConfigProperties.getTargetFolder()).willReturn(TARGET_FOLDER);
        given(ftpClient.downloadFile(TARGET_FOLDER + "/" + SERVICE_FOLDER + "/" + filename))
            .willThrow(new FtpException("FTP operation failed.", new RuntimeException()));

        assertThatThrownBy(() -> testingSupportService.downloadLetterFile(letterId))
            .isInstanceOf(FtpDownloadException.class)
            .hasMessage("Unable to download file '" + filename + "' from SFTP");
    }

    @Test
    void should_throw_letter_not_found_when_service_folder_is_not_configured() {
        UUID letterId = UUID.randomUUID();
        Letter letter = letter(letterId, "letter_type", null, false);

        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(serviceFolderMapping.getFolderFor(SERVICE)).willReturn(Optional.empty());

        assertThatThrownBy(() -> testingSupportService.downloadLetterFile(letterId))
            .isInstanceOf(LetterNotFoundException.class);

        verifyNoInteractions(ftpClient);
    }

    private Letter letter(UUID id, String type, JsonNode additionalData, Boolean isEncrypted) {
        return new Letter(
            id,
            "checksum",
            SERVICE,
            additionalData,
            type,
            null,
            isEncrypted,
            null,
            CREATED_AT,
            null
        );
    }
}
