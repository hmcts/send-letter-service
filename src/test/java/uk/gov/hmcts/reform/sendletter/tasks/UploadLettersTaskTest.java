package uk.gov.hmcts.reform.sendletter.tasks;

import com.fasterxml.jackson.databind.ObjectMapper;
import net.schmizz.sshj.sftp.SFTPClient;
import nl.altindag.log.LogCaptor;
import nl.altindag.log.model.LogEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.FtpException;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.services.LetterEventService;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileToSend;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import static java.time.LocalDateTime.now;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Skipped;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;
import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.FACT_1593_INTERNATIONAL_POST_FLAG;
import static uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask.INTERNATIONAL_FOLDER;
import static uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask.SMOKE_TEST_LETTER_TYPE;

@ExtendWith(MockitoExtension.class)
class UploadLettersTaskTest {

    @Mock
    private LetterRepository repo;

    @Mock
    private FtpClient ftpClient;

    @Mock
    private SFTPClient sftpClient;

    @Mock
    private FtpAvailabilityChecker availabilityChecker;

    @Mock
    private LetterEventService letterEventService;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    @Mock
    private LaunchDarklyClient launchDarklyClient;

    private final ArgumentCaptor<FileToSend> captureFileToSend = ArgumentCaptor.forClass(FileToSend.class);

    @Captor
    private ArgumentCaptor<Function<SFTPClient, Integer>> captureRunWith;

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private LogCaptor uploadLettersTaskLogCaptor = LogCaptor.forClass(UploadLettersTask.class);

    private static final String ASSERTION_MESSAGE = "Expected and actual logs did not match";

    @BeforeEach
    void setUp() {
        given(availabilityChecker.isFtpAvailable(any(LocalTime.class))).willReturn(true);
        given(ftpClient.runWith(any())).willReturn(0);// value is a counter of uploaded letters
    }

    @AfterEach
    void tearDown() {
        reset(availabilityChecker, repo);
    }

    @Test
    void should_handle_smoke_test_letters() {
        // given
        given(serviceFolderMapping.getFolderFor(any())).willReturn(Optional.of("some_folder"));

        given(launchDarklyClient.isFeatureEnabled(FACT_1593_INTERNATIONAL_POST_FLAG)).willReturn(true);

        given(repo.countByStatus(Created)).willReturn(2);

        given(repo.findFirstLetterCreated(isA(LocalDateTime.class)))
            .willReturn(Optional.of(letterOfType(SMOKE_TEST_LETTER_TYPE, Map.of("Document_1", 1))))
            .willReturn(Optional.of(letterOfType("not_" + SMOKE_TEST_LETTER_TYPE, Map.of("Document_1", 1))))
            .willReturn(Optional.empty());

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

        // and
        verify(ftpClient, times(2)).upload(captureFileToSend.capture(), any(), any());
        assertThat(
            captureFileToSend
                .getAllValues()
                .stream()
                .map(file -> file.isSmokeTest)
        ).containsExactlyInAnyOrder(false, true);

        List<LogEvent> logEvents = uploadLettersTaskLogCaptor.getLogEvents();
        assertTrue(logEvents.get(2).getMessage().contains("folder: some_folder"),ASSERTION_MESSAGE);
    }

    @Test
    void should_handle_smoke_test_international_letters() {
        // given
        given(serviceFolderMapping.getFolderFor(any())).willReturn(Optional.of("some_folder" + INTERNATIONAL_FOLDER));

        given(launchDarklyClient.isFeatureEnabled(FACT_1593_INTERNATIONAL_POST_FLAG)).willReturn(true);

        given(repo.countByStatus(Created)).willReturn(2);

        given(repo.findFirstLetterCreated(isA(LocalDateTime.class)))
            .willReturn(Optional.of(internationalLetterOfType(SMOKE_TEST_LETTER_TYPE, Map.of("Document_1", 1))))
            .willReturn(Optional.of(internationalLetterOfType(
                "not_" + SMOKE_TEST_LETTER_TYPE, Map.of("Document_1", 1))))
            .willReturn(Optional.empty());

        // when
        task().run();

        // and
        verify(ftpClient).runWith(captureRunWith.capture());

        // when
        captureRunWith
            .getAllValues()
            .stream()
            .mapToInt(function -> function.apply(sftpClient))
            .sum();

        List<LogEvent> logEvents = uploadLettersTaskLogCaptor.getLogEvents();
        assertTrue(logEvents.get(2).getMessage().contains("folder: some_folder/international"),ASSERTION_MESSAGE);
    }

    @Test
    void should_not_start_process_if_ftp_is_not_available() {
        reset(availabilityChecker, ftpClient);
        given(availabilityChecker.isFtpAvailable(any(LocalTime.class))).willReturn(false);

        task().run();

        verify(ftpClient, never()).runWith(any());
        verify(repo, never()).countByStatus(Created);
    }

    @Test
    void should_skip_letter_if_folder_for_its_service_is_not_configured() {
        // given
        Letter letterA = letterForService("service_A", Map.of("Document_1", 1));
        Letter letterB = letterForService("service_B", Map.of("Document_1", 1));
        Letter letterC = letterForService("service_C", Map.of("Document_1", 1));

        given(repo.countByStatus(Created)).willReturn(3);

        given(repo.findFirstLetterCreated(isA(LocalDateTime.class)))
            .willReturn(Optional.of(letterA))
            .willReturn(Optional.of(letterB))
            .willReturn(Optional.of(letterC))
            .willReturn(Optional.empty());

        // and
        given(serviceFolderMapping.getFolderFor(letterA.getService())).willReturn(Optional.of("folder_A"));
        given(serviceFolderMapping.getFolderFor(letterB.getService())).willReturn(Optional.empty());
        given(serviceFolderMapping.getFolderFor(letterC.getService())).willReturn(Optional.of("folder_C"));

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
        assertThat(letterA.getStatus()).isEqualTo(Uploaded);
        assertThat(letterB.getStatus()).isEqualTo(Skipped);
        assertThat(letterC.getStatus()).isEqualTo(Uploaded);

        // and
        verify(ftpClient).upload(any(), eq("folder_A"), any());
        verify(ftpClient).upload(any(), eq("folder_C"), any());
        verifyNoMoreInteractions(ftpClient);
    }

    @Test
    void should_fail_letter_if_exception_thrown_during_upload() {
        // given
        Letter letterA = letterForService("service_A", Map.of("Document_1", 1));
        Letter letterB = letterForService("service_B", Map.of("Document_1", 1));
        Letter letterC = letterForService("service_C", Map.of("Document_1", 1));

        given(repo.countByStatus(Created)).willReturn(3);

        given(repo.findFirstLetterCreated(isA(LocalDateTime.class)))
            .willReturn(Optional.of(letterA))
            .willReturn(Optional.of(letterB))
            .willReturn(Optional.of(letterC))
            .willReturn(Optional.empty());

        // and
        given(serviceFolderMapping.getFolderFor(letterA.getService())).willReturn(Optional.of("folder_A"));
        given(serviceFolderMapping.getFolderFor(letterB.getService())).willReturn(Optional.of("folder_B"));

        doNothing().when(ftpClient).upload(any(FileToSend.class), eq("folder_A"), eq(sftpClient));
        NullPointerException ex = new NullPointerException("msg");
        doThrow(ex).when(ftpClient).upload(any(FileToSend.class), eq("folder_B"), eq(sftpClient));

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
        assertThat(uploadAttempts).isEqualTo(1);
        assertThat(letterA.getStatus()).isEqualTo(Uploaded);
        assertThat(letterC.getStatus()).isEqualTo(Created);

        // and
        verify(ftpClient).upload(any(), eq("folder_A"), any());
        verify(ftpClient).upload(any(), eq("folder_B"), any());
        verify(letterEventService).failLetterUpload(letterB, ex);
        verifyNoMoreInteractions(ftpClient);
    }

    @Test
    void should_not_fail_letter_if_ftp_exception_thrown_during_upload() {
        // given
        Letter letterA = letterForService("service_A", Map.of("Document_1", 1));
        Letter letterB = letterForService("service_B", Map.of("Document_1", 1));
        Letter letterC = letterForService("service_C", Map.of("Document_1", 1));

        given(repo.countByStatus(Created)).willReturn(3);

        given(repo.findFirstLetterCreated(isA(LocalDateTime.class)))
            .willReturn(Optional.of(letterA))
            .willReturn(Optional.of(letterB))
            .willReturn(Optional.of(letterC))
            .willReturn(Optional.empty());

        // and
        given(serviceFolderMapping.getFolderFor(letterA.getService())).willReturn(Optional.of("folder_A"));
        given(serviceFolderMapping.getFolderFor(letterB.getService())).willReturn(Optional.of("folder_B"));

        doNothing().when(ftpClient).upload(any(FileToSend.class), eq("folder_A"), eq(sftpClient));
        FtpException ex = new FtpException("msg", new IOException("io error"));
        doThrow(ex).when(ftpClient).upload(any(FileToSend.class), eq("folder_B"), eq(sftpClient));

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
        assertThat(uploadAttempts).isEqualTo(1);
        assertThat(letterA.getStatus()).isEqualTo(Uploaded);
        assertThat(letterB.getStatus()).isEqualTo(Created);
        assertThat(letterC.getStatus()).isEqualTo(Created);

        // and
        verify(ftpClient).upload(any(), eq("folder_A"), any());
        verify(ftpClient).upload(any(), eq("folder_B"), any());
        verifyNoInteractions(letterEventService);
        verifyNoMoreInteractions(ftpClient);
    }

    private Letter letterOfType(String type,  Map<String, Integer> copies) {
        return letter("cmc", type, copies);
    }

    private Letter internationalLetterOfType(String type,  Map<String, Integer> copies) {
        return internationalLetter("cmc", type, copies);
    }

    private Letter letterForService(String serviceName,  Map<String, Integer> copies) {
        return letter(serviceName, "type", copies);
    }

    private Letter letter(String service, String type, Map<String, Integer> copies) {
        return new Letter(
            UUID.randomUUID(),
            "msgId",
            service,
            null,
            type,
            "hello".getBytes(),
            true,
            "9c61b7da4e6c94416be51136122ed01acea9884f",
            now(),
            objectMapper.valueToTree(copies)
        );
    }

    private Letter internationalLetter(String service, String type, Map<String, Integer> copies) {
        return new Letter(
            UUID.randomUUID(),
            "msgId",
            service,
            new ObjectMapper().createObjectNode().put("isInternational",true),
            type,
            "hello".getBytes(),
            true,
            "9c61b7da4e6c94416be51136122ed01acea9884f",
            now(),
            objectMapper.valueToTree(copies)
        );
    }

    private UploadLettersTask task() {
        return new UploadLettersTask(
            repo,
            ftpClient,
            availabilityChecker,
            letterEventService,
            serviceFolderMapping,
            launchDarklyClient,
            0
        );
    }
}
