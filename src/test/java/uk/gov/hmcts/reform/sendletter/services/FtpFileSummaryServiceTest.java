package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.schmizz.sshj.sftp.SFTPClient;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.exception.FtpException;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class FtpFileSummaryServiceTest {

    @Mock
    private IFtpAvailabilityChecker ftpAvailabilityChecker;

    @Mock
    private FtpClient ftpClient;

    @Mock
    private SFTPClient sftpClient;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    private FtpFileSummaryService ftpFileSummaryService;

    @BeforeEach
    void setUp() {
        ftpFileSummaryService = new FtpFileSummaryService(ftpAvailabilityChecker, ftpClient, serviceFolderMapping);
        given(ftpAvailabilityChecker.isFtpAvailable(any())).willReturn(true);
    }

    @Test
    @SuppressWarnings("unchecked") // {@code invocation.getArgument(0)} has an Object. But we know what it is
    void should_return_files_list_for_two_services() throws IOException {
        // given
        String service1 = "service1";
        String service2 = "service2";
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of(service1, service2));
        FileInfo fileInfo = new FileInfo("aFile1 path", Instant.now());

        given(ftpClient.listLetters(service1, sftpClient)).willReturn(ImmutableList.of(fileInfo));
        given(ftpClient.listLetters(service2, sftpClient)).willReturn(emptyList());
        given(ftpClient.runWith(any())).willAnswer(invocation ->
            ((Function<SFTPClient, Void>) invocation.getArgument(0)).apply(sftpClient)
        );

        // when
        final Map<String, File> uploadSummaryFiles = ftpFileSummaryService.getFtpFileUploadSummaryFiles();

        // then
        verify(ftpClient, times(2)).runWith(any());
        verify(ftpClient, times(1)).listLetters(eq(service1), any());
        verify(ftpClient, times(1)).listLetters(eq(service2), any());

        assertFtpFilesSummaryFiles(service1, service2, fileInfo, uploadSummaryFiles);
    }

    private void assertFtpFilesSummaryFiles(String service1,
                                            String service2,
                                            FileInfo fileInfo,
                                            Map<String, File> uploadSummaryFiles) throws IOException {

        List<CSVRecord> csvRecordsForService1 = readCsv(uploadSummaryFiles.get(service1));
        assertThat(csvRecordsForService1).isNotEmpty().hasSize(2)
            .extracting(record -> tuple(record.get(0), record.get(1)))
            .contains(tuple("FileName", "UploadedAt"),
                tuple(fileInfo.path, fileInfo.modifiedAt.toString()));

        // File2 records
        List<CSVRecord> csvRecordsForService2 = readCsv(uploadSummaryFiles.get(service2));
        assertThat(csvRecordsForService2).isNotEmpty().hasSize(1)
            .extracting(record -> tuple(record.get(0), record.get(1)))
            .contains(tuple("FileName", "UploadedAt"));
    }

    @Test
    void should_not_connect_to_ftp_when_service_folders_are_empty() {
        // given
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of());

        // when
        Map<String, File> ftpFileUploadSummaryFiles = ftpFileSummaryService.getFtpFileUploadSummaryFiles();

        // then
        verify(ftpClient, never()).runWith(any());
        verify(ftpClient, never()).listLetters(any(), any());
        assertThat(ftpFileUploadSummaryFiles).isEmpty();
    }

    @Test
    void should_handle_ftp_exception() {
        // given
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of("aFolder"));
        given(ftpClient.runWith(any())).willThrow(FtpException.class);

        // when
        Map<String, File> ftpFileUploadSummaryFiles = ftpFileSummaryService.getFtpFileUploadSummaryFiles();

        // then
        assertThat(ftpFileUploadSummaryFiles).isEmpty();
        verify(ftpClient, times(1)).runWith(any());
        verify(ftpClient, never()).listLetters(eq("aFolder"), any());
    }

    @Test
    void should_not_connect_to_ftp_during_ftp_downtime() {
        // given
        given(ftpAvailabilityChecker.isFtpAvailable(any())).willReturn(false);

        // when
        Map<String, File> ftpFileUploadSummaryFiles = ftpFileSummaryService.getFtpFileUploadSummaryFiles();

        // then
        verify(ftpClient, never()).runWith(any());
        verify(ftpClient, never()).listLetters(any(), any());
        assertThat(ftpFileUploadSummaryFiles).isEmpty();
    }


    private List<CSVRecord> readCsv(File file) throws IOException {
        return CSVFormat.DEFAULT.parse(new FileReader(file)).getRecords();
    }
}
