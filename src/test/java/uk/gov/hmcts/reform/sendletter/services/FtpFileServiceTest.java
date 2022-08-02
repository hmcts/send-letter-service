package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.schmizz.sshj.sftp.SFTPClient;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.exception.FtpException;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.Instant;
import java.util.function.Function;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class FtpFileServiceTest {

    @Mock
    private FtpClient ftpClient;

    @Mock
    private SFTPClient sftpClient;

    @Mock
    private ServiceFolderMapping serviceFolderMapping;

    private FtpFileService ftpFileService;

    @BeforeEach
    @SuppressWarnings("unchecked")
        // {@code invocation.getArgument(0)} has Object. But we know what it is
    void setUp() {
        given(ftpClient.runWith(any())).willAnswer(invocation ->
            ((Function<SFTPClient, Void>) invocation.getArgument(0)).apply(sftpClient)
        );
        ftpFileService = new FtpFileService(ftpClient, serviceFolderMapping);
    }

    @Test
    void should_return_files_list_for_all_services() {
        // given
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of("aFolder", "bFolder"));
        given(ftpClient.listLetters("aFolder", sftpClient))
            .willReturn(ImmutableList.of(
                new FileInfo("aFile1 path", Instant.now()),
                new FileInfo("aFile2 path", Instant.now())));
        given(ftpClient.listLetters("bFolder", sftpClient))
            .willReturn(ImmutableList.of(new FileInfo("bFile1 path", Instant.now())));

        // when
        ftpFileService.listLettersForAllServices();

        // then
        verify(ftpClient, times(2)).runWith(any());
        verify(ftpClient, times(1)).listLetters(eq("aFolder"), any());
        verify(ftpClient, times(1)).listLetters(eq("bFolder"), any());
    }

    @Test
    void should_not_connect_to_ftp_when_service_folders_are_empty() {
        // given
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of());

        // when
        ftpFileService.listLettersForAllServices();

        // then
        verify(ftpClient, never()).runWith(any());
        verify(ftpClient, never()).listLetters(any(), any());
    }

    @Test
    void should_handle_ftp_exception() {
        // given
        given(serviceFolderMapping.getFolders()).willReturn(ImmutableSet.of("aFolder"));
        given(ftpClient.listLetters("aFolder", sftpClient)).willThrow(FtpException.class);

        // when
        ftpFileService.listLettersForAllServices();

        // then
        verify(ftpClient, times(1)).runWith(any());
        verify(ftpClient, never()).listLetters(eq("aFolder"), any());
    }
}
