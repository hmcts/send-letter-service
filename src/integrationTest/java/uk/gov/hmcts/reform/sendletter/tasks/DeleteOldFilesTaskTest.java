package uk.gov.hmcts.reform.sendletter.tasks;

import com.google.common.collect.ImmutableSet;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.springframework.test.context.junit4.SpringRunner;
import uk.gov.hmcts.reform.sendletter.helper.FtpHelper;
import uk.gov.hmcts.reform.sendletter.services.LocalSftpServer;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileToSend;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.Duration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;

@RunWith(SpringRunner.class)
public class DeleteOldFilesTaskTest {

    @Mock ServiceFolderMapping serviceFolderMapping;

    @Test
    public void should_delete_files() throws Exception {
        try (LocalSftpServer server = LocalSftpServer.create()) {

            // given
            given(serviceFolderMapping.getFolders())
                .willReturn(ImmutableSet.of(LocalSftpServer.SERVICE_FOLDER));

            FtpClient ftp = FtpHelper.getSuccessfulClient(LocalSftpServer.port);

            ftp.upload(
                new FileToSend("hello.zip", "some content".getBytes()),
                false,
                LocalSftpServer.SERVICE_FOLDER
            );

            DeleteOldFilesTask task = new DeleteOldFilesTask(ftp, serviceFolderMapping, Duration.ZERO);

            // when
            task.run();

            //then
            assertThat(ftp.listLetters(LocalSftpServer.SERVICE_FOLDER)).isEmpty();
        }
    }
}
