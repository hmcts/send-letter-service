package uk.gov.hmcts.reform.sendletter;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(SpringExtension.class)
public class ProcessMessageWithDocumentCountTest extends FunctionalTestSuite {

    @Test
    void should_send_letter_and_upload_file_on_sftp_server() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            samplePdfLetterRequestJson("letter-with-document-count.json", "test.pdf")
        );

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 22);
            }
        });
    }

    @Test
    void should_throw_ConflictException()  {
        executeMutiReques(this::getLetterRequest);
    }

    private String getLetterRequest() {
        String letterId = "none";
        try {
            letterId = sendPrintLetterRequest(
                    signIn(),
                    samplePdfLetterRequestJson("letter-with-document-count_3.json", "test.pdf")
            );
        } catch (Exception e) {
            e.printStackTrace();
        }
        return letterId;
    }


    @Override
    String getContentType() {
        return MediaTypes.LETTER_V3;
    }
}
