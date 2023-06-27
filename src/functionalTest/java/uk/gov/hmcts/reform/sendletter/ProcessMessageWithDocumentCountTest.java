package uk.gov.hmcts.reform.sendletter;

import io.restassured.RestAssured;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import static javax.servlet.http.HttpServletResponse.SC_CONFLICT;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;

@ExtendWith(SpringExtension.class)
public class ProcessMessageWithDocumentCountTest extends FunctionalTestSuite {
    private static final Logger logger = LoggerFactory.getLogger(ProcessMessageWithDocumentCountTest.class);

    @Test
    void should_send_letter_and_upload_file_on_sftp_server() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-document-count.json",
                true,11, 12)
        );

        String status = verifyLetterUploaded(letterId);
        assertThat(status).isEqualTo(LetterStatus.Uploaded.name());

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 22);
            }
        });
    }

    @Test
    void should_return_conflict_if_same_document_sent_twice() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-document-count-2.json",
                false, 131, 132)
        );

        String status = verifyLetterUploaded(letterId);
        assertThat(status).isEqualTo(LetterStatus.Uploaded.name());

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 22);
            }
        });

        try {
            Thread.sleep(10000);
        } catch (InterruptedException ex) {
            logger.error("Error: ", ex);
        }

        // the same pdf document in another letter (preserve the order)
        String jsonBody = sampleIndexedPdfLetterRequestJson("letter-with-document-count-3.json",
            true, 132, 133);
        RestAssured.given()
                .relaxedHTTPSValidation()
                .header("ServiceAuthorization", "Bearer " + signIn())
                .header(CONTENT_TYPE, getContentType())
                .baseUri(sendLetterServiceUrl)
                .body(jsonBody.getBytes())
                .when()
                .post("/letters")
                .then()
                .statusCode(SC_CONFLICT);
    }

    private String verifyLetterUploaded(String letterId) {
        int counter = 1;
        String letterStatus = LetterStatus.Created.name();

        while (!letterStatus.equals(LetterStatus.Uploaded.name())) {
            try {
                Thread.sleep(LETTER_UPLOAD_DELAY);
                logger.info("Retrieving letter id {} and retry count {} ", letterId, counter++);
                letterStatus = getLetterStatus(letterId);
            } catch (AssertionError e) {
                logger.info("Retry error " + e.getMessage());
            } catch (InterruptedException interruptedException) {
                logger.error(interruptedException.getMessage(), interruptedException);
            }
        }

        return letterStatus;
    }

    @Test
    void may_throw_ConflictException()  {
        executeMultiRequest(this::getLetterRequest);
    }

    private String getLetterRequest() {
        String letterId = "none";

        try {
            letterId = sendPrintLetterRequest(
                signIn(),
                sampleIndexedPdfLetterRequestJson("letter-with-document-count_duplicate.json",
                    true, 21, 22)
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
