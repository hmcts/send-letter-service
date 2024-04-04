package uk.gov.hmcts.reform.sendletter;

import io.restassured.RestAssured;
import org.hamcrest.Matchers;
import org.json.JSONException;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.io.IOException;

import static javax.servlet.http.HttpServletResponse.SC_OK;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;

@ExtendWith(SpringExtension.class)
class ProcessMessageTestForPdfEndpoint extends FunctionalTestSuite {
    private static final Logger logger = LoggerFactory.getLogger(ProcessMessageTestForPdfEndpoint.class);

    @Test
    @Disabled // upload is already tested in below test.
    void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_one_pdf_document() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            getModifiedJsonWithRecipients(
                sampleIndexedPdfLetterRequestJson("letter-with-single-pdf.json",
                    true, 51))
        );

        String status = verifyLetterUploaded(letterId);
        assertThat(status).isEqualTo(LetterStatus.Uploaded.name());

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 2);
            }
        });
    }

    @Test
    void should_return_same_letter_id_if_same_document_sent_twice() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-single-pdf-1.json",
                false, 111)
        );

        String status = verifyLetterUploaded(letterId);
        assertThat(status).isEqualTo(LetterStatus.Uploaded.name());

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 2);
            }
        });

        // the same pdf document in another letter
        String jsonBody = sampleIndexedPdfLetterRequestJson("letter-with-single-pdf-2.json",
            false, 111);
        RestAssured.given()
            .relaxedHTTPSValidation()
            .header("ServiceAuthorization", "Bearer " + signIn())
            .header(CONTENT_TYPE, getContentType())
            .baseUri(sendLetterServiceUrl)
            .body(jsonBody.getBytes())
            .when()
            .post("/letters")
            .then()
            .statusCode(SC_OK)
            .and()
            .body("letter_id", Matchers.equalTo(letterId));
    }

    @Test
    void may_throw_ConflictException() {
        executeMultiRequest(this::getLetterRequest);
    }

    private String getLetterRequest() {
        String letterId = "none";
        try {
            letterId = sendPrintLetterRequest(
                signIn(),
                sampleIndexedPdfLetterRequestJson("letter-with-three-pdfs-1.json",
                    true, 91, 92, 93)
            );
        } catch (IOException | JSONException e) {
            e.printStackTrace();
        }
        return letterId;
    }

    @Test
    @Disabled // upload works with 3 pdfs in other tests already.
    void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_two_pdf_document() throws Exception {
        String letterId = sendPrintLetterRequest(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-two-pdfs.json",
                true, 71, 72)
        );

        String status = verifyLetterUploaded(letterId);
        assertThat(status).isEqualTo(LetterStatus.Uploaded.name());

        awaitAndVerifyFileOnSftp(letterId, (sftpFile, sftp) -> {
            assertThat(sftpFile.getName()).matches(getFileNamePattern(letterId));

            if (!isEncryptionEnabled) {
                validatePdfFile(letterId, sftp, sftpFile, 4);
            }
        });
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

    @Override
    String getContentType() {
        return MediaTypes.LETTER_V2;
    }
}
