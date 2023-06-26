package uk.gov.hmcts.reform.sendletter;

import io.restassured.RestAssured;
import org.json.JSONException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.io.IOException;

import static javax.servlet.http.HttpServletResponse.SC_CONFLICT;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;

@ExtendWith(SpringExtension.class)
class ProcessMessageTestForPdfEndpointV2Async extends FunctionalTestSuite {
    private static final Logger logger = LoggerFactory.getLogger(ProcessMessageTestForPdfEndpointV2Async.class);

    @Override
    String getContentType() {
        return MediaTypes.LETTER_V2;
    }

    @Test
    void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_three_pdf_documents() throws Exception {
        String letterId = sendPrintLetterRequestAsync(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-three-pdfs.json", 61, 62, 63)
        );

        String letterStatus = verifyLetterCreated(letterId);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.name());
    }

    @Test
    void should_return_conflict_if_same_document_sent_twice_when_letter_contains_three_pdf_documents()
            throws Exception {
        String letterId = sendPrintLetterRequestAsync(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-three-pdfs-2.json", 121, 122, 123)
        );

        String letterStatus = verifyLetterCreated(letterId);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.name());

        // the same pdf document in another letter
        String jsonBody = sampleIndexedPdfLetterRequestJson("letter-with-three-pdfs-3.json", 123, 124, 125);
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

    private String verifyLetterCreated(String letterId) {
        int counter = 1;
        String letterStatus = "Not found";
        while (letterStatus.equals("Not found") && counter <= LETTER_STATUS_RETRY_COUNT) {
            try {
                logger.info("Retrieving letter id {} and retry count {} ", letterId, counter++);
                letterStatus = getLetterStatus(letterId);
            } catch (AssertionError e) {
                logger.info("Retry error " + e.getMessage());
                if (e.getMessage().contains("409")) {
                    throw e;
                }
                try {
                    Thread.sleep(LETTER_STATUS_RETRY_INTERVAL);
                } catch (InterruptedException interruptedException) {
                    logger.error(interruptedException.getMessage(), interruptedException);
                }
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
            letterId = sendPrintLetterRequestAsync(
                signIn(),
                sampleIndexedPdfLetterRequestJson("letter-with-four-pdfs.json", 41, 42, 43, 44)
            );
            String letterStatus = verifyLetterCreated(letterId);
            logger.info("Letter id {} , status {} ",  letterId, letterStatus);
        } catch (IOException | JSONException e) {
            logger.error(e.getMessage(), e);
        }
        return letterId;

    }

}
