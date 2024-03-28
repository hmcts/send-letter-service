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
public class ProcessSaveV3Asyn extends FunctionalTestSuite {
    private static final Logger logger = LoggerFactory.getLogger(ProcessSaveV3Asyn.class);

    @Override
    String getContentType() {
        return MediaTypes.LETTER_V3;
    }

    @Test
    public void testSaveLetterAsync() throws IOException, JSONException {
        String letterId = sendPrintLetterRequestAsync(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-document-count-1.json",
                true,81, 82)
        );

        logger.info("Letter id created {}", letterId);
        String letterStatus = verifyLetterCreated(letterId);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.name());
    }

    @Test
    public void testSaveLetterAsync_should_return_same_letter_id_if_same_document_sent_twice()
        throws IOException, JSONException {
        String letterId = sendPrintLetterRequestAsync(
            signIn(),
            sampleIndexedPdfLetterRequestJson("letter-with-document-count-4.json",
                false, 141, 142)
        );

        logger.info("Letter id created {}", letterId);
        String letterStatus = verifyLetterCreated(letterId);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.name());

        try {
            Thread.sleep(10000);
        } catch (InterruptedException ex) {
            logger.error("Error: ", ex);
        }

        // the same pdf document in another letter (preserve the order)
        String jsonBody = sampleIndexedPdfLetterRequestJson("letter-with-document-count-5.json",
            false, 142, 143);
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
                sampleIndexedPdfLetterRequestJson("letter-with-document-count_duplicate_async-1.json",
                    true,101, 102)
            );
            String letterStatus = verifyLetterCreated(letterId);
            logger.info("Letter id {} , status {} ",  letterId, letterStatus);
        } catch (IOException | JSONException e) {
            logger.error(e.getMessage(), e);
        }
        return letterId;
    }
}
