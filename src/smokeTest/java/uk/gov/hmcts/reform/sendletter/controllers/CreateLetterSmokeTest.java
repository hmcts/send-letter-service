package uk.gov.hmcts.reform.sendletter.controllers;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import io.restassured.RestAssured;
import io.restassured.specification.RequestSpecification;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.logging.appinsights.SyntheticHeaders;

import java.io.IOException;
import java.util.Base64;
import java.util.UUID;

import static com.google.common.io.Resources.getResource;
import static com.google.common.io.Resources.toByteArray;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;

@ExtendWith(SpringExtension.class)
class CreateLetterSmokeTest extends SmokeTestSuite {

    @Test
    @Disabled
    void should_create_letter_successfully() throws Exception {

        String jsonBody = sampleIndexedPdfLetterRequestJson("letter-with-document-count.json",
            false, 142, 143);

        String jwt = signIn();

        String id = givenJwt(jwt)
            .and()
            .body(jsonBody.getBytes())
            .header(CONTENT_TYPE,  MediaTypes.LETTER_V3)
            .when()
            .post("/letters")
            .then()
            .statusCode(200)
            .extract()
            .body()
            .jsonPath()
            .get("letter_id");

        givenJwt(jwt)
            .when()
            .get("/letters/" + id)
            .then()
            .statusCode(200);
    }

    private RequestSpecification givenJwt(String jwt) {
        return RestAssured
            .given()
            .relaxedHTTPSValidation()
            .baseUri(this.testUrl)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .header(SyntheticHeaders.SYNTHETIC_TEST_SOURCE, SYNTHETIC_SOURCE_HEADER_VALUE)
            .header("ServiceAuthorization", "Bearer " + jwt);
    }


    private String sampleIndexedPdfLetterRequestJson(String requestBodyFilename, boolean uniqueRecipients, int... idxs)
        throws IOException, JSONException {
        String requestBody = Resources.toString(getResource(requestBodyFilename), Charsets.UTF_8);

        for (int idx: idxs) {
            byte[] pdf = toByteArray(getResource("test" + idx + ".pdf"));
            requestBody = requestBody.replace("{{pdf" + idx + "}}", new String(Base64.getEncoder().encode(pdf)));
        }
        return uniqueRecipients ? getModifiedJsonWithRecipients(requestBody) : requestBody;
    }

    private String getModifiedJsonWithRecipients(String originalJson) throws JSONException {
        JSONObject json = new JSONObject(originalJson);
        JSONArray recipientsArray = new JSONArray().put(UUID.randomUUID().toString());
        JSONObject additionalData = new JSONObject().put("recipients", recipientsArray);
        json.put("additional_data", additionalData);
        return json.toString();
    }
}
