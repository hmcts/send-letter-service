package uk.gov.hmcts.reform.sendletter.controllers;

import io.restassured.RestAssured;
import io.restassured.specification.RequestSpecification;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.logging.appinsights.SyntheticHeaders;

import java.util.UUID;

@ExtendWith(SpringExtension.class)
class CreateLetterSmokeTest extends SmokeTestSuite {

    @Test
    void should_create_letter_successfully() throws Exception {

        String jwt = signIn();

        JSONObject json = new JSONObject(sampleLetterJson("letter.json"));
        JSONArray recipientsArray = new JSONArray().put(UUID.randomUUID().toString());
        JSONObject additionalData = new JSONObject().put("recipients", recipientsArray);
        json.put("additional_data", additionalData);
        String modifiedJson = json.toString();

        String id = givenJwt(jwt)
            .and()
            .body(modifiedJson)
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
}
