package uk.gov.hmcts.reform.sendletter.e2e;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@SpringBootTest
@TestPropertySource(properties = {
    "scheduling.enabled=true",
    "tasks.upload-letters.interval-ms=1000",
    "tasks.mark-letters-posted.cron=*/1 * * * * *",
    "tasks.stale-letters-report.cron=*/1 * * * * *",
    "ftp.service-folders[0].service=some_service_name",
    "ftp.service-folders[0].folder=BULKPRINT"
})
class EndToEndTest extends BaseTest {

    @ParameterizedTest
    @ValueSource(booleans = {false,true})
    void shouldHandleOldLetterModel(boolean isEncryptionEnabled) throws Throwable {
        should_upload_letter_and_mark_posted(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaType.APPLICATION_JSON)
                .content(readResource("letter.json")),
            isEncryptionEnabled
        );
    }

    @ParameterizedTest
    @ValueSource(booleans = {false,true})
    void shouldHandleNewLetterModel(boolean isEncryptionEnabled) throws Throwable {
        should_upload_letter_and_mark_posted(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaTypes.LETTER_V2)
                .content(readResource("letter-with-pdf.json")),
            isEncryptionEnabled
        );
    }
}
