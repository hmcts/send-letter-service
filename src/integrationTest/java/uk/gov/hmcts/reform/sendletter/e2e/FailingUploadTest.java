package uk.gov.hmcts.reform.sendletter.e2e;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.util.StreamUtils;
import org.springframework.web.context.WebApplicationContext;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.helper.FakeFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.LocalSftpServer;

import java.io.IOException;
import java.time.Duration;
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;
import static org.awaitility.Awaitility.await;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.FailedToUpload;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@SpringBootTest
@TestPropertySource(properties = {
    "scheduling.enabled=true",
    "tasks.upload-letters.interval-ms=20000",
    "tasks.mark-letters-posted.cron=*/1 * * * * *",
    "tasks.stale-letters-report.cron=*/1 * * * * *",
    "ftp.service-folders[0].service=some_service_name",
    "ftp.service-folders[0].folder=BULKPRINT"
})
@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@DirtiesContext
class FailingUploadTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private LetterRepository repository;

    @Autowired
    private FakeFtpAvailabilityChecker fakeFtpAvailabilityChecker;

    @Autowired
    private WebApplicationContext wac;

    @AfterEach
    public void cleanUp() {
        // This test commits transactions to the database
        // so we must clean up afterwards
        repository.deleteAll();
    }

    @Test
    void shouldHandleEmptyFileUploadsOldLetterModel() throws Throwable {
        try (var server = LocalSftpServer.create()) {

            // sftp servers is ups, now the background jobs can start connecting to it
            fakeFtpAvailabilityChecker.setAvailable(true);

            int numberOfRequests = 5;
            for (int i = 0; i < numberOfRequests; i++) {
                MockHttpServletRequestBuilder request =
                        post("/letters")
                                .header("ServiceAuthorization", "auth-header-value")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(readResource("letter" + i + ".json"));
                sendRequest(request);
            }

            corruptFileAndAwaitResult(numberOfRequests);
        }
    }

    @Test
    void shouldHandleEmptyFileUploadsNewLetterModel() throws Throwable {
        try (var server = LocalSftpServer.create()) {

            // sftp servers is ups, now the background jobs can start connecting to it
            fakeFtpAvailabilityChecker.setAvailable(true);

            int numberOfRequests = 5;
            for (int i = 0; i < numberOfRequests; i++) {
                MockHttpServletRequestBuilder request =
                        post("/letters")
                                .header("ServiceAuthorization", "auth-header-value")
                                .contentType(MediaTypes.LETTER_V2)
                                .content(readResource("letter-with-pdf" + i + ".json"));
                sendRequest(request);
            }

            corruptFileAndAwaitResult(numberOfRequests);
        }
    }

    private void sendRequest(MockHttpServletRequestBuilder request) throws Exception {
        mvc.perform(request)
                .andExpect(status().isOk())
                .andReturn();
    }

    private void corruptFileAndAwaitResult(int numberOfRequests) {
        await()
                .atMost(Duration.ofSeconds(10))
                .pollInterval(Duration.ofSeconds(2))
                .untilAsserted(() -> {
                    List<Letter> letters = repository.findAll();
                    assertThat(letters).as("Letters in DB").hasSize(numberOfRequests);
                });

        corruptFileInDataBase(2);

        // The report should be processed and the letter marked posted.
        await()
                .atMost(Duration.ofMinutes(2))
                .pollInterval(Duration.ofSeconds(5))
                .untilAsserted(() -> {
                    List<Letter> letters = repository.findAll();
                    assertThat(letters).as("Letters in DB").hasSize(numberOfRequests);
                    long failedCnt = letters.stream().filter(l -> l.getStatus().equals(FailedToUpload)).count();
                    long uploadedCnt = letters.stream().filter(l -> l.getStatus().equals(Uploaded)).count();
                    assertThat(failedCnt).as("Failed letters").isEqualTo(1);
                    assertThat(uploadedCnt).as("Uploaded letters").isEqualTo(4);
                });
    }

    private void corruptFileInDataBase(int ind) {
        // The interval between upload-letters tasks is 60 seconds (as defined by tasks.upload-letters.interval-ms),
        // this gives enough time to corrupt letter in db by making fileContent null
        List<Letter> savedLetters = repository.findAll();
        Letter savedLetter = savedLetters.get(ind);
        // making fileContent null causes NPE when file is being uploaded
        savedLetter.setFileContent(null);
        repository.saveAndFlush(savedLetter);
    }

    private String readResource(final String fileName) throws IOException {
        return StreamUtils.copyToString(
                new ClassPathResource(fileName).getInputStream(), UTF_8);
    }
}
