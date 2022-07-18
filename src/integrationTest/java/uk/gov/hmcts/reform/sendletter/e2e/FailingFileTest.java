package uk.gov.hmcts.reform.sendletter.e2e;

import com.microsoft.applicationinsights.web.internal.WebRequestTrackingFilter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.ClassPathResource;
import org.springframework.mock.web.MockFilterConfig;
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
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;
import static org.awaitility.Awaitility.await;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.webAppContextSetup;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.FailedToUpload;

@SpringBootTest
@TestPropertySource(properties = {
        "scheduling.enabled=true",
        "tasks.upload-letters.interval-ms=30000",
        "tasks.mark-letters-posted.cron=*/1 * * * * *",
        "tasks.stale-letters-report.cron=*/1 * * * * *",
        "ftp.service-folders[0].service=some_service_name",
        "ftp.service-folders[0].folder=BULKPRINT"
})
@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@DirtiesContext
class FailingFileTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private LetterRepository repository;

    @Autowired
    private FakeFtpAvailabilityChecker fakeFtpAvailabilityChecker;

    @Autowired
    private WebApplicationContext wac;

    @BeforeEach
    void setUp() {
        var filter = new WebRequestTrackingFilter();
        filter.init(new MockFilterConfig());
        mvc = webAppContextSetup(wac).addFilters(filter).build();
        repository.deleteAll();
    }

    @AfterEach
    public void cleanUp() {
        // This test commits transactions to the database
        // so we must clean up afterwards
        repository.deleteAll();
    }

    @Test
    void shouldHandleUploadFailure() throws Throwable {
        shouldHandleCorruptedLetter(
                post("/letters")
                        .header("ServiceAuthorization", "auth-header-value")
                        .contentType(MediaTypes.LETTER_V2)
                        .content(readResource("letter-with-pdf.json"))
        );
    }

    private void shouldHandleCorruptedLetter(MockHttpServletRequestBuilder request) throws Throwable {
        try (var server = LocalSftpServer.create()) {

            // sftp servers is ups, now the background jobs can start connecting to it
            fakeFtpAvailabilityChecker.setAvailable(true);

            mvc.perform(request)
                    .andExpect(status().isOk())
                    .andReturn();

            await()
                    .forever()
                    .untilAsserted(() -> {
                        List<Letter> letters = repository.findAll();
                        assertThat(letters).as("Letters in DB").hasSize(1);
                    });

            corruptFileInDataBase();

            // The report should be processed and the letter marked posted.
            await()
                    .forever()
                    .untilAsserted(() -> {
                        List<Letter> letters = repository.findAll();
                        assertThat(letters).as("Letters in DB").hasSize(1);
                        assertThat(letters.get(0).getStatus()).as("Letter status").isEqualTo(FailedToUpload);
                    });
        }
    }

    private void corruptFileInDataBase() {
        // The interval between upload-letters tasks is 30 seconds,
        // this gives enough time to corrupt letter in db by making fileContent null
        List<Letter> savedLetters = repository.findAll();
        Letter savedLetter = savedLetters.get(0);
        // making fileContent null causes NPE when file is being uploaded
        savedLetter.setFileContent(null);
        repository.saveAndFlush(savedLetter);
    }

    String readResource(final String fileName) throws IOException {
        return StreamUtils.copyToString(
                new ClassPathResource(fileName).getInputStream(), UTF_8);
    }
}
