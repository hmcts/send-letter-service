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
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.StreamUtils;
import org.springframework.web.context.WebApplicationContext;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.helper.FakeFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.LocalSftpServer;

import java.io.IOException;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.webAppContextSetup;

@SpringBootTest
@TestPropertySource(properties = {
        "scheduling.enabled=true",
        "tasks.upload-letters.interval-ms=20000",
        "tasks.mark-letters-posted.cron=*/1 * * * * *",
        "tasks.stale-letters-report.cron=*/1 * * * * *",
        "ftp.service-folders[0].service=some_service_name",
        "ftp.service-folders[0].folder=BULKPRINT",
        "ftp.service-folders[0].enabled=false"
})
@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@DirtiesContext
class LetterUploadForDisabledServiceTest {

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
    void shouldHandleEmptyFileUploadsOldLetterModel() throws Throwable {
        try (var server = LocalSftpServer.create()) {

            // sftp servers is ups, now the background jobs can start connecting to it
            fakeFtpAvailabilityChecker.setAvailable(true);

            MvcResult mvcResult = mvc.perform(
                    post("/letters")
                            .header("ServiceAuthorization", "auth-header-value")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(readResource("letter.json"))
            ).andReturn();

            assertThat(mvcResult.getResponse().getStatus()).isEqualTo(403);
        }
    }

    private String readResource(final String fileName) throws IOException {
        return StreamUtils.copyToString(
                new ClassPathResource(fileName).getInputStream(), UTF_8);
    }
}
