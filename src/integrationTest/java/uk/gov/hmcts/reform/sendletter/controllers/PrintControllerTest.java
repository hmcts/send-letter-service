package uk.gov.hmcts.reform.sendletter.controllers;

import com.google.common.io.Resources;
import com.microsoft.applicationinsights.web.internal.WebRequestTrackingFilter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockFilterConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.context.WebApplicationContext;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.SasTokenGeneratorService;

import java.util.UUID;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.webAppContextSetup;
import static org.springframework.util.DigestUtils.md5DigestAsHex;
import static org.springframework.util.SerializationUtils.serialize;

@AutoConfigureMockMvc
@SpringBootTest
public class PrintControllerTest {
    @MockBean
    private AuthService authService;
    @MockBean
    private SasTokenGeneratorService sasTokenGeneratorService;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private WebApplicationContext wac;

    @BeforeEach
    void setUp() {
        WebRequestTrackingFilter filter = new WebRequestTrackingFilter();
        filter.init(new MockFilterConfig());
        mockMvc = webAppContextSetup(wac).addFilters(filter).build();
    }

    @Test
    void should_responsed_when_request_is_valid() throws Exception {
        String serviceName = "sscs";
        String serviceAuthorization = "ServiceAuthorization";
        given(authService.authenticate(serviceAuthorization))
            .willReturn(serviceName);
        String accountUrl = "https://rpesendletterdemo.blob.core.windows.net";
        given(sasTokenGeneratorService.getAccountUrl())
            .willReturn(accountUrl);
        given(sasTokenGeneratorService.getContainerName(serviceName))
            .willReturn("new-sscs");
        String sasTokenForNewSscsContainer = "sasTokenForNewSscsContainer";
        given(sasTokenGeneratorService.generateSasToken(serviceName))
            .willReturn(sasTokenForNewSscsContainer);
        String type = "SSC001";
        UUID uuid = UUID.randomUUID();
        String idempotencyKey = md5DigestAsHex(serialize(uuid));

        String requestJson = Resources.toString(getResource("print_job.json"), UTF_8);

        mockMvc.perform(put("/print-jobs/{id}", uuid)
            .header("ServiceAuthorization", serviceAuthorization)
            .header("X-Hash", idempotencyKey)
            .contentType(MediaTypes.PRINT_V1)
            .content(requestJson))
            .andDo(print())
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.print_job.id")
                .value(uuid.toString()))
            .andExpect(jsonPath("$.print_job.service")
                .value(serviceName))
            .andExpect(jsonPath("$.print_job.type")
                .value(type))
            .andExpect(jsonPath("$.print_job.container_name")
                .value("new-sscs"))
            .andExpect(jsonPath("$.print_job.documents[0].file_name")
                .value("1.pdf"))
            .andExpect(jsonPath("$.print_job.documents[0].upload_to_path")
                .value(
                    String.join("-",
                        uuid.toString(),
                        serviceName,
                        type,
                        "1.pdf"
                    )
                ))
            .andExpect(jsonPath("$.print_job.documents[0].copies_required")
                .value(2))
            .andExpect(jsonPath("$.print_job.documents[1].file_name")
                .value("2.pdf"))
            .andExpect(jsonPath("$.print_job.documents[1].upload_to_path")
                .value(
                    String.join("-",
                        uuid.toString(),
                        serviceName,
                        type,
                        "2.pdf"
                    )
                ))
            .andExpect(jsonPath("$.print_job.case_id")
                .value("12345"))
            .andExpect(jsonPath("$.print_job.case_ref")
                .value("162MC066"))
            .andExpect(jsonPath("$.print_job.letter_type")
                .value("first-contact-pack"))
            .andExpect(jsonPath("$.upload.upload_to_container")
                .value("https://rpesendletterdemo.blob.core.windows.net/new-sscs"))
            .andExpect(jsonPath("$.upload.sas")
                .value(sasTokenForNewSscsContainer))
            .andExpect(jsonPath("$.upload.manifest_path")
                .value(
                    String.join("-",
                        "manifest",
                        uuid.toString(),
                        "sscs.json"
                    )
                ));
    }

    @Test
    void should_fail_validation_when_type_and_documents_missing() throws Exception {
        String type = "SSC001";
        UUID uuid = UUID.randomUUID();
        String idempotencyKey = md5DigestAsHex(serialize(uuid));

        String requestJson = Resources.toString(getResource("print_job_type_documents_missing.json"), UTF_8);
        String serviceAuthorization = "ServiceAuthorization";
        mockMvc.perform(put("/print-jobs/{id}", uuid)
            .header("ServiceAuthorization", serviceAuthorization)
            .header("X-Hash", idempotencyKey)
            .contentType(MediaTypes.PRINT_V1)
            .content(requestJson))
            .andDo(print())
            .andExpect(status().isBadRequest())
            .andExpect(jsonPath("$.errors[0].field_name")
                .isNotEmpty())
            .andExpect(jsonPath("$.errors[0].message")
                .value("must not be empty"))
            .andExpect(jsonPath("$.errors[1].field_name")
                .isNotEmpty())
            .andExpect(jsonPath("$.errors[1].message")
                .value("must not be empty"));
    }
}
