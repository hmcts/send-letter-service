package uk.gov.hmcts.reform.sendletter.controllers;

import com.google.common.io.Resources;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.config.TimeConfiguration;
import uk.gov.hmcts.reform.sendletter.entity.DocumentRepository;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@SpringBootTest
class GetLetterStatusTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private LetterRepository letterRepository;

    @Autowired
    private DocumentRepository documentRepository;

    @MockBean
    private AuthTokenValidator tokenValidator;


    @AfterEach
    void tearDown() {
        documentRepository.deleteAll();
        letterRepository.deleteAll();
    }

    @Test
    void should_return_200_when_matching_letter_found_in_db() throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some-service");

        // and
        Letter letter = SampleData.letterEntity("some-service");
        letterRepository.saveAndFlush(letter);

        getLetterStatus(letter.getId())
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").value(letter.getId().toString()))
            .andExpect(jsonPath("$.status").value(letter.getStatus().name()))
            .andExpect(jsonPath("$.checksum").value(letter.getChecksum()))
            .andExpect(jsonPath("$.created_at").value(toIso(letter.getCreatedAt())))
            .andExpect(jsonPath("$.sent_to_print_at").isEmpty())
            .andExpect(jsonPath("$.printed_at").isEmpty());
    }

    @Test
    void should_return_404_when_letter_is_not_found() throws Exception {
        getLetterStatus(UUID.randomUUID()).andExpect(status().isNotFound());
    }

    @Test
    void should_return_200_when_valid_json_is_sent_with_additionaldata_but_no_recipients() throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some_service_name");

        // no recipients in additional data - FACT-1388 flag means it'll be ok, so 200 unless switched on
        String json = Resources.toString(getResource("letter-with-pdf-no-recipients.json"), UTF_8);
        mvc.perform(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaTypes.LETTER_V2)
                .content(json)
        ).andExpect(status().isBadRequest());
    }

    @Test
    void should_return_200_when_duplicated_document_is_sent_attachments_and_with_recipients() throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some_service_name");

        String letter = Resources.toString(getResource("letter-with-pdf-and-recipients.json"), UTF_8);
        MvcResult result = mvc
            .perform(
                post("/letters")
                    .header("ServiceAuthorization", "auth-header-value")
                    .contentType(MediaTypes.LETTER_V2)
                    .content(letter)
            ).andReturn();

        JSONObject letterResult = new JSONObject(result.getResponse().getContentAsString());
        String letterId = letterResult.getString("letter_id");
        getLetterStatus(letterId)
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").isNotEmpty())
            .andExpect(jsonPath("$.status").value(LetterStatus.Created.name()))
            .andExpect(jsonPath("$.checksum").isNotEmpty())
            .andExpect(jsonPath("$.created_at").isNotEmpty())
            .andExpect(jsonPath("$.sent_to_print_at").isEmpty())
            .andExpect(jsonPath("$.printed_at").isEmpty())
            .andExpect(jsonPath("$.additional_data").doesNotHaveJsonPath());

        String duplicatedLetter = Resources.toString(getResource("letter-with-pdf-duplicate.json"), UTF_8);
        mvc.perform(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaTypes.LETTER_V2)
                .content(duplicatedLetter)
        ).andExpect(status().isOk());
    }

    @Test
    void should_return_400_when_duplicated_document_is_sent_with_no_additional_data() throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some_service_name");

        String letter = Resources.toString(getResource("letter-with-pdf.json"), UTF_8);
        MvcResult result = mvc
            .perform(
                post("/letters")
                    .header("ServiceAuthorization", "auth-header-value")
                    .contentType(MediaTypes.LETTER_V2)
                    .content(letter)
            ).andReturn();

        JSONObject letterResult = new JSONObject(result.getResponse().getContentAsString());
        String letterId = letterResult.getString("letter_id");
        getLetterStatus(letterId)
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").isNotEmpty())
            .andExpect(jsonPath("$.status").value(LetterStatus.Created.name()))
            .andExpect(jsonPath("$.checksum").isNotEmpty())
            .andExpect(jsonPath("$.created_at").isNotEmpty())
            .andExpect(jsonPath("$.sent_to_print_at").isEmpty())
            .andExpect(jsonPath("$.printed_at").isEmpty())
            .andExpect(jsonPath("$.additional_data").doesNotHaveJsonPath());

        String duplicatedLetter = Resources.toString(getResource("letter-with-pdf-duplicate.json"), UTF_8);
        mvc.perform(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaTypes.LETTER_V2)
                .content(duplicatedLetter)
        ).andExpect(status().isOk());
    }

    @Test
    void should_return_200_and_duplicated_letter_id_when_duplicated_document_is_sent_with_recipients()
        throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some_service_name");

        String letter = Resources.toString(getResource("letter-with-pdf-and-recipients.json"), UTF_8);
        MvcResult result = mvc
            .perform(
                post("/letters")
                    .header("ServiceAuthorization", "auth-header-value")
                    .contentType(MediaTypes.LETTER_V2)
                    .content(letter)
            ).andReturn();

        JSONObject letterResult = new JSONObject(result.getResponse().getContentAsString());
        String letterId = letterResult.getString("letter_id");

        mvc.perform(
            get("/letters/" + letterId)
                .param("include-additional-info", "true"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").isNotEmpty())
            .andExpect(jsonPath("$.status").value(LetterStatus.Created.name()))
            .andExpect(jsonPath("$.checksum").isNotEmpty())
            .andExpect(jsonPath("$.created_at").isNotEmpty())
            .andExpect(jsonPath("$.sent_to_print_at").isEmpty())
            .andExpect(jsonPath("$.printed_at").isEmpty())
            .andExpect(jsonPath("$.additional_data.reference").value("ABD-123-WAZ"))
            .andExpect(jsonPath("$.additional_data.count").value(10))
            .andExpect(jsonPath("$.additional_data.additionInfo").value("present"))
            .andExpect(jsonPath("$.additional_data.recipients").isArray());

        // Same request, but the reference is different. Recipients/documents are the same
        String duplicatedLetter =
            Resources.toString(getResource("letter-with-pdf-and-recipients-duplicate.json"), UTF_8);
        mvc.perform(
            post("/letters")
                .header("ServiceAuthorization", "auth-header-value")
                .contentType(MediaTypes.LETTER_V2)
                .content(duplicatedLetter)
        ).andExpect(status().isOk())
            .andExpect(jsonPath("$.letter_id", equalTo(letterId)));
    }

    @Test
    void should_return_200_when_valid_json_is_sent_with_additionaldata() throws Exception {
        // given
        given(tokenValidator.getServiceName("auth-header-value")).willReturn("some_service_name");

        String json = Resources.toString(getResource("letter-with-pdf-additionaldata.json"), UTF_8);
        MvcResult result = mvc
            .perform(
                post("/letters")
                    .header("ServiceAuthorization", "auth-header-value")
                    .contentType(MediaTypes.LETTER_V2)
                    .content(json)
            ).andReturn();
        JSONObject jsonObject = new JSONObject(result.getResponse().getContentAsString());
        String letterId = jsonObject.getString("letter_id");
        mvc.perform(
            get("/letters/" + letterId)
                .param("include-additional-info", "true"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").isNotEmpty())
            .andExpect(jsonPath("$.status").value(LetterStatus.Created.name()))
            .andExpect(jsonPath("$.checksum").isNotEmpty())
            .andExpect(jsonPath("$.created_at").isNotEmpty())
            .andExpect(jsonPath("$.sent_to_print_at").isEmpty())
            .andExpect(jsonPath("$.printed_at").isEmpty())
            .andExpect(jsonPath("$.additional_data.reference").value("ABD-123-WAZ"))
            .andExpect(jsonPath("$.additional_data.count").value(10))
            .andExpect(jsonPath("$.additional_data.additionInfo").value("present"));
    }

    private ResultActions getLetterStatus(UUID id) throws Exception {
        return getLetterStatus(id.toString());
    }

    private ResultActions getLetterStatus(String letterId) throws Exception {
        return mvc.perform(
            get("/letters/" + letterId)
                .header("ServiceAuthorization", "auth-header-value")
        );
    }

    private String toIso(LocalDateTime dateTime) {
        return dateTime
            .atZone(ZoneId.of("UTC"))
            .format(DateTimeFormatter.ofPattern(TimeConfiguration.DATE_TIME_PATTERN));
    }
}
