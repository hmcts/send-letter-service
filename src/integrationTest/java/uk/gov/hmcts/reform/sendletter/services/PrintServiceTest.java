package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;

import java.io.IOException;
import java.time.LocalDate;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class PrintServiceTest {

    @Autowired
    private PrintRepository printRepository;
    private PrintService printService;
    private ObjectMapper mapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        printService = new PrintService(printRepository, mapper);
        printRepository.deleteAll();
    }

    @Test
    void should_save_print_request_when_request_is_valid() throws IOException {
        String json = Resources.toString(getResource("print_job.json"), UTF_8);
        String service = "sscs";
        String idempotencyKey = "idempotencyKey";

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);

        printService.save(service, printRequest, idempotencyKey);

        Print print = printRepository.findAll().get(0);
        assertThat(print.getId())
            .isNotNull();
        assertThat(print.getDocuments())
            .isEqualTo(getDocuments());
        assertThat(print.getService())
            .isEqualTo("sscs");
        assertThat(print.getCreatedAt().toLocalDate())
            .isEqualTo(LocalDate.now());
        assertThat(print.getType())
            .isEqualTo("SSC001");
        assertThat(print.getIdempotencyKey())
            .isEqualTo("idempotencyKey");
        assertThat(print.getCaseId())
            .isEqualTo("12345");
        assertThat(print.getCaseRef())
            .isEqualTo("162MC066");
        assertThat(print.getLetterType())
            .isEqualTo("first-contact-pack");
        assertThat(print.getStatus())
            .isEqualTo(PrintStatus.NEW);
        assertThat(print.getSentToPrintAt())
            .isNull();
        assertThat(print.getPrintedAt())
            .isNull();
        assertThat(print.isFailed())
            .isFalse();
    }

    private JsonNode getDocuments() throws JsonProcessingException {
        return mapper.readTree(
            "[{\n"
                + "\"file_name\": \"1.pdf\",\n"
                + "\"copies_required\": 2\n"
                + "},\n"
                + "{\n"
                + "\"file_name\": \"2.pdf\",\n"
                + "\"copies_required\": 1\n"
                + " }\n"
                + "]");
    }

}
