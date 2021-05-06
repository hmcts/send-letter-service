package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;

import java.io.IOException;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class PrintServiceTest {
    @Mock
    private PrintRepository repository;

    @Captor
    private ArgumentCaptor<Print> printArgumentCaptor;

    private PrintService printService;

    private ObjectMapper mapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        printService = new PrintService(repository, mapper);
    }

    @Test
    void should_save_print_request_when_request_is_valid() throws IOException {
        String json = Resources.toString(getResource("print_job.json"), UTF_8);
        String service = "sscs";
        String idempotencyKey = "idempotencyKey";

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);

        printService.save(service, printRequest, idempotencyKey);

        verify(repository).save(printArgumentCaptor.capture());

        Print result = printArgumentCaptor.getValue();
        assertThat(result.getId())
            .isNotNull();
        assertThat(result.getDocuments())
            .isEqualTo(getDocuments());
        assertThat(result.getService())
            .isEqualTo("sscs");
        assertThat(result.getCreatedAt())
            .isNotNull();
        assertThat(result.getType())
            .isEqualTo("SSC001");
        assertThat(result.getIdempotencyKey())
            .isEqualTo("idempotencyKey");
        assertThat(result.getCaseId())
            .isEqualTo("12345");
        assertThat(result.getCaseRef())
            .isEqualTo("162MC066");
        assertThat(result.getLetterType())
            .isEqualTo("first-contact-pack");
        assertThat(result.getStatus())
            .isEqualTo(PrintStatus.NEW);
        assertThat(result.getSentToPrintAt())
            .isNull();
        assertThat(result.getPrintedAt())
            .isNull();
        assertThat(result.isFailed())
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
