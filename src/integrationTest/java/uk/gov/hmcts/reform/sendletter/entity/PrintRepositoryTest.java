package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.LocalDateTime;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class PrintRepositoryTest {
    @Autowired
    private PrintRepository printRepository;

    private static ObjectMapper objectMapper = new ObjectMapper();

    @Test
    void should_create_new_record_when_saved() throws JsonProcessingException {
        UUID uuid = UUID.randomUUID();
        LocalDateTime currentDateTime = LocalDateTime.now();
        printRepository.save(
            getPrintEntity(
                uuid,
                currentDateTime
            )
        );
        Print print = printRepository.findAll().get(0);
        assertThat(print.getId()).isEqualTo(uuid);
        assertThat(print.getDocuments()).isEqualTo(getDocuments());
        assertThat(print.getService()).isEqualTo("sscs");
        assertThat(print.getCreatedAt()).isEqualTo(currentDateTime);
        assertThat(print.getType()).isEqualTo("sscs_001");
        assertThat(print.getIdempotencyKey()).isEqualTo("idempotencyKey");
        assertThat(print.getCaseId()).isEqualTo("caseId");
        assertThat(print.getCaseRef()).isEqualTo("caseRef");
        assertThat(print.getLetterType()).isEqualTo("letterType");
        assertThat(print.getStatus()).isEqualTo(PrintStatus.NEW);
        assertThat(print.getSentToPrintAt()).isNull();
        assertThat(print.getPrintedAt()).isNull();
        assertThat(print.isFailed()).isFalse();
    }

    private JsonNode getDocuments() throws JsonProcessingException {
        return objectMapper.readTree(
            "[{\n"
                + "\"file_name\": \"mypdf.pdf\",\n"
                + "\"copies_required\": 2\n"
                + "},\n"
                + "{\n"
                + "\"file_name\": \"1.pdf\",\n"
                + "\"copies_required\": 1\n"
                + " }\n"
                + "]");
    }

    private Print getPrintEntity(UUID uuid, LocalDateTime currentDateTime) throws JsonProcessingException {
        return SampleData.printEntity(
            uuid,
            "sscs",
            currentDateTime,
            "sscs_001",
            "idempotencyKey",
            getDocuments(),
            "caseId",
            "caseRef",
            "letterType"
        );
    }
}
