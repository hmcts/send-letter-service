package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.util.DigestUtils.md5DigestAsHex;
import static org.springframework.util.SerializationUtils.serialize;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class PrintRepositoryTest {
    @Autowired
    private PrintRepository printRepository;

    private static ObjectMapper objectMapper = new ObjectMapper();

    @BeforeEach
    void beforeEach() {
        printRepository.deleteAll();
    }

    @AfterEach
    void afterEach() {
        printRepository.deleteAll();
    }

    @Test
    void should_create_new_record_when_saved() throws JsonProcessingException {
        UUID uuid = UUID.randomUUID();
        String idempotencyKey = md5DigestAsHex(serialize(uuid));

        LocalDateTime currentDateTime = LocalDateTime.now();
        printRepository.save(
            getPrintEntity(
                uuid,
                idempotencyKey,
                currentDateTime
            )
        );
        Optional<Print> print = printRepository.findById(uuid);
        assertThat(print).isPresent();
        assertThat(print.get().getId())
            .isEqualTo(uuid);
        assertThat(print.get().getDocuments())
            .isEqualTo(getDocuments());
        assertThat(print.get().getService())
            .isEqualTo("sscs");
        assertThat(print.get().getCreatedAt())
            .isEqualTo(currentDateTime);
        assertThat(print.get().getType())
            .isEqualTo("sscs_001");
        assertThat(print.get().getIdempotencyKey())
            .isEqualTo(idempotencyKey);
        assertThat(print.get().getCaseId())
            .isEqualTo("caseId");
        assertThat(print.get().getCaseRef())
            .isEqualTo("caseRef");
        assertThat(print.get().getLetterType())
            .isEqualTo("letterType");
        assertThat(print.get().getStatus())
            .isEqualTo(PrintStatus.NEW);
        assertThat(print.get().getSentToPrintAt())
            .isNull();
        assertThat(print.get().getPrintedAt())
            .isNull();
        assertThat(print.get().isFailed())
            .isFalse();
    }

    @Test
    void should_update_sendToPrint_printAt_failed_status_when_saved() throws JsonProcessingException {
        UUID uuid = UUID.randomUUID();
        String idempotencyKey = md5DigestAsHex(serialize(uuid));
        LocalDateTime currentDateTime = LocalDateTime.now();
        printRepository.save(
            getPrintEntity(
                uuid,
                idempotencyKey,
                currentDateTime
            )
        );
        LocalDateTime sentToPrintAt = LocalDateTime.now();
        LocalDateTime printAt = sentToPrintAt.plusDays(1);
        Print updatedPrint = printRepository.findAll().get(0);
        updatedPrint.setFailed(true);
        updatedPrint.setSentToPrintAt(sentToPrintAt);
        updatedPrint.setPrintedAt(printAt);
        updatedPrint.setStatus(PrintStatus.PROCESSED);

        printRepository.save(updatedPrint);

        Optional<Print> print = printRepository.findById(uuid);
        assertThat(print).isPresent();
        assertThat(print.get().getId())
            .isEqualTo(uuid);
        assertThat(print.get().getDocuments())
            .isEqualTo(getDocuments());
        assertThat(print.get().getService())
            .isEqualTo("sscs");
        assertThat(print.get().getCreatedAt())
            .isEqualTo(currentDateTime);
        assertThat(print.get().getType())
            .isEqualTo("sscs_001");
        assertThat(print.get().getIdempotencyKey())
            .isEqualTo(idempotencyKey);
        assertThat(print.get().getCaseId())
            .isEqualTo("caseId");
        assertThat(print.get().getCaseRef())
            .isEqualTo("caseRef");
        assertThat(print.get().getLetterType())
            .isEqualTo("letterType");
        assertThat(print.get().getStatus())
            .isEqualTo(PrintStatus.PROCESSED);
        assertThat(print.get().getSentToPrintAt())
            .isEqualTo(sentToPrintAt);
        assertThat(print.get().getPrintedAt())
            .isEqualTo(printAt);
        assertThat(print.get().isFailed())
            .isTrue();
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

    private Print getPrintEntity(UUID uuid,
                                 String idempotencyKey,
                                 LocalDateTime currentDateTime)
        throws JsonProcessingException {
        return SampleData.printEntity(
            uuid,
            "sscs",
            currentDateTime,
            "sscs_001",
            idempotencyKey,
            getDocuments(),
            "caseId",
            "caseRef",
            "letterType"
        );
    }
}
