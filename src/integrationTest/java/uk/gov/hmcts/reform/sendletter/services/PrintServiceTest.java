package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintJob;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.model.out.PrintUploadInfo;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.BDDMockito.given;
import static org.springframework.util.DigestUtils.md5DigestAsHex;
import static org.springframework.util.SerializationUtils.serialize;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class PrintServiceTest {

    @Autowired
    private PrintRepository printRepository;
    private PrintService printService;
    private ObjectMapper mapper = new ObjectMapper();
    @Mock
    private SasTokenGeneratorService sasTokenGeneratorService;

    @BeforeEach
    void setUp() {
        printRepository.deleteAll();
        printService = new PrintService(printRepository, mapper, sasTokenGeneratorService);
    }

    @AfterEach
    void afterEach() {
        printRepository.deleteAll();
    }

    @Test
    void should_save_print_request_when_request_is_valid() throws IOException {
        String json = Resources.toString(getResource("print_job.json"), UTF_8);
        String service = "sscs";
        UUID uuid = UUID.randomUUID();
        String idempotencyKey = md5DigestAsHex(serialize(uuid));

        String accountUrl = "https://blobstoreurl.com";
        given(sasTokenGeneratorService.getAccountUrl())
            .willReturn(accountUrl);

        String sasToken = "?sas=sadas56tfuvydasd";
        given(sasTokenGeneratorService.generateSasToken(service))
            .willReturn(sasToken);

        String containerName = "new-sscs";
        given(sasTokenGeneratorService.getContainerName(service))
            .willReturn(containerName);

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);

        PrintResponse printResponse = printService.save(uuid.toString(), service, printRequest, idempotencyKey);

        Optional<Print> print = printRepository.findById(uuid);
        assertThat(print).isPresent();
        assertThat(print.get().getId())
            .isEqualTo(uuid);
        assertThat(print.get().getDocuments())
            .isEqualTo(getDocuments());
        assertThat(print.get().getService())
            .isEqualTo("sscs");
        assertThat(print.get().getCreatedAt().toLocalDate())
            .isEqualTo(LocalDate.now());
        assertThat(print.get().getType())
            .isEqualTo("SSC001");
        assertThat(print.get().getIdempotencyKey())
            .isEqualTo(idempotencyKey);
        assertThat(print.get().getCaseId())
            .isEqualTo("12345");
        assertThat(print.get().getCaseRef())
            .isEqualTo("162MC066");
        assertThat(print.get().getLetterType())
            .isEqualTo("first-contact-pack");
        assertThat(print.get().getStatus())
            .isEqualTo(PrintStatus.NEW);
        assertThat(print.get().getSentToPrintAt())
            .isNull();
        assertThat(print.get().getPrintedAt())
            .isNull();
        assertThat(print.get().isFailed())
            .isFalse();

        PrintUploadInfo printUploadInfo = printResponse.printUploadInfo;
        assertThat(printUploadInfo.uploadToContainer)
            .isEqualTo("https://blobstoreurl.com/new-sscs");
        assertThat(printUploadInfo.sasToken)
            .isEqualTo(sasToken);
        assertThat(printUploadInfo.manifestPath)
            .isEqualTo(
                String.format(
                    "manifest-%s-%s.json",
                    uuid.toString(), service
                )
            );

        PrintJob printJob = printResponse.printJob;

        assertThat(printJob.id)
            .isEqualTo(uuid);

        assertThat(printJob.documents)
            .extracting("fileName", "uploadToPath", "copies")
            .contains(
                tuple(
                    "1.pdf",
                    String.format("%s-%s-%s-1.pdf", printJob.id, service, printJob.type),
                    2
                ),
                tuple(
                    "2.pdf",
                    String.format("%s-%s-%s-2.pdf", printJob.id, service, printJob.type),
                    1
                )
            );

        assertThat(printJob.service)
            .isEqualTo("sscs");
        assertThat(printJob.createdAt)
            .isNotNull();
        assertThat(printJob.type)
            .isEqualTo("SSC001");
        assertThat(printJob.caseId)
            .isEqualTo("12345");
        assertThat(printJob.caseRef)
            .isEqualTo("162MC066");
        assertThat(printJob.letterType)
            .isEqualTo("first-contact-pack");
        assertThat(printJob.printStatus)
            .isEqualTo(PrintStatus.NEW);
        assertThat(printJob.sentToPrintAt)
            .isNull();
        assertThat(printJob.printedAt)
            .isNull();
        assertThat(printJob.type)
            .isEqualTo("SSC001");
        assertThat(printJob.containerName)
            .isEqualTo("new-sscs");
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
