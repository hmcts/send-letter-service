package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import uk.gov.hmcts.reform.sendletter.config.AccessTokenProperties;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintJob;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.model.out.PrintUploadInfo;
import uk.gov.hmcts.reform.sendletter.util.TestStorageHelper;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

@SpringBootTest
public class PrintServiceTest {

    @Autowired
    private PrintRepository printRepository;
    @Autowired
    private AccessTokenProperties accessTokenProperties;
    private PrintService printService;
    private ObjectMapper mapper = new ObjectMapper();
    private SasTokenGeneratorService sasTokenGeneratorService;

    @BeforeAll
    public static void initializeStorage() {
        TestStorageHelper.initialize();
    }

    @AfterAll
    public static void tearDownContainer() {
        TestStorageHelper.stopDocker();
    }

    @BeforeEach
    void beforeEach() {
        TestStorageHelper.getInstance().createBulkprintContainer();
        printRepository.deleteAll();
        sasTokenGeneratorService = new SasTokenGeneratorService(
            TestStorageHelper.blobServiceClient,
            accessTokenProperties
            );
        printService = new PrintService(printRepository, mapper, sasTokenGeneratorService);
    }


    @AfterEach
    void afterEach() {
        printRepository.deleteAll();
        TestStorageHelper.getInstance().deleteBulkprintContainer();
    }

    @Test
    void should_save_print_request_when_request_is_valid() throws IOException {
        String json = Resources.toString(getResource("print_job.json"), UTF_8);
        String service = "sscs";
        UUID uuid = UUID.randomUUID();


        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);
        String idempotencyKey = LetterChecksumGenerator.generateChecksum(printRequest);

        PrintResponse printResponse = printService.save(uuid.toString(), service, printRequest);

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
            .isEqualTo("http://localhost:10000/devstoreaccount1/new-sscs");
        String token = printUploadInfo.sasToken;
        Map<String, String> tokenData = Arrays.stream(token.split("&"))
            .map(data -> {
                String[] split = data.split("=");
                return Map.entry(split[0], split[1]);
            })
            .collect(Collectors.toMap(Map.Entry::getKey,
                Map.Entry::getValue));

        assertThat(tokenData.get("sig")).isNotNull();//this is a generated hash of the resource string
        assertThat(tokenData.get("se")).startsWith(LocalDate.now().toString());//the expiry date/time for the signature
        assertThat(tokenData.get("sv")).contains("2021-08-06");//azure api version is latest
        assertThat(tokenData.get("sp")).contains("rwl");//access permissions(write-w,list-l)
        assertThat(tokenData.get("sr")).isNotNull();

        assertThat(printUploadInfo.manifestPath)
            .isEqualTo(
                String.format(
                    "manifest-/%s-%s.json",
                    uuid, service
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
