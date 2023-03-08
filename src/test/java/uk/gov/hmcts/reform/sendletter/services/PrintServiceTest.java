package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import uk.gov.hmcts.reform.sendletter.model.out.PrintJob;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.model.out.PrintUploadInfo;

import java.time.LocalDateTime;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadJson;

@ExtendWith(MockitoExtension.class)
class PrintServiceTest {
    @Mock
    private PrintRepository repository;
    @Mock
    private SasTokenGeneratorService sasTokenGeneratorService;

    @Captor
    private ArgumentCaptor<Print> printArgumentCaptor;
    private PrintService printService;
    private final ObjectMapper mapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        printService = new PrintService(repository, mapper, sasTokenGeneratorService);
    }

    @Test
    void should_save_print_request_when_request_is_valid() throws Exception {
        String json = loadJson("print_job.json");
        String service = "sscs";
        UUID uuid = UUID.randomUUID();

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);
        String idempotencyKey = ChecksumGenerator.generateChecksum(printRequest);
        given(repository.save(isA(Print.class)))
            .willReturn(
                new Print(
                    uuid,
                    service,
                    LocalDateTime.now(),
                    printRequest.type,
                    idempotencyKey,
                    mapper.valueToTree(printRequest.documents),
                    printRequest.caseId,
                    printRequest.caseRef,
                    printRequest.letterType
                )
            );
        String accountUrl = "https://blobstoreurl.com";
        given(sasTokenGeneratorService.getAccountUrl())
            .willReturn(accountUrl);

        String sasToken = "?sas=sadas56tfuvydasd";
        given(sasTokenGeneratorService.generateSasToken(service))
            .willReturn(sasToken);

        String containerName = "new-sscs";
        given(sasTokenGeneratorService.getContainerName(service))
            .willReturn(containerName);

        printService.save(
            uuid.toString(),
            service,
            printRequest
        );

        verify(sasTokenGeneratorService).generateSasToken(service);
        verify(sasTokenGeneratorService).getAccountUrl();
        verify(sasTokenGeneratorService).getContainerName(service);

        verify(repository).save(printArgumentCaptor.capture());

        Print result = printArgumentCaptor.getValue();
        assertThat(result.getId())
            .isEqualTo(uuid);
        assertThat(result.getDocuments())
            .isEqualTo(getDocuments());
        assertThat(result.getService())
            .isEqualTo("sscs");
        assertThat(result.getCreatedAt())
            .isNotNull();
        assertThat(result.getType())
            .isEqualTo("SSC001");
        assertThat(result.getIdempotencyKey())
            .isEqualTo(idempotencyKey);
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

    @Test
    void should_return_print_response_when_request_is_valid() throws Exception {
        String json = loadJson("print_job.json");
        String service = "sscs";

        UUID uuid = UUID.randomUUID();

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);
        String idempotencyKey = ChecksumGenerator.generateChecksum(printRequest);
        given(repository.save(isA(Print.class)))
            .willReturn(
                new Print(
                    uuid,
                    service,
                    LocalDateTime.now(),
                    printRequest.type,
                    idempotencyKey,
                    mapper.valueToTree(printRequest.documents),
                    printRequest.caseId,
                    printRequest.caseRef,
                    printRequest.letterType
                )
            );
        String accountUrl = "https://blobstoreurl.com";
        given(sasTokenGeneratorService.getAccountUrl())
            .willReturn(accountUrl);

        String sasToken = "?sas=sadas56tfuvydasd";
        given(sasTokenGeneratorService.generateSasToken(service))
            .willReturn(sasToken);

        String containerName = "new-sscs";
        given(sasTokenGeneratorService.getContainerName(service))
            .willReturn(containerName);

        PrintResponse printResponse = printService.save(
            uuid.toString(),
            service,
            printRequest
        );

        verify(sasTokenGeneratorService).generateSasToken(service);
        verify(sasTokenGeneratorService).getAccountUrl();
        verify(sasTokenGeneratorService).getContainerName(service);

        verify(repository).save(isA(Print.class));

        PrintUploadInfo printUploadInfo = printResponse.printUploadInfo;
        assertThat(printUploadInfo.uploadToContainer)
            .isEqualTo("https://blobstoreurl.com/new-sscs");
        assertThat(printUploadInfo.sasToken)
            .isEqualTo(sasToken);
        assertThat(printUploadInfo.manifestPath)
            .isEqualTo(
                String.format(
                    "manifest-/%s-%s.json",
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

    @Test
    void shouldThrowJsonProcessingException() throws Exception {
        var service = "sscs";
        var letterId = UUID.randomUUID().toString();

        ObjectMapper mockMapper = mock(ObjectMapper.class);
        PrintRequest mockRequest = mock(PrintRequest.class);

        JsonProcessingException mockException = mock(JsonProcessingException.class);
        given(mockMapper.writeValueAsString(any())).willThrow(mockException);

        PrintService mockService = new PrintService(repository, mockMapper, sasTokenGeneratorService);

        assertThatThrownBy(() -> mockService.save(
            letterId,
            service,
            mockRequest
        )).isInstanceOf(NullPointerException.class);
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
