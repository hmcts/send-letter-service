package uk.gov.hmcts.reform.sendletter;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobClientBuilder;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.model.out.PrintUploadInfo;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

import static com.google.common.io.Resources.getResource;
import static javax.servlet.http.HttpServletResponse.SC_CONFLICT;

@ExtendWith(SpringExtension.class)
public class PrintMessageTest extends FunctionalTestSuite {

    private static final String destDirectory = "/var/tmp";
    private ObjectMapper objectMapper;

    @BeforeEach
    void beforeEach() {
        objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
    }

    @Test
    void should_responsed_with_upload_information_when_request_is_valid() throws IOException {
        String requestTemplate = Resources.toString(getResource("print_job_case_id_dynamic.json"), Charsets.UTF_8);
        String caseId = UUID.randomUUID().toString().replace("-","");
        String requestBody = requestTemplate.replace("{{case_id}}", caseId);

        String response = putPrintRequest(
            requestBody,
            "/print-jobs/{id}",
            UUID.randomUUID().toString(),
            200
        );

        PrintResponse printResponse = objectMapper.readValue(response, PrintResponse.class);

        printResponse.printJob.documents.stream()
            .forEach(
                document -> {
                    String filePath = String.join(
                        "/", "src/functionalTest/resources",
                        document.fileName
                    );
                    BlobClient blobClient = getBlobCLient(printResponse.printUploadInfo, document.uploadToPath);
                    blobClient.uploadFromFile(filePath);
                }
            );

        String uploadDetails = objectMapper.writeValueAsString(printResponse);
        byte[] data = uploadDetails.getBytes(StandardCharsets.UTF_8);

        BlobClient blobCLient = getBlobCLient(
            printResponse.printUploadInfo,
            printResponse.printUploadInfo.manifestPath
            );
        blobCLient.upload(new ByteArrayInputStream(data), data.length);
    }

    private BlobClient getBlobCLient(PrintUploadInfo printUploadInfo,
                              String blobName) {
        return new BlobClientBuilder()
            .endpoint(printUploadInfo.uploadToContainer)
            .sasToken(printUploadInfo.sasToken)
            .blobName(blobName)
            .buildClient();
    }

    @Test
    void should_responsed_with_duplicate_exception_when_duplicate_requests() throws IOException {
        String requestTemplate = Resources.toString(getResource("print_job_case_id_dynamic.json"), Charsets.UTF_8);
        String caseId = UUID.randomUUID().toString().replace("-","");
        String requestBody = requestTemplate.replace("{{case_id}}", caseId);

        putPrintRequest(
            requestBody,
            "/print-jobs/{id}",
            UUID.randomUUID().toString(),
            200
        );

        putPrintRequest(
            requestBody,
            "/print-jobs/{id}",
            UUID.randomUUID().toString(),
            SC_CONFLICT
        );
    }

    @Override
    String getContentType() {
        return MediaTypes.PRINT_V1;
    }
}
