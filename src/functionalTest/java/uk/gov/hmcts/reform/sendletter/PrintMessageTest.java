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

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;

import static com.google.common.io.Resources.getResource;

@ExtendWith(SpringExtension.class)
public class PrintMessageTest extends FunctionalTestSuite {

    private static String destDirectory = "/var/tmp";
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
                    upload(printResponse.printUploadInfo, document.uploadToPath, filePath);
                }
            );

        String manifestPath = printResponse.printUploadInfo.manifestPath;
        String[] fileDetails = manifestPath.split("/");
        String uploadDetails = objectMapper.writeValueAsString(printResponse);
        String filePath = String.join("/", destDirectory, fileDetails[1]);

        Path path = Paths.get(filePath);
        Files.write(path, uploadDetails.getBytes(StandardCharsets.UTF_8));

        upload(
            printResponse.printUploadInfo,
            printResponse.printUploadInfo.manifestPath,
            filePath
        );
    }

    private void upload(PrintUploadInfo printUploadInfo,
                        String blobName,
                        String filePath
    ) {
        BlobClient blobClient = new BlobClientBuilder()
            .endpoint(printUploadInfo.uploadToContainer)
            .sasToken(printUploadInfo.sasToken)
            .blobName(blobName)
            .buildClient();
        blobClient.uploadFromFile(filePath);
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
            409
        );
    }

    @Override
    String getContentType() {
        return MediaTypes.PRINT_V1;
    }
}
