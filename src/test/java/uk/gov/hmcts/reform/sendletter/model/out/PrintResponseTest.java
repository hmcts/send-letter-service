package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.google.common.io.Resources;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.entity.PrintStatus;
import uk.gov.hmcts.reform.sendletter.model.Document;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.assertj.core.api.Assertions.tuple;

class PrintResponseTest {

    @Test
    void should_serialize_when_request_parsed() throws IOException {
        String json = Resources.toString(getResource("print_job_response.json"), UTF_8);

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        PrintResponse printRequest = objectMapper.readValue(json, PrintResponse.class);

        PrintJob printJob = printRequest.printJob;
        assertThat(printJob.id)
            .isEqualTo(UUID.fromString("33dffc2f-94e0-4584-a973-cc56849ecc0b"));
        assertThat(printJob.createdAt)
            .isEqualTo(LocalDateTime.parse("2021-04-07T10:03:00"));
        assertThat(printJob.printedAt)
            .isEqualTo(LocalDateTime.parse("2021-04-08T11:03:00"));
        assertThat(printJob.sentToPrintAt)
            .isEqualTo(LocalDateTime.parse("2021-04-09T12:03:00"));
        assertThat(printJob.service)
            .isEqualTo("sscs");
        assertThat(printJob.printStatus)
            .isEqualTo(PrintStatus.PROCESSED);

        List<Document> documents = printJob.documents;
        assertThat(documents)
            .as("documents list")
            .extracting("fileName", "copies")
            .contains(
                tuple("mypdf.pdf", 2),
                tuple("1.pdf", 1)
            );

        assertThat(printJob.additionalData)
            .as("Addition data")
            .contains(
                entry("x", "y")
            );

        PrintUploadInfo printUploadInfo = printRequest.printUploadInfo;
        assertThat(printUploadInfo.uploadToContainer)
            .isEqualTo("url");
        assertThat(printUploadInfo.sasToken)
            .isEqualTo("?sas=sadas56tfuvydasd");
        assertThat(printUploadInfo.uploadToPath)
            .isEqualTo("/sscs/33dffc2f-94e0-4584-a973-cc56849ecc0b/file1-c10.pdf");
    }

    @Test
    void should_set_all_fields_when_intialised_with_values() {

        List<Document> documents = List.of(
            new Document("mypdf.pdf", 2),
            new Document("1.pdf", 1)
        );

        Map<String, String> additionalData = Map.of("x", "y");
        UUID uuid = UUID.randomUUID();
        LocalDateTime createAt = LocalDateTime.now();
        LocalDateTime printedAt = createAt.plusDays(1);
        LocalDateTime sentToPrint = createAt.plusDays(2);

        PrintResponse response = new PrintResponse(
            new PrintJob(
                uuid,
                createAt,
                printedAt,
                sentToPrint,
                "sscs",
                PrintStatus.ZIPPED,
                documents,
                additionalData
            ),
            new PrintUploadInfo(
                "url",
                "token",
                "path"
            )
        );

        PrintJob printJob = response.printJob;
        assertThat(printJob).isNotNull();
        assertThat(printJob.id)
            .isEqualTo(uuid);
        assertThat(printJob.createdAt)
            .isEqualTo(createAt);
        assertThat(printJob.printedAt)
            .isEqualTo(printedAt);
        assertThat(printJob.sentToPrintAt)
            .isEqualTo(sentToPrint);
        assertThat(printJob.service)
            .isEqualTo("sscs");
        assertThat(printJob.printStatus)
            .isEqualTo(PrintStatus.ZIPPED);

        assertThat(printJob.documents)
            .as("documents list")
            .extracting("fileName", "copies")
            .contains(
                tuple("mypdf.pdf", 2),
                tuple("1.pdf", 1)
            );

        PrintUploadInfo printUploadInfo = response.printUploadInfo;
        assertThat(printUploadInfo).isNotNull();
        assertThat(printUploadInfo.uploadToContainer)
            .isEqualTo("url");
        assertThat(printUploadInfo.sasToken)
            .isEqualTo("token");
        assertThat(printUploadInfo.uploadToPath)
            .isEqualTo("path");
    }
}
