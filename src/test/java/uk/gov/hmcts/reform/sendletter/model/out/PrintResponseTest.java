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
        assertThat(printJob.printedAt).isNull();
        assertThat(printJob.sentToPrintAt).isNull();
        assertThat(printJob.service)
            .isEqualTo("sscs");
        assertThat(printJob.printStatus)
            .isEqualTo(PrintStatus.New);

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
}
