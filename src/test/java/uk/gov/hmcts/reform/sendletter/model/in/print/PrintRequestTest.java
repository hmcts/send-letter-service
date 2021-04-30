package uk.gov.hmcts.reform.sendletter.model.in.print;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.model.Document;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;

import java.io.IOException;
import java.util.List;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;


class PrintRequestTest {

    @Test
    void should_serialize_when_request_parsed() throws IOException {
        String json = Resources.toString(getResource("print_job.json"), UTF_8);

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);

        assertThat(printRequest.caseId)
            .isEqualTo("12345");

        assertThat(printRequest.caseRef)
            .isEqualTo("162MC066");

        assertThat(printRequest.letterType)
            .isEqualTo("first-contact-pack");

        assertThat(printRequest.documents)
            .as("Document list")
            .extracting("fileName", "copies")
            .contains(
                tuple("1.pdf", 2),
                tuple("2.pdf", 1)
            );
    }

    @Test
    void should_set_all_fields_when_intialised_with_values() {
        List<Document> documents = List.of(
            new Document("mypdf.pdf", 2),
            new Document("1.pdf", 1)
        );

        PrintRequest printRequest = new PrintRequest(
            documents,
            "12345",
            "162MC066",
            "first-contact-pack"
        );

        assertThat(printRequest.documents)
            .as("documents list")
            .extracting("fileName", "copies")
            .contains(
                tuple("mypdf.pdf", 2),
                tuple("1.pdf", 1)
            );

        assertThat(printRequest.caseId)
            .isEqualTo("12345");

        assertThat(printRequest.caseRef)
            .isEqualTo("162MC066");

        assertThat(printRequest.letterType)
            .isEqualTo("first-contact-pack");
    }
}
