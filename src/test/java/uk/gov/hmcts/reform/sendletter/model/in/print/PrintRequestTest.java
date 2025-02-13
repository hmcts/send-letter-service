package uk.gov.hmcts.reform.sendletter.model.in.print;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.model.Document;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadJson;


class PrintRequestTest {

    @Test
    void should_serialize_when_request_parsed() throws Exception {
        String json = loadJson("print_job.json");

        ObjectMapper objectMapper = new ObjectMapper();
        PrintRequest printRequest = objectMapper.readValue(json, PrintRequest.class);

        assertThat(printRequest.type)
            .isEqualTo("SSC001");

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
            new Document(
                "mypdf.pdf",
                "33dffc2f-94e0-4584-a973-cc56849ecc0b-sscs-SSC001-mypdf.pdf",
                2
            ),
            new Document(
                "1.pdf",
                "33dffc2f-94e0-4584-a973-cc56849ecc0b-sscs-SSC001-2.pdf",
                1
            )
        );

        PrintRequest printRequest = new PrintRequest(
            "SSC001",
            documents,
            "12345",
            "162MC066",
            "first-contact-pack"
        );

        assertThat(printRequest.type)
            .isEqualTo("SSC001");

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

    @Test
    void should_throw_exception_when_documents_size_exceeds_limit() {
        List<Document> documents = new ArrayList<>();
        for (int i = 0; i < 102; i++) {
            documents.add(new Document("doc" + i + ".pdf", "content-" + i, 1));
        }

        PrintRequest printRequest = new PrintRequest(
            "SSC001",
            documents,
            "12345",
            "162MC066",
            "first-contact-pack"
        );

        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();
        var violations = validator.validate(printRequest);

        assertThat(violations)
            .isNotEmpty()
            .anyMatch(violation -> violation.getMessage().contains("size must be between 1 and 100"));
    }
}
