package uk.gov.hmcts.reform.sendletter.model.out.v2;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.json.JsonTest;
import org.springframework.boot.test.json.JacksonTester;
import org.springframework.boot.test.json.JsonContent;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(SpringExtension.class)
@JsonTest
class LetterStatusTest {
    @Autowired
    private JacksonTester<LetterStatus> json;

    @Test
    void testAdditionalDataPresent() throws IOException {
        UUID uuid = UUID.randomUUID();
        Map<String, Object> additionalData = Map.of("reference", "ABD-123-WAZ", "count", 10, "additionInfo", "present");
        Map<String, Object> detailCopies = Map.of("Document_1", 1, "Document_2", 2);
        LetterStatus letterStatus = new LetterStatus(uuid, "TEST", "abc",
                ZonedDateTime.now(), ZonedDateTime.now().plusHours(1),
                ZonedDateTime.now().plusHours(2), additionalData, detailCopies);
        JsonContent<LetterStatus> jsonContent = this.json.write(letterStatus);

        assertThat(jsonContent).hasJsonPathStringValue("$.id")
                .hasJsonPathMapValue("$.additional_data")
                .extractingJsonPathMapValue("$.copies")
                .containsExactlyEntriesOf(detailCopies);
    }

    @Test
    void testWithEmptyAdditionalData() throws IOException {
        UUID uuid = UUID.randomUUID();
        Map<String, Object> detailCopies = Map.of("Document_1", 1);
        LetterStatus letterStatus = new LetterStatus(uuid, "TEST", "abc",
                ZonedDateTime.now(), ZonedDateTime.now().plusHours(1),
                ZonedDateTime.now().plusHours(2), Collections.emptyMap(), detailCopies);
        JsonContent<LetterStatus> jsonContent = this.json.write(letterStatus);
        assertThat(jsonContent).hasJsonPathStringValue("$.id")
                .hasJsonPath("$.additional_data")
                .extractingJsonPathMapValue("$.copies")
                .containsExactlyEntriesOf(detailCopies);
    }

    @Test
    void testWithNullAdditionalDataPresent() throws IOException {
        UUID uuid = UUID.randomUUID();

        LetterStatus letterStatus = new LetterStatus(uuid, "TEST", "abc",
                ZonedDateTime.now(), ZonedDateTime.now().plusHours(1),
                ZonedDateTime.now().plusHours(2), null,  null);
        JsonContent<LetterStatus> jsonContent = this.json.write(letterStatus);
        System.out.println(jsonContent);
        assertThat(jsonContent).hasJsonPathStringValue("$.id")
                .doesNotHaveJsonPath("$.additional_data")
                .doesNotHaveJsonPath("$.copies");
    }
}