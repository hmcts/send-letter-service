package uk.gov.hmcts.reform.sendletter.model.out;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.google.common.io.Resources;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.UUID;

import static com.google.common.base.Charsets.UTF_8;
import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;

class PrintResponseTest {

    @Test
    void should_serialize_when_request_parsed() throws IOException {
        String json = Resources.toString(getResource("print_job_response.json"), UTF_8);

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        PrintResponse printRequest = objectMapper.readValue(json, PrintResponse.class);
        PrintJob printJob = printRequest.printJob;

        assertThat(printJob.id).isEqualTo(UUID.fromString("33dffc2f-94e0-4584-a973-cc56849ecc0b"));
    }
}
