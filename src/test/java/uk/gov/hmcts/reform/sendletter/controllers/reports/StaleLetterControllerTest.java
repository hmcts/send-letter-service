package uk.gov.hmcts.reform.sendletter.controllers.reports;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;
import uk.gov.hmcts.reform.sendletter.util.CsvWriter;

import java.io.File;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.mockito.BDDMockito.given;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.http.MediaType.APPLICATION_OCTET_STREAM_VALUE;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadJson;

@WebMvcTest(StaleLetterController.class)
class StaleLetterControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockitoBean
    private StaleLetterService staleLetterService;

    @Test
    void should_return_letters_from_stale_letter_service() throws Exception {
        List<BasicLetterInfo> letters = Arrays.asList(
            letter(
                UUID.fromString("767cf17e-0ec0-452b-a457-bc173d51ff40"),
                "service1",
                LetterStatus.Uploaded,
                LocalDateTime.parse("2019-05-03T12:34:56.123"),
                LocalDateTime.parse("2019-05-03T13:00:00.000")
            ),
            letter(
                UUID.fromString("462a58fe-e9b7-494f-a719-5083f31c69cf"),
                "service2",
                LetterStatus.Created,
                LocalDateTime.parse("2019-05-02T10:11:22.321"),
                null
            )
        );

        given(staleLetterService.getStaleLetters()).willReturn(letters);

        mockMvc
            .perform(get("/stale-letters").accept(APPLICATION_JSON_VALUE))
            .andExpect(status().isOk())
            .andExpect(content().contentType(APPLICATION_JSON_VALUE))
            .andExpect(
                content().json(loadJson(
                    "controller/stale-letters/stale-letters-response.json"
                    ),
                    false
                )
            );
    }

    @Test
    void should_return_letters_from_stale_as_csv_file() throws Exception {
        String expected = String.join("\r\n", "Id,Status,Service,CreatedAt,SentToPrintAt",
                "767cf17e-0ec0-452b-a457-bc173d51ff40,Uploaded,service1,2019-05-03T12:34:56.123,2019-05-03T13:00",
                "462a58fe-e9b7-494f-a719-5083f31c69cf,Created,service2,2019-05-02T10:11:22.321,","");

        List<BasicLetterInfo> letters = Arrays.asList(
                letter(
                        UUID.fromString("767cf17e-0ec0-452b-a457-bc173d51ff40"),
                        "service1",
                        LetterStatus.Uploaded,
                        LocalDateTime.parse("2019-05-03T12:34:56.123"),
                        LocalDateTime.parse("2019-05-03T13:00:00.000")
                ),
                letter(
                        UUID.fromString("462a58fe-e9b7-494f-a719-5083f31c69cf"),
                        "service2",
                        LetterStatus.Created,
                        LocalDateTime.parse("2019-05-02T10:11:22.321"),
                        null
                )
        );
        File file = CsvWriter.writeStaleLettersToCsv(letters);
        given(staleLetterService.getDownloadFile()).willReturn(file);

        mockMvc
                .perform(get("/stale-letters/download"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(APPLICATION_OCTET_STREAM_VALUE))
                .andExpect(header().string("Content-Disposition","attachment; filename=stale-letters.csv"))
                .andExpect(content().string(expected));
    }


    @Test
    void should_return_server_error_when_stale_letter_service_throws_exception() throws Exception {
        given(staleLetterService.getStaleLetters()).willThrow(RuntimeException.class);

        mockMvc
            .perform(
                get("/stale-letters")
                    .accept(APPLICATION_JSON_VALUE)
            )
            .andExpect(status().is5xxServerError());
    }

    private BasicLetterInfo letter(
        UUID id,
        String service,
        LetterStatus status,
        LocalDateTime createdAt,
        LocalDateTime sentToPrintAt
    ) {
        return new BasicLetterInfo(id, "checksum", service, status, "type", null, createdAt, sentToPrintAt, null);
    }
}
