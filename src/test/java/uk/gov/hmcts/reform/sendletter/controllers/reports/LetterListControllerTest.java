package uk.gov.hmcts.reform.sendletter.controllers.reports;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

import static java.time.Month.MAY;
import static java.util.Arrays.asList;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(LetterListController.class)
class LetterListControllerTest {

    @Autowired MockMvc mockMvc;

    @MockBean LetterRepository repository;

    @Test
    void should_return_letters_from_given_day() throws Exception {

        BasicLetterInfo letter1 = letter(
            UUID.fromString("9b9c019d-78e7-479d-afc6-a7b5b585c47e"),
            "Posted",
            "service_A",
            LocalDateTime.of(2020, MAY, 17, 7, 0, 0),
            LocalDateTime.of(2020, MAY, 17, 8, 0, 0),
            LocalDateTime.of(2020, MAY, 17, 9, 29, 30)
        );

        BasicLetterInfo letter2 = letter(
            UUID.fromString("db4c7ec5-c2ae-4bd7-b2b6-f743234f55bf"),
            "Uploaded",
            "service_B",
            LocalDateTime.of(2020, MAY, 17, 10, 0, 0),
            LocalDateTime.of(2020, MAY, 17, 11, 0, 0),
            null
        );

        LocalDate date = LocalDate.of(2020, 4, 8);

        given(repository.findCreatedAt(date)).willReturn(asList(letter1, letter2));

        mockMvc
            .perform(get("/letters").queryParam("date", "2020-04-08"))
            .andExpect(status().isOk())
            .andExpect(content().json(
                "{"
                    + "'count': 2,"
                    + "'letters': ["
                    + "  {"
                    + "    'id': '9b9c019d-78e7-479d-afc6-a7b5b585c47e',"
                    + "    'service': 'service_A',"
                    + "    'status': 'Posted',"
                    + "    'created_at': '2020-05-17T07:00:00',"
                    + "    'sent_to_print_at': '2020-05-17T08:00:00',"
                    + "    'printed_at': '2020-05-17T09:29:30'"
                    + "  },"
                    + "  {"
                    + "    'id': 'db4c7ec5-c2ae-4bd7-b2b6-f743234f55bf',"
                    + "    'service': 'service_B',"
                    + "    'status': 'Uploaded',"
                    + "    'created_at': '2020-05-17T10:00:00',"
                    + "    'sent_to_print_at': '2020-05-17T11:00:00',"
                    + "    'printed_at': null"
                    + "  }"
                    + "]"
                    + "}",
                true
                )
            );
    }

    private BasicLetterInfo letter(
        UUID id,
        String status,
        String service,
        LocalDateTime createdAt,
        LocalDateTime sentToPrintAt,
        LocalDateTime printedAt
    ) {
        BasicLetterInfo letter = mock(BasicLetterInfo.class);
        when(letter.getId()).thenReturn(id);
        when(letter.getStatus()).thenReturn(status);
        when(letter.getService()).thenReturn(service);
        when(letter.getCreatedAt()).thenReturn(createdAt);
        when(letter.getSentToPrintAt()).thenReturn(sentToPrintAt);
        when(letter.getPrintedAt()).thenReturn(printedAt);
        return letter;
    }
}
