package uk.gov.hmcts.reform.sendletter.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.PrintService;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadJson;

@WebMvcTest(PrintLetterController.class)
class PrintLetterControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private PrintService printService;

    @MockBean
    private AuthService authService;

    @Test
    void should_responsed_when_request_is_valid() throws Exception {
        String serviceName = "test";
        UUID uuid = UUID.randomUUID();

        String responseJson = loadJson("print_job_response.json");

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        PrintResponse printResponse = objectMapper.readValue(responseJson, PrintResponse.class);

        String serviceAuthorization = "ServiceAuthorization";
        given(authService.authenticate(serviceAuthorization))
            .willReturn(serviceName);

        given(printService.save(eq(uuid.toString()), eq(serviceName), isA(PrintRequest.class)))
            .willReturn(printResponse);

        String requestJson = loadJson("print_job.json");
        mockMvc.perform(put("/print-jobs/{id}", uuid)
            .header("ServiceAuthorization", serviceAuthorization)
            .contentType(MediaTypes.PRINT_V1)
            .content(requestJson)
        ).andDo(print())
            .andExpect(status()
                .isOk())
            .andExpect(content()
                .json(responseJson));
    }
}
