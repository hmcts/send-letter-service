package uk.gov.hmcts.reform.sendletter.controllers.sendlettercontroller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.controllers.ActionController;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotStaleException;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.util.UUID;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(ActionController.class)
class ActionControllerTest {

    @Autowired private MockMvc mockMvc;

    @MockBean private StaleLetterService staleLetterService;

    @Test
    void should_return_ok_when_letter_is_successfully_marked() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willReturn(1);

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isOk());

        verify(staleLetterService).markStaleLetterAsNotSent(letterId);
        verifyNoMoreInteractions(staleLetterService);
    }

    @Test
    void should_return_not_found_when_letter_is_not_found() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willThrow(new LetterNotFoundException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isNotFound());

        verify(staleLetterService).markStaleLetterAsNotSent(letterId);
        verifyNoMoreInteractions(staleLetterService);
    }

    @Test
    void should_return_bad_request_when_letter_is_not_stale() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isBadRequest());

        verify(staleLetterService).markStaleLetterAsNotSent(letterId);
        verifyNoMoreInteractions(staleLetterService);
    }

    @Test
    void should_return_bad_request_when_letter_id_is_corrupted() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isBadRequest());

        verify(staleLetterService).markStaleLetterAsNotSent(letterId);
        verifyNoMoreInteractions(staleLetterService);
    }

    @Test
    void should_return_unauthorized_when_header_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
            )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(staleLetterService);
    }

    @Test
    void should_return_unauthorized_when_when_header_is_invalid() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(staleLetterService.markStaleLetterAsNotSent(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-not-sent")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-report-api-key")
        )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(staleLetterService);
    }
}
