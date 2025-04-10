package uk.gov.hmcts.reform.sendletter.controllers.sendlettercontroller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.controllers.ActionController;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotStaleException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToMarkLetterPostedLocallyException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToReprocessLetterException;
import uk.gov.hmcts.reform.sendletter.services.LetterActionService;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.time.LocalDate;
import java.time.LocalTime;
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

    @Autowired
    private MockMvc mockMvc;

    @MockitoBean
    private StaleLetterService staleLetterService;

    @MockitoBean
    private LetterActionService letterActionService;

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

    @Test
    void should_return_ok_when_letter_is_successfully_marked_as_created() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsCreated(letterId)).willReturn(1);

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isOk());

        verify(letterActionService).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_not_found_when_marking_letter_created_and_letter_not_found() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsCreated(letterId)).willThrow(new LetterNotFoundException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isNotFound());

        verify(letterActionService).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_bad_request_when_marking_letter_created_and_letter_not_stale() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsCreated(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isBadRequest());

        verify(letterActionService).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_bad_request_when_marking_letter_created_and_letter_status_not_supported() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsCreated(letterId))
            .willThrow(new UnableToReprocessLetterException(
                "Letter with ID '" + letterId + "', status 'Posted' can not be re-processed"));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                    .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
            .andExpect(status().isBadRequest())
            .andExpect(content().string(
                "Letter with ID '" + letterId + "', status 'Posted' can not be re-processed"));

        verify(letterActionService).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_bad_request_when_marking_letter_created_and_letter_id_is_corrupted() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsCreated(letterId)).willThrow(new LetterNotStaleException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isBadRequest());

        verify(letterActionService).markLetterAsCreated(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_created_and_header_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
            )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_created_and_header_is_invalid() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-created")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-report-api-key")
        )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_ok_when_letter_is_successfully_marked_as_aborted() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsAborted(letterId)).willReturn(1);

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-aborted")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isOk());

        verify(letterActionService).markLetterAsAborted(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_not_found_when_marking_letter_aborted_and_letter_not_found() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsAborted(letterId)).willThrow(new LetterNotFoundException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-aborted")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isNotFound());

        verify(letterActionService).markLetterAsAborted(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_aborted_and_header_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-aborted")
            )
            .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_aborted_and_header_is_invalid() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-aborted")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-report-api-key")
        )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_ok_when_letter_is_successfully_marked_as_posted_locally() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsPostedLocally(letterId)).willReturn(1);

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isOk());

        verify(letterActionService).markLetterAsPostedLocally(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_not_found_when_marking_letter_posted_locally_and_letter_not_found() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsPostedLocally(letterId))
            .willThrow(new LetterNotFoundException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
            )
                .andExpect(status().isNotFound());

        verify(letterActionService).markLetterAsPostedLocally(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_posted_locally_and_header_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
            )
            .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_posted_locally_and_header_is_invalid() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-report-api-key")
        )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_bad_request_when_marking_letter_posted_locally_and_letter_aborted() throws Exception {
        UUID letterId = UUID.randomUUID();

        String errorMessage = "Letter with ID '" + letterId + "', status '"
            + LetterStatus.Aborted + "' can not be marked as " + LetterStatus.PostedLocally;
        given(letterActionService.markLetterAsPostedLocally(letterId))
            .willThrow(new UnableToMarkLetterPostedLocallyException(errorMessage));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                    .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
        )
            .andExpect(status().isBadRequest())
            .andExpect(content().string(errorMessage));

        verify(letterActionService).markLetterAsPostedLocally(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_ok_when_letter_is_successfully_marked_as_posted() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsPostedLocally(letterId)).willReturn(1);

        LocalDate printedOn = LocalDate.parse("2022-08-12");
        LocalTime printedAt = LocalTime.parse("17:49");
        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
                    .queryParam("date", printedOn.toString())
                    .queryParam("time", printedAt.toString())
            )
                .andExpect(status().isOk());

        verify(letterActionService).markLetterAsPosted(letterId, printedOn, printedAt);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_not_found_when_marking_letter_posted_and_letter_not_found() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(letterActionService.markLetterAsPostedLocally(letterId))
            .willThrow(new LetterNotFoundException(letterId));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
                        .queryParam("date", LocalDate.parse("2022-08-10").toString())
                        .queryParam("time", LocalTime.parse("17:10").toString())
            )
                .andExpect(status().isNotFound());

        verify(letterActionService).markLetterAsPostedLocally(letterId);
        verifyNoMoreInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_posted_and_header_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .queryParam("date", LocalDate.parse("2022-08-10").toString())
                        .queryParam("time", LocalTime.parse("17:10").toString())
            )
            .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_unauthorized_when_marking_letter_posted_and_header_is_invalid() throws Exception {
        UUID letterId = UUID.randomUUID();

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer invalid-report-api-key")
                        .queryParam("date", LocalDate.parse("2022-08-10").toString())
                        .queryParam("time", LocalTime.parse("17:10").toString())
        )
                .andExpect(status().isUnauthorized());

        verifyNoInteractions(letterActionService);
    }

    @Test
    void should_return_bad_request_when_marking_letter_posted_and_letter_posted_locally() throws Exception {
        UUID letterId = UUID.randomUUID();

        String errorMessage = "Letter with ID '" + letterId + "', status '"
            + LetterStatus.PostedLocally + "' can not be marked as " + LetterStatus.PostedLocally;
        given(letterActionService.markLetterAsPostedLocally(letterId))
            .willThrow(new UnableToMarkLetterPostedLocallyException(errorMessage));

        mockMvc.perform(
                put("/letters/" + letterId + "/mark-posted-locally")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer valid-report-api-key")
                        .queryParam("date", LocalDate.parse("2022-08-10").toString())
                        .queryParam("time", LocalTime.parse("17:10").toString())
        )
            .andExpect(status().isBadRequest())
            .andExpect(content().string(errorMessage));

        verify(letterActionService).markLetterAsPostedLocally(letterId);
        verifyNoMoreInteractions(letterActionService);
    }
}
