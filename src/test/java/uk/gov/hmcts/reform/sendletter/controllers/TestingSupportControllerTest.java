package uk.gov.hmcts.reform.sendletter.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import uk.gov.hmcts.reform.sendletter.exception.LetterFileNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.TestingSupportLetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.DownloadedLetterFile;
import uk.gov.hmcts.reform.sendletter.services.TestingSupportService;

import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(TestingSupportController.class)
@TestPropertySource(properties = "testing-support.enabled=true")
class TestingSupportControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockitoBean
    private TestingSupportService testingSupportService;

    @Test
    void should_download_letter_file() throws Exception {
        UUID letterId = UUID.randomUUID();
        byte[] content = "zip-content".getBytes();

        given(testingSupportService.downloadLetterFile(letterId))
            .willReturn(new DownloadedLetterFile("letter.zip", content));

        mockMvc
            .perform(get("/testing-support/download/" + letterId))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_OCTET_STREAM))
            .andExpect(header().string(HttpHeaders.CONTENT_DISPOSITION, containsString("attachment")))
            .andExpect(header().string(HttpHeaders.CONTENT_DISPOSITION, containsString("filename=\"letter.zip\"")))
            .andExpect(content().bytes(content));
    }

    @Test
    void should_return_not_found_when_letter_id_is_invalid() throws Exception {
        mockMvc
            .perform(get("/testing-support/download/not-a-uuid"))
            .andExpect(status().isNotFound())
            .andExpect(content().string("Letter with ID 'not-a-uuid' not found"));

        verifyNoInteractions(testingSupportService);
    }

    @Test
    void should_return_not_found_message_when_letter_does_not_exist() throws Exception {
        UUID letterId = UUID.randomUUID();

        given(testingSupportService.downloadLetterFile(letterId))
            .willThrow(new TestingSupportLetterNotFoundException(letterId));

        mockMvc
            .perform(get("/testing-support/download/" + letterId))
            .andExpect(status().isNotFound())
            .andExpect(content().string("Letter with ID '" + letterId + "' not found"));
    }

    @Test
    void should_return_not_found_message_when_uploaded_letter_file_is_missing() throws Exception {
        UUID letterId = UUID.randomUUID();
        String filename = "letter.zip";

        given(testingSupportService.downloadLetterFile(letterId))
            .willThrow(new LetterFileNotFoundException(letterId, filename));

        mockMvc
            .perform(get("/testing-support/download/" + letterId))
            .andExpect(status().isNotFound())
            .andExpect(content().string(
                "Letter with ID '" + letterId + "' exists but file '" + filename + "' was not found on SFTP"
            ));
    }
}
