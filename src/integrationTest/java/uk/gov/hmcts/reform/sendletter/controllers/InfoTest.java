package uk.gov.hmcts.reform.sendletter.controllers;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.WebApplicationContext;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@AutoConfigureMockMvc
@ComponentScan(basePackages = "...", lazyInit = true)
@ContextConfiguration
@SpringBootTest
@Transactional
class InfoTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private LetterRepository letterRepository;

    @Autowired
    private WebApplicationContext wac;

    @AfterEach
    void tearDown() {
        letterRepository.deleteAll();
    }

    @Test
    void should_return_status_breakdown() throws Exception {

        insert(LetterStatus.Created);
        insert(LetterStatus.Posted);
        insert(LetterStatus.Posted);

        mvc.perform(get("/info"))
            .andExpect(jsonPath("$.letters_by_status.created", is(1)))
            .andExpect(jsonPath("$.letters_by_status.posted", is(2)))
            .andExpect(jsonPath("$.letters_by_status.uploaded", is(0)));
    }

    private void insert(LetterStatus status) {
        Letter letter = SampleData.letterEntity("some-service");
        letter.setStatus(status);
        letterRepository.saveAndFlush(letter);
    }
}
