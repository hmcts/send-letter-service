package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class PendingLettersServiceTest {

    @Test
    void should_read_letters_in_proper_status_from_repo() {
        // given
        LetterRepository repo = mock(LetterRepository.class);
        PendingLettersService service = new PendingLettersService(repo);
        List<Letter> noLetters = Collections.emptyList();
        given(repo.findByStatus(any())).willReturn(noLetters);

        ArgumentCaptor<LetterStatus> statusArgumentCaptor = ArgumentCaptor.forClass(LetterStatus.class);

        // when
        List<Letter> letters = service.getPendingLetters();

        // then
        assertThat(letters).isEqualTo(noLetters);
        verify(repo).findByStatus(statusArgumentCaptor.capture());
        assertThat(statusArgumentCaptor.getValue()).isEqualTo(LetterStatus.Created);
    }
}
