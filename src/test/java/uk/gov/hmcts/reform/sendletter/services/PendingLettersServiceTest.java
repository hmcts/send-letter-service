package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class PendingLettersServiceTest {

    @Test
    void should_read_letters_in_proper_status_from_repo() {
        // given
        LetterRepository repo = mock(LetterRepository.class);
        PendingLettersService service = new PendingLettersService(repo);

        ArgumentCaptor<LetterStatus> statusArgumentCaptor = ArgumentCaptor.forClass(LetterStatus.class);

        // when
        service.getPendingLetters();

        // then
        verify(repo).findByStatus(statusArgumentCaptor.capture());
        assertThat(statusArgumentCaptor.getValue()).isEqualTo(LetterStatus.Created);
    }
}
