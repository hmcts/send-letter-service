package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterEvent;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;

import java.time.LocalDateTime;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.FAILED_IN_UPLOAD;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Failed;

@ExtendWith(MockitoExtension.class)
class LetterEventServiceTest {

    private LetterEventService letterEventService;

    @Mock
    private LetterRepository letterRepository;

    @Mock
    private LetterEventRepository letterEventRepository;

    @BeforeEach
    void setUp() {
        letterEventService = new LetterEventService(letterRepository, letterEventRepository);
    }

    @Test
    void failLetterUpload_should_save_letter_and_event() {
        // given
        UUID letterId = UUID.randomUUID();
        Letter letter = new Letter(
                letterId,
                letterId.toString(),
                "cmc",
                null,
                "type",
                null,
                false,
                null,
                LocalDateTime.now(),
                null
        );
        letter.setStatus(Created);
        Exception ex = new NullPointerException("msg");

        // when
        letterEventService.failLetterUpload(letter, ex);

        // then
        ArgumentCaptor<Letter> letterArgumentCaptor = ArgumentCaptor.forClass(Letter.class);
        ArgumentCaptor<LetterEvent> letterEventArgumentCaptor = ArgumentCaptor.forClass(LetterEvent.class);
        verify(letterRepository).save(letterArgumentCaptor.capture());
        verify(letterEventRepository).save(letterEventArgumentCaptor.capture());
        assertThat(letterArgumentCaptor.getValue().getStatus()).isEqualTo(Failed);
        assertThat(letterEventArgumentCaptor.getValue().getLetter()).isEqualTo(letterArgumentCaptor.getValue());
        assertThat(letterEventArgumentCaptor.getValue().getType()).isEqualTo(FAILED_IN_UPLOAD);
        assertThat(letterEventArgumentCaptor.getValue().getNotes()).isEqualTo("msg");
    }
}
