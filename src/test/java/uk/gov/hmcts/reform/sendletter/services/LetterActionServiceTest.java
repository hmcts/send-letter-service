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
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToAbortLetterException;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_ABORTED;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@ExtendWith(MockitoExtension.class)
class LetterActionServiceTest {

    @Mock
    private LetterRepository letterRepository;

    @Mock
    private LetterEventRepository letterEventRepository;

    private LetterActionService letterActionService;

    @BeforeEach
    void setUp() {
        letterActionService = new LetterActionService(letterRepository, letterEventRepository);
    }

    @Test
    void should_abort_letter_when_record_present() {
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
        letter.setStatus(Uploaded);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));
        given(letterRepository.markLetterAsAborted(letterId)).willReturn(1);

        // when
        int result = letterActionService.markLetterAsAborted(letterId);

        // then
        assertThat(result).isEqualTo(1);

        ArgumentCaptor<LetterEvent> letterEventArgumentCaptor = ArgumentCaptor.forClass(LetterEvent.class);
        verify(letterEventRepository).save(letterEventArgumentCaptor.capture());
        assertThat(letterEventArgumentCaptor.getValue().getLetter()).isEqualTo(letter);
        assertThat(letterEventArgumentCaptor.getValue().getType()).isEqualTo(MANUALLY_MARKED_AS_ABORTED);
        assertThat(letterEventArgumentCaptor.getValue().getNotes())
            .isEqualTo("Letter marked manually as Aborted to stop processing");

        verify(letterRepository).markLetterAsAborted(letterId);
        verifyNoMoreInteractions(letterRepository, letterEventRepository);
    }

    @Test
    void should_throw_exception_when_letter_not_present() {
        // given
        UUID letterId = UUID.randomUUID();
        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.empty());

        // when
        // then
        assertThatThrownBy(() -> letterActionService.markLetterAsAborted(letterId))
            .isInstanceOf(LetterNotFoundException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

    @Test
    void should_throw_exception_when_letter_status_is_printed() {
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
        letter.setStatus(Posted);

        reset(letterRepository);
        given(letterRepository.findById(letterId)).willReturn(Optional.of(letter));

        // when
        // then
        assertThatThrownBy(() -> letterActionService.markLetterAsAborted(letterId))
            .isInstanceOf(UnableToAbortLetterException.class);

        verifyNoMoreInteractions(letterRepository);
        verifyNoInteractions(letterEventRepository);
    }

}
