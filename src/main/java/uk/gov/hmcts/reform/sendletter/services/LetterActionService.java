package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.EventType;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterEvent;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.UnsupportedLetterStatusException;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_ABORTED;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;

@Service
public class LetterActionService {
    private static final Logger log = LoggerFactory.getLogger(StaleLetterService.class);

    private final LetterRepository letterRepository;
    private final LetterEventRepository letterEventRepository;

    public LetterActionService(LetterRepository letterRepository, LetterEventRepository letterEventRepository) {
        this.letterRepository = letterRepository;
        this.letterEventRepository = letterEventRepository;
    }

    @Transactional
    public int markLetterAsAborted(UUID id) {
        log.info("Marking the letter id {} as Aborted", id);

        Optional<Letter> letterOpt = letterRepository.findById(id);

        if (letterOpt.isEmpty()) {
            throw new LetterNotFoundException(id);
        }

        Letter letter = letterOpt.get();
        if (letter.getStatus() == Posted) {
            throw new UnsupportedLetterStatusException(letter.getId(), letter.getStatus());
        }

        createLetterEvent(
            letterOpt.get(),
            MANUALLY_MARKED_AS_ABORTED,
            "Letter marked manually as Aborted to stop processing");

        return letterRepository.markLetterAsAborted(id);
    }

    private void createLetterEvent(Letter letter, EventType type, String notes) {
        log.info("Creating letter event {} for letter {}, notes {}", type, letter.getId(), notes);

        LetterEvent letterEvent = new LetterEvent(letter, type, notes, Instant.now());

        letterEventRepository.save(letterEvent);
    }
}
