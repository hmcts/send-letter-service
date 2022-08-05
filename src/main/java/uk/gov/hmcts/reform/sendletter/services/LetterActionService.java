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
import uk.gov.hmcts.reform.sendletter.exception.UnableToAbortLetterException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToReprocessLetterException;

import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_ABORTED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_CREATED;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.FailedToUpload;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@Service
public class LetterActionService {
    private static final Logger log = LoggerFactory.getLogger(StaleLetterService.class);

    private final LetterRepository letterRepository;
    private final LetterEventRepository letterEventRepository;
    private final StaleLetterService staleLetterService;

    public LetterActionService(LetterRepository letterRepository,
                               LetterEventRepository letterEventRepository,
                               StaleLetterService staleLetterService) {
        this.letterRepository = letterRepository;
        this.letterEventRepository = letterEventRepository;
        this.staleLetterService = staleLetterService;
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
            throw new UnableToAbortLetterException(
                "Letter with ID '" + letter.getId() + "', status '" + Posted + "' can not be aborted");
        }

        createLetterEvent(
            letterOpt.get(),
            MANUALLY_MARKED_AS_ABORTED,
            "Letter marked manually as Aborted to stop processing");

        return letterRepository.markLetterAsAborted(id);
    }

    @Transactional
    public int markLetterAsCreated(UUID id) {
        log.info("Marking the letter id {} as Created to re-upload to SFTP server", id);

        Optional<Letter> letterOpt = letterRepository.findById(id);

        if (letterOpt.isEmpty()) {
            throw new LetterNotFoundException(id);
        }

        Letter letter = letterOpt.get();
        checkLetterStatus(letter);

        if (letter.getStatus() == FailedToUpload) {
            createLetterEvent(letter, MANUALLY_MARKED_AS_CREATED, "Letter marked manually as Created to re-process");
            return letterRepository.markLetterAsCreated(letter.getId());
        } else {
            return staleLetterService.markStaleLetterAsCreated(id);
        }
    }

    private void checkLetterStatus(Letter letter) {
        if (!List.of(FailedToUpload, Uploaded).contains(letter.getStatus())) {
            throw new UnableToReprocessLetterException(
                "Letter with ID '" + letter.getId() + "', status '"
                    + letter.getStatus() + "' can not be re-processed");
        }

    }

    private void createLetterEvent(Letter letter, EventType type, String notes) {
        log.info("Creating letter event {} for letter {}, notes {}", type, letter.getId(), notes);

        LetterEvent letterEvent = new LetterEvent(letter, type, notes, Instant.now());

        letterEventRepository.save(letterEvent);
    }
}
