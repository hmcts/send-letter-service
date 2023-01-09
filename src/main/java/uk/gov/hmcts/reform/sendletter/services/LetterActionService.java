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
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToAbortLetterException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToMarkLetterPostedException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToMarkLetterPostedLocallyException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToReprocessLetterException;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_ABORTED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_CREATED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_POSTED;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_POSTED_LOCALLY;
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
        checkLetterStatusForLetterReUpload(letter);

        if (letter.getStatus() == FailedToUpload) {
            createLetterEvent(letter, MANUALLY_MARKED_AS_CREATED, "Letter marked manually as Created to re-process");
            return letterRepository.markLetterAsCreated(letter.getId());
        } else {
            return staleLetterService.markStaleLetterAsCreated(id);
        }
    }

    @Transactional
    public int markLetterAsPostedLocally(UUID id) {
        log.info("Marking the letter id {} as PostedLocally", id);

        Optional<Letter> letterOpt = letterRepository.findById(id);

        if (letterOpt.isEmpty()) {
            throw new LetterNotFoundException(id);
        }

        Letter letter = letterOpt.get();
        checkLetterStatusForMarkPostedLocally(letter);

        createLetterEvent(
            letterOpt.get(),
            MANUALLY_MARKED_AS_POSTED_LOCALLY,
            "Letter marked manually as PostedLocally as the letter was printed and posted by CTSC");

        return letterRepository.markLetterAsPostedLocally(id);
    }

    @Transactional
    public int markLetterAsPosted(UUID id,
                                  LocalDate printedOn,
                                  LocalTime printedAt) {
        log.info("Marking the letter id {} as Posted", id);

        Optional<Letter> letterOpt = letterRepository.findById(id);
        if (letterOpt.isEmpty()) {
            throw new LetterNotFoundException(id);
        }

        Letter letter = letterOpt.get();
        checkLetterStatusForMarkPosted(letter);

        final ZonedDateTime printedDateTime = ZonedDateTime.of(printedOn, printedAt, ZoneOffset.UTC);

        createLetterEvent(
            letterOpt.get(),
            MANUALLY_MARKED_AS_POSTED,
            "Letter marked manually as Posted");

        return letterRepository.markLetterAsPosted(id, printedDateTime.toLocalDateTime());
    }

    private void checkLetterStatusForLetterReUpload(Letter letter) {
        if (!List.of(FailedToUpload, Uploaded).contains(letter.getStatus())) {
            throw new UnableToReprocessLetterException(
                "Letter with ID '" + letter.getId() + "', status '"
                    + letter.getStatus() + "' can not be re-processed");
        }

    }

    private void checkLetterStatusForMarkPostedLocally(Letter letter) {
        if (!List.of(Uploaded, Posted).contains(letter.getStatus())) {
            throw new UnableToMarkLetterPostedLocallyException(
                "Letter with ID '" + letter.getId() + "', status '"
                    + letter.getStatus() + "' can not be marked as " + LetterStatus.PostedLocally);
        }

    }

    private void checkLetterStatusForMarkPosted(Letter letter) {
        if (letter.getStatus() == Posted) {
            throw new UnableToMarkLetterPostedException(
                "Letter with ID '" + letter.getId() + "', status '"
                    + letter.getStatus() + "' can not be marked as " + LetterStatus.Posted);
        }

    }

    private void createLetterEvent(Letter letter, EventType type, String notes) {
        log.info("Creating letter event {} for letter {}, notes {}", type, letter.getId(), notes);

        LetterEvent letterEvent = new LetterEvent(letter, type, notes, Instant.now());

        letterEventRepository.save(letterEvent);
    }
}
