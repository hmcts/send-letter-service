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

import java.time.Instant;

import static uk.gov.hmcts.reform.sendletter.entity.EventType.FAILED_TO_UPLOAD;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.FailedToUpload;

/**
 * Service to handle letter events.
 */
@Service
public class LetterEventService {
    private static final Logger log = LoggerFactory.getLogger(LetterEventService.class);

    private final LetterRepository letterRepository;
    private final LetterEventRepository letterEventRepository;

    /**
     * Constructor for the LetterEventService.
     * @param letterRepository The repository for letter
     * @param letterEventRepository The repository for letter event
     */
    public LetterEventService(
            LetterRepository letterRepository,
            LetterEventRepository letterEventRepository
    ) {
        this.letterRepository = letterRepository;
        this.letterEventRepository = letterEventRepository;
    }

    /**
     * Create failed letter event.
     * @param letter letter
     */
    @Transactional
    public void failLetterUpload(Letter letter, Exception ex) {
        log.error("Error uploading letter {}", letter.getId(), ex);

        letter.setStatus(FailedToUpload);
        letterRepository.save(letter);

        createLetterEvent(
                letter,
                FAILED_TO_UPLOAD,
                ex.getMessage()
        );
    }

    /**
     * Create letter event.
     * @param letter letter
     * @param type event type
     * @param notes notes
     */
    private void createLetterEvent(Letter letter, EventType type, String notes) {
        log.info("Creating letter event {} for letter {}, notes {}", type, letter.getId(), notes);

        LetterEvent letterEvent = new LetterEvent(letter, type, notes, Instant.now());

        letterEventRepository.save(letterEvent);
    }
}
