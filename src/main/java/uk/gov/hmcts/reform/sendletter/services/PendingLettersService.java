package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Stream;

import static uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask.SMOKE_TEST_LETTER_TYPE;

/**
 * Service to handle pending letters.
 */
@Service
public class PendingLettersService {

    private final LetterRepository repo;

    /**
     * Constructor for the PendingLettersService.
     *
     * @param repo The repository for letter
     */
    public PendingLettersService(LetterRepository repo) {
        this.repo = repo;
    }

    /**
     * Get pending letters.
     *
     * @return the pending letters
     */
    public List<BasicLetterInfo> getPendingLetters() {
        return repo.findPendingLetters();
    }

    /**
     * Get pending letters created before a certain time.
     *
     * @param before the time in minutes
     * @return the pending letters
     */
    public Stream<BasicLetterInfo> getPendingLettersCreatedBeforeTime(int before) {
        LocalDateTime localDateTime = LocalDateTime.now().minusMinutes(before);
        return repo.findByCreatedAtBeforeAndStatusAndTypeNot(localDateTime,
                LetterStatus.Created, SMOKE_TEST_LETTER_TYPE);
    }
}
