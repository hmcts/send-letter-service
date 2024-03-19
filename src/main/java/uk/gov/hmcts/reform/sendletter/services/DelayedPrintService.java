package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.util.CsvWriter;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.Period;
import java.util.List;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

/**
 * Service for delayed print data.
 */
@Service
public class DelayedPrintService {
    private final LetterRepository letterRepository;

    /**
     * Constructor for the DelayedPrintService.
     * @param letterRepository The repository for letter
     */
    @Autowired
    public DelayedPrintService(LetterRepository letterRepository) {
        this.letterRepository = letterRepository;
    }

    /**
     * Get the delayed print letters.
     * @param fromCreatedDate The from created date
     * @param toCreatedDate The to created date
     * @param minProcessingDays The minimum processing days
     * @return The delayed print letters
     * @throws IOException If an I/O error occurs
     */
    @Transactional
    public File getDelayLettersAttachment(LocalDateTime fromCreatedDate,
                                          LocalDateTime toCreatedDate,
                                          int minProcessingDays) throws IOException {
        try (Stream<BasicLetterInfo> delayedPostedLetter = letterRepository
                .findByStatusAndCreatedAtBetweenOrderByCreatedAtAsc(LetterStatus.Posted,
                    fromCreatedDate, toCreatedDate)) {
            List<BasicLetterInfo> filteredLetters = delayedPostedLetter.filter(letter ->
                Period.between(letter.getSentToPrintAt().toLocalDate(),
                    letter.getPrintedAt().toLocalDate()).getDays() > minProcessingDays)
                .collect(toList());
            return CsvWriter.writeDelayedPostedLettersToCsv(filteredLetters.stream());
        }
    }
}
