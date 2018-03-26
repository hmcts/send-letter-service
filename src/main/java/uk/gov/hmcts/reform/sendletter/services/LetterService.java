package uk.gov.hmcts.reform.sendletter.services;

import org.apache.http.util.Asserts;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.PdfCreator;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.duplex.DuplexPreparator;

import java.sql.Timestamp;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Optional;
import java.util.UUID;

import static uk.gov.hmcts.reform.sendletter.services.LetterChecksumGenerator.generateChecksum;

@Service
public class LetterService {

    private static final Logger log = LoggerFactory.getLogger(LetterService.class);

    private final PdfCreator pdfCreator = new PdfCreator(new DuplexPreparator());
    private final LetterRepository letterRepository;

    public LetterService(LetterRepository letterRepository) {
        this.letterRepository = letterRepository;
    }

    public UUID send(LetterRequest letter, String serviceName) {
        Asserts.notEmpty(serviceName, "serviceName");

        final String messageId = generateChecksum(letter);

        log.info("Generated message: id = {}", messageId);

        Optional<Letter> result = letterRepository.findOptionalByMessageIdOrderByCreatedAtDesc(messageId);

        return result.filter(l -> l.getState().equals(LetterState.Created))
            .map(l -> {
                UUID id = l.getId();
                log.info("Same message found already created. Returning {} letter", id);
                return id;
            })
            .orElse(getNewLetterId(letter, messageId, serviceName));
    }

    private UUID getNewLetterId(LetterRequest letterRequest, String messageId, String serviceName) {
        byte[] pdf = pdfCreator.create(letterRequest);

        Letter letter = new Letter(messageId, serviceName, null, letterRequest.type, pdf);

        UUID letterId = letterRepository.save(letter).getId();

        log.info("Created new letter {}", letterId);

        return letterId;
    }

    public LetterStatus getStatus(UUID id, String serviceName) {
        Letter letter = letterRepository
            .findOptionalByIdAndService(id, serviceName)
            .orElseThrow(() -> new LetterNotFoundException(id));

        return new LetterStatus(
            id,
            letter.getMessageId(),
            toDateTime(letter.getCreatedAt()),
            toDateTime(letter.getSentToPrintAt()),
            toDateTime(letter.getPrintedAt()),
            letter.isFailed()
        );
    }

    public static ZonedDateTime toDateTime(Timestamp stamp) {
        if (null == stamp) {
            return null;
        }
        return stamp.toInstant().atZone(ZoneId.of("UTC"));
    }
}
