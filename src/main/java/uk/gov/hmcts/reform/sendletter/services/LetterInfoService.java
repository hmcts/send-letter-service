package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class LetterInfoService {

    private final LetterRepository repository;

    public LetterInfoService(LetterRepository repository) {
        this.repository = repository;
    }

    public List<LetterStatus> findAll(LocalDate createdAt) {
        return repository
            .findCreatedAt(LocalDate.now())
                .stream()
                .map(l -> new LetterStatus(
                    l.getId(),
                    l.getStatus(),
                    l.getChecksum(),
                    toUtc(l.getCreatedAt()),
                    toUtc(l.getSentToPrintAt()),
                    toUtc(l.getPrintedAt()),
                    false
                ))
                .collect(toList());
    }

    private ZonedDateTime toUtc(LocalDateTime dateTime) {
        if (null == dateTime) {
            return null;
        } else {
            return dateTime.atZone(ZoneId.of("UTC"));
        }
    }
}
