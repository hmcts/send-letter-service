package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class LettersPendingUploadService {

    private final LetterRepository repo;

    public LettersPendingUploadService(LetterRepository repo) {
        this.repo = repo;
    }

    public List<Letter> getLettersPendingUpload() {
        return repo.findByStatus(LetterStatus.Created).collect(toList());
    }
}
