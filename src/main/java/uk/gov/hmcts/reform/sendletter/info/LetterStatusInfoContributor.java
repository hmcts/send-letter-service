package uk.gov.hmcts.reform.sendletter.info;

import org.springframework.boot.actuate.info.Info;
import org.springframework.boot.actuate.info.InfoContributor;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.entity.StatusCount;

import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

@Component
public class LetterStatusInfoContributor implements InfoContributor {

    private final LetterRepository repo;

    public LetterStatusInfoContributor(LetterRepository repo) {
        this.repo = repo;
    }

    @Override
    public void contribute(Info.Builder builder) {
        builder.withDetail(
            "letters_by_status",
            Stream.of(LetterStatus.values())
                .collect(
                    toMap(
                        s -> s,
                        s -> repo.countByStatus(s)
                    )
                )
        );
    }
}
