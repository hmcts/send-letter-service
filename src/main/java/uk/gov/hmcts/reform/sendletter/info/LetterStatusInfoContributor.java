package uk.gov.hmcts.reform.sendletter.info;

import org.springframework.boot.actuate.info.Info;
import org.springframework.boot.actuate.info.InfoContributor;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.util.Locale;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

/**
 * Contributes letter status information to the /actuator/info endpoint.
 */
@Component
public class LetterStatusInfoContributor implements InfoContributor {

    private final LetterRepository repo;

    /**
     * Create a new instance.
     *
     * @param repo the repository to get letter status information from
     */
    public LetterStatusInfoContributor(LetterRepository repo) {
        this.repo = repo;
    }

    /**
     * Contributes letter status information to the /actuator/info endpoint.
     *
     * @param builder the builder to contribute to
     */
    @Override
    public void contribute(Info.Builder builder) {
        builder.withDetail(
            "letters_by_status",
            Stream.of(LetterStatus.values())
                .collect(
                    toMap(
                        s -> s.name().toLowerCase(Locale.ENGLISH),
                        s -> repo.countByStatus(s)
                    )
                )
        );
    }
}
