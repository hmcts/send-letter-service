package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

import static java.time.LocalDateTime.now;
import static java.util.Collections.singletonList;
import static org.apache.commons.lang3.RandomStringUtils.randomAlphanumeric;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;

@ExtendWith(MockitoExtension.class)
class LetterInfoServiceTest {

    @Mock LetterRepository repo;

    LetterInfoService service;

    @BeforeEach
    void setUp() {
        this.service = new LetterInfoService(repo);
    }

    @Test
    void should_map_repository_result() {
        // given
        LocalDate date = LocalDate.now();

        BasicLetterInfo dbLetter =
            new BasicLetterInfo(
                UUID.randomUUID(),
                randomAlphanumeric(5),
                randomAlphanumeric(5),
                Posted,
                randomAlphanumeric(3),
                randomAlphanumeric(10),
                now().minusDays(3),
                now().minusDays(2),
                now().minusDays(1)
            );

        given(repo.findCreatedAt(date)).willReturn(singletonList(dbLetter));

        // when
        List<LetterStatus> result = service.findAll(date);

        // then
        assertThat(result).hasSize(1);
        assertThat(result.get(0))
            .satisfies(letter -> {
                assertThat(letter.id).isEqualTo(dbLetter.getId());
                assertThat(letter.status).isEqualTo(dbLetter.getStatus());
                assertThat(letter.checksum).isEqualTo(dbLetter.getChecksum());
                assertThat(letter.createdAt).isEqualTo(dbLetter.getCreatedAt());
                assertThat(letter.sentToPrintAt).isEqualTo(dbLetter.getSentToPrintAt());
                assertThat(letter.printedAt).isEqualTo(dbLetter.getPrintedAt());
        });
    }

}
