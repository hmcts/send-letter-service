package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Stream;

import static java.time.LocalDateTime.now;
import static java.util.Arrays.asList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@ExtendWith(MockitoExtension.class)
class PendingLettersServiceTest {

    @Mock
    private LetterRepository repo;

    private PendingLettersService service;

    @BeforeEach
    void setUp() {
        service = new PendingLettersService(repo);
    }

    @Test
    void should_return_list_of_letters_from_repo() {
        // given
        List<BasicLetterInfo> letters =
            asList(
                new BasicLetterInfo(randomUUID(), "c1", "s1", Created, "t1", "f1", now(), now().minusSeconds(1), null),
                new BasicLetterInfo(randomUUID(), "c2", "s2", Created, "t1", "f2", now().minusSeconds(1), now(), null)
            );

        given(repo.findPendingLetters()).willReturn(letters);

        // when
        List<BasicLetterInfo> lettersFromDb = service.getPendingLetters();

        // then
        assertThat(lettersFromDb)
            .usingRecursiveFieldByFieldElementComparator()
            .isEqualTo(letters);
    }

    @Test
    void should_return_list_of_letters_from_repo_before_certain_time() {
        // given
        final BasicLetterInfo pendingLetter1 = new BasicLetterInfo(
            randomUUID(), "c1", "s1", Created, "t1", "f1", now(), now().minusSeconds(1), null
        );
        final BasicLetterInfo pendingLetter2 = new BasicLetterInfo(
            randomUUID(), "c2", "s2", Created, "t1", "f2", now().minusSeconds(1), now(), null
        );
        final BasicLetterInfo uploadedLetter = new BasicLetterInfo(
            randomUUID(), "c3", "s3", Uploaded, "t1", "f3", now().minusHours(1), now(), null
        );

        List<BasicLetterInfo> pendingLetters = asList(pendingLetter1, pendingLetter2);

        given(repo.findByCreatedAtBeforeAndStatusAndTypeNot(isA(LocalDateTime.class), eq(Created),
            eq(UploadLettersTask.SMOKE_TEST_LETTER_TYPE))).willReturn(pendingLetters.stream());

        // when
        try (Stream<BasicLetterInfo> lettersFromDb = service.getPendingLettersCreatedBeforeTime(5)) {
            // then
            assertThat(lettersFromDb)
                .usingRecursiveFieldByFieldElementComparator()
                .isEqualTo(pendingLetters);
        }
    }
}
