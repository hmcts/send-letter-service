package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Aborted;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.NotSent;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.PostedLocally;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class LetterRepositoryTest {

    @Autowired
    private LetterRepository repository;

    @BeforeEach
    void setUp() {
        repository.deleteAll();
    }

    @AfterEach
    void tearDown() {
        repository.deleteAll();
    }

    @Test
    void should_change_status_of_uploaded_letter() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Uploaded);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markStaleLetterAsNotSent(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(1);
        assertThat(repository.findAll())
            .extracting(l ->
                tuple(l.getId(), l.getStatus())
            )
            .containsExactly(
                tuple(savedLetter.getId(), NotSent)
            );
    }

    @Test
    void should_not_change_status_of_uploaded_letter_with_different_id() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Uploaded);

        repository.save(letter);

        // when
        int updateCount = repository.markStaleLetterAsNotSent(UUID.randomUUID());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void should_not_change_status_of_posted_letter() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Posted);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markStaleLetterAsNotSent(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @ParameterizedTest
    @EnumSource(
        value = LetterStatus.class,
        names = {"Uploaded", "FailedToUpload"},
        mode = EnumSource.Mode.INCLUDE)
    void should_change_letter_status_to_created(LetterStatus status) {
        // given
        Letter letter = SampleData.letterEntity("aService");
        letter.setStatus(status);
        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsCreated(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(1);
        assertThat(repository.findAll())
            .extracting(l -> tuple(l.getId(), l.getStatus()))
            .containsExactly(tuple(savedLetter.getId(), Created));
    }

    @ParameterizedTest
    @EnumSource(
        value = LetterStatus.class,
        names = {"Uploaded", "FailedToUpload"},
        mode = EnumSource.Mode.EXCLUDE)
    void should_not_change_status_to_created_when_letter_status_is_unsupported_to_reprocess(LetterStatus status) {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(status);
        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsCreated(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(0);
    }


    @ParameterizedTest
    @EnumSource(
        value = LetterStatus.class,
        names = {"Uploaded"},
        mode = EnumSource.Mode.EXCLUDE)
    void should_not_change_letter_status_when_status_is_not_uploaded(LetterStatus status) {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(status);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markStaleLetterAsNotSent(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void should_not_change_letter_status_to_created_for_different_letter_id() {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(Uploaded);

        repository.save(letter);

        // when
        int updateCount = repository.markLetterAsCreated(UUID.randomUUID());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @ParameterizedTest
    @EnumSource(
        value = LetterStatus.class,
        names = {"Posted"},
        mode = EnumSource.Mode.EXCLUDE)
    void should_change_status_to_aborted_when_letter_status_is_not_posted(LetterStatus status) {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(status);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsAborted(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(1);
        assertThat(repository.findAll())
            .extracting(l ->
                tuple(l.getId(), l.getStatus())
            )
            .containsExactly(
                tuple(savedLetter.getId(), Aborted)
            );
    }

    @Test
    void should_not_change_status_to_aborted_for_posted_letter() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Posted);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsAborted(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void should_not_change_letter_status_to_aborted_for_different_letter_id() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Created);

        repository.save(letter);

        // when
        int updateCount = repository.markLetterAsAborted(UUID.randomUUID());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @ParameterizedTest
    @EnumSource(
        value = LetterStatus.class,
        names = {"Uploaded"},
        mode = EnumSource.Mode.EXCLUDE)
    void markLetterAsPostedLocally_should_not_change_status_to_posted_locally(LetterStatus status) {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(status);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsPostedLocally(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void markLetterAsPostedLocally_should_change_status_to_posted_locally() {
        // given
        Letter letter = SampleData.letterEntity("service1");
        letter.setStatus(Uploaded);

        Letter savedLetter = repository.save(letter);

        // when
        int updateCount = repository.markLetterAsPostedLocally(savedLetter.getId());

        // then
        assertThat(updateCount).isEqualTo(1);
        assertThat(repository.findAll())
            .extracting(l ->
                tuple(l.getId(), l.getStatus())
            )
            .containsExactly(
                tuple(savedLetter.getId(), PostedLocally)
            );
    }

    @Test
    void markLetterAsPostedLocally_should_not_change_letter_status_for_different_letter_id() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Uploaded);

        repository.save(letter);

        // when
        int updateCount = repository.markLetterAsPostedLocally(UUID.randomUUID());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void findStaleLetters_should_return_stale_letters() {
        // given
        Letter letter1 = SampleData.letterEntity("aService", LocalDateTime.now().minusDays(4));
        letter1.setStatus(Uploaded);

        Letter letter2 = SampleData.letterEntity("aService", LocalDateTime.now().minusDays(3));
        letter2.setStatus(Created);

        Letter letter3 = SampleData.letterEntity("aService", LocalDateTime.now().minusDays(4));
        letter3.setStatus(NotSent);

        Letter letter4 = SampleData.letterEntity("aService", LocalDateTime.now());
        letter3.setStatus(Aborted);

        Letter letter5 = SampleData.letterEntity("aService", LocalDateTime.now());
        letter3.setStatus(Posted);

        final Letter savedLetter1 = repository.save(letter1);
        final Letter savedLetter2 = repository.save(letter2);
        final Letter savedLetter3 = repository.save(letter3);
        final Letter savedLetter4 = repository.save(letter4);
        final Letter savedLetter5 = repository.save(letter5);

        // when
        List<BasicLetterInfo> staleLetters = repository.findStaleLetters(LocalDateTime.now().minusDays(1));

        // then
        assertThat(staleLetters).isNotEmpty().size().isEqualTo(2);
        assertThat(staleLetters)
            .extracting(l ->
                tuple(l.getId(), l.getStatus())
            )
            .contains(
                tuple(savedLetter1.getId(), Uploaded.name()),
                tuple(savedLetter2.getId(), Created.name())
            )
            .doesNotContain(
                tuple(savedLetter3.getId(), NotSent.name()),
                tuple(savedLetter4.getId(), Aborted.name()),
                tuple(savedLetter5.getId(), Posted.name())
            );
    }

    @Test
    void findStaleLetters_should_return_empty_when_no_letters_are_pending_to_print() {
        // given
        Letter letter1 = SampleData.letterEntity("aService", LocalDateTime.now().minusDays(2));
        letter1.setStatus(NotSent);

        Letter letter2 = SampleData.letterEntity("aService", LocalDateTime.now().minusDays(1));
        letter2.setStatus(Posted);
        repository.saveAll(List.of(letter1, letter2));

        // when
        // then
        assertThat(repository.findStaleLetters(LocalDateTime.now())).isEmpty();
    }
}
