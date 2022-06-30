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
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.*;

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

    @Test
    void should_change_letter_status_to_created() {
        // given
        Letter letter = SampleData.letterEntity("aService");

        letter.setStatus(Uploaded);

        Letter savedLetter = repository.save(letter);

        // when
        LocalDateTime postedAt = LocalDateTime.now();
        int updateCount = repository.markStaleLetterAsCreated(savedLetter.getId(), postedAt);

        // then
        assertThat(updateCount).isEqualTo(1);
        assertThat(repository.findAll())
            .extracting(l ->
                tuple(l.getId(), l.getStatus(), l.getSentToPrintAt())
            )
            .containsExactly(
                tuple(savedLetter.getId(), Created, postedAt)
            );
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
        int updateCount = repository.markStaleLetterAsCreated(UUID.randomUUID(), LocalDateTime.now());

        // then
        assertThat(updateCount).isEqualTo(0);
    }

    @Test
    void should_change_status_to_aborted_for_uploaded_letter() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        letter.setStatus(Uploaded);

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

}
