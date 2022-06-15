package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.NotSent;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;
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
}
