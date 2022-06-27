package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.Instant;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static uk.gov.hmcts.reform.sendletter.entity.EventType.MANUALLY_MARKED_AS_CREATED;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class LetterEventRepositoryTest {

    @Autowired
    private LetterEventRepository repository;

    @Autowired
    private LetterRepository letterRepository;

    @BeforeEach
    void setUp() {
        repository.deleteAll();
        letterRepository.deleteAll();
    }

    @AfterEach
    void tearDown() {
        repository.deleteAll();
        letterRepository.deleteAll();
    }

    @Test
    void should_save_letter_event() {
        // given
        Letter letter = SampleData.letterEntity("service1");

        Letter savedLetter = letterRepository.save(letter);

        LetterEvent event = new LetterEvent(savedLetter, MANUALLY_MARKED_AS_CREATED, "notes", Instant.now());

        // when
        repository.save(event);

        // then
        assertThat(repository.findAll())
            .extracting(ev ->
                tuple(ev.getLetter(), ev.getType(), ev.getNotes(), ev.getCreatedAt())
            )
            .containsExactly(
                tuple(event.getLetter(), event.getType(), event.getNotes(), event.getCreatedAt())
            );
    }
}
