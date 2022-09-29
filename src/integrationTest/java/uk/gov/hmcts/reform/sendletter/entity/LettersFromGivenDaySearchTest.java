package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import static java.time.LocalDateTime.now;
import static org.assertj.core.api.Assertions.assertThat;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Posted;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class LettersFromGivenDaySearchTest {

    private static final String SMOKE_TEST_LETTER_TYPE = "smoke_test";
    public static final String TYPE_1 = "type-1";
    public static final String TYPE_2 = "type-2";

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
    void should_read_expected_data_from_db() {
        // given
        LocalDateTime createdAt = now();
        Letter letter = SampleData.letterEntity("service1", createdAt, "some_type");
        letter.setStatus(Posted);
        letter.setPrintedAt(createdAt.plusDays(1));
        letter.setSentToPrintAt(createdAt.minusDays(2));

        repository.save(letter);

        // when
        List<BasicLetterInfo> letters = repository.findCreatedAt(LocalDate.now());

        // then
        assertThat(letters).hasSize(1);
        BasicLetterInfo actual = letters.get(0);
        // assertThat(actual.getCreatedAt()).isEqualTo(letter.getCreatedAt());
        // assertThat(actual.getSentToPrintAt()).isEqualTo(letter.getSentToPrintAt());
        // assertThat(actual.getPrintedAt()).isEqualTo(letter.getPrintedAt());
        assertThat(actual.getService()).isEqualTo(letter.getService());
        assertThat(actual.getType()).isEqualTo(letter.getType());
        assertThat(actual.getStatus()).isEqualTo(letter.getStatus().toString());
    }

    @Test
    void should_exclude_letters_created_on_different_day() {
        // given
        storeLetter(Uploaded, TYPE_1, now());
        storeLetter(Posted, TYPE_2, now());
        storeLetter(Uploaded, TYPE_1, now().minusDays(1));
        storeLetter(Posted, TYPE_2, now().plusDays(1));

        // when
        List<BasicLetterInfo> letters = repository.findCreatedAt(LocalDate.now());

        // then
        assertThat(letters).hasSize(2);
    }

    @Test
    public void should_exclude_smoke_test_letters() {
        // given
        storeLetter(Uploaded, TYPE_1, now());
        storeLetter(Posted, TYPE_2, now());
        storeLetter(Posted, SMOKE_TEST_LETTER_TYPE, now());
        storeLetter(Posted, SMOKE_TEST_LETTER_TYPE, now());

        // when
        List<BasicLetterInfo> letters = repository.findCreatedAt(LocalDate.now());

        // then
        assertThat(letters).hasSize(2);
    }

    private void storeLetter(LetterStatus status, String type, LocalDateTime createdAt) {
        Letter letter = SampleData.letterEntity("service1", createdAt, type);
        letter.setStatus(status);
        repository.save(letter);
    }
}
