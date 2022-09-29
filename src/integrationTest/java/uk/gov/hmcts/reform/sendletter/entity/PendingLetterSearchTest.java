package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class PendingLetterSearchTest {

    private static final String SMOKE_TEST_LETTER_TYPE = "smoke_test";

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
    public void should_return_letters_in_created_status() {
        // given

        storeLetter(Created, "type-3", LocalDateTime.now().minusMinutes(2));
        storeLetter(Created, "type-4", LocalDateTime.now().minusHours(1));
        storeLetter(Uploaded, "type-5", LocalDateTime.now());

        // when
        List<BasicLetterInfo> letters = repository.findPendingLetters();

        // then
        assertThat(letters)
            .extracting(BasicLetterInfo::getType)
            .containsExactly("type-4", "type-3")
            .doesNotContain("type-5");
    }

    @Test
    public void should_return_letters_in_created_status_before_given_time() {
        LocalDateTime currentTime = LocalDateTime.now();
        // given
        storeLetter(Uploaded, "type-1", currentTime.minusMinutes(10));
        storeLetter(Uploaded, "type-2", currentTime.minusMinutes(10));
        storeLetter(Created, "type-3", currentTime.minusMinutes(10));
        storeLetter(Created, "type-4", currentTime.minusMinutes(10));
        storeLetter(Created, "type-5", currentTime.minusMinutes(2));

        // when
        try (Stream<BasicLetterInfo> letters = repository
            .findByCreatedAtBeforeAndStatusAndTypeNot(currentTime.minusMinutes(5), Created,
                UploadLettersTask.SMOKE_TEST_LETTER_TYPE)) {
            // then
            assertThat(letters)
                .extracting(BasicLetterInfo::getType)
                .containsOnly("type-3", "type-4");
        }
    }

    @Test
    public void should_not_include_smoke_test_letters_in_the_result() {
        // given
        storeLetter(Created, SMOKE_TEST_LETTER_TYPE, LocalDateTime.now());
        storeLetter(Created, "not-smoke-test-type-1", LocalDateTime.now());
        storeLetter(Created, "not-smoke-test-type-2", LocalDateTime.now());

        // when
        List<BasicLetterInfo> letters = repository.findPendingLetters();

        // then
        assertThat(letters).hasSize(2)
            .noneMatch(l -> l.getStatus().equals(SMOKE_TEST_LETTER_TYPE));

        assertThat(letters).extracting("type")
            .containsExactlyInAnyOrder("not-smoke-test-type-1", "not-smoke-test-type-2");
    }

    @Test
    public void should_not_include_smoke_test_letters_in_the_result_before_given_time() {
        LocalDateTime currentTime = LocalDateTime.now();
        // given
        storeLetter(Created, SMOKE_TEST_LETTER_TYPE, currentTime.minusMinutes(30));
        storeLetter(Created, "not-smoke-test-type-1", currentTime.minusMinutes(30));
        storeLetter(Created, "not-smoke-test-type-2", currentTime.minusMinutes(30));
        List<BasicLetterInfo> collect;
        // when
        try (Stream<BasicLetterInfo> letters = repository
            .findByCreatedAtBeforeAndStatusAndTypeNot(currentTime.minusMinutes(5), Created,
                UploadLettersTask.SMOKE_TEST_LETTER_TYPE)) {
            collect = letters.collect(toList());
        }
        assertThat(collect.size()).isEqualTo(2);
        assertThat(collect).extracting("type")
            .containsExactlyInAnyOrder("not-smoke-test-type-1", "not-smoke-test-type-2")
            .doesNotContain(SMOKE_TEST_LETTER_TYPE);
    }

    @Test
    public void should_return_pending_letters_with_correct_properties() {
        // given
        Letter letter1 = SampleData.letterEntity("service", LocalDateTime.now(), "type", "fingerprint",
            Map.of("Document_1", 1),
            SampleData.checkSumSupplier);
        letter1.setStatus(Created);
        Letter savedLetter1 = repository.save(letter1);

        Letter letter2 = SampleData.letterEntity(
            "service2", LocalDateTime.now().minusHours(1), "type2", "fingerprint2",
            Map.of("Document_2", 1),
            () -> UUID.randomUUID().toString());
        letter2.setStatus(Uploaded);
        repository.save(letter2);

        // when
        List<BasicLetterInfo> letters = repository.findPendingLetters();

        // then
        assertThat(letters).hasSize(1);
        final BasicLetterInfo actual = letters.get(0);
        assertThat(actual.getId()).isEqualTo(savedLetter1.getId());
        assertThat(actual.getChecksum()).isEqualTo(savedLetter1.getChecksum());
        assertThat(actual.getCreatedAt()).isEqualTo(savedLetter1.getCreatedAt());
        assertThat(actual.getEncryptionKeyFingerprint()).isEqualTo(savedLetter1.getEncryptionKeyFingerprint());
        assertThat(actual.getService()).isEqualTo(savedLetter1.getService());
        assertThat(actual.getType()).isEqualTo(savedLetter1.getType());
    }


    private void storeLetter(LetterStatus status, String type, LocalDateTime createdAt) {
        Letter letter = SampleData.letterEntity("service1", createdAt, type);
        letter.setStatus(status);
        repository.save(letter);
    }
}
