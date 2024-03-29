package uk.gov.hmcts.reform.sendletter.entity;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class DocumentRepositoryTest {

    @Autowired
    private DocumentRepository repository;

    @Autowired
    private LetterRepository letterRepository;

    @BeforeEach
    void setUp() {
        repository.deleteAll();
    }

    @AfterEach
    void tearDown() {
        repository.deleteAll();
    }

    @Test
    void findOneCreatedAfter_should_return_empty_if_document_created_with_different_recipients() {
        // given
        final Letter letter = SampleData.letterEntity("aService", LocalDateTime.now());
        final Letter savedLetter = letterRepository.save(letter);

        final String checkSum = UUID.randomUUID().toString();
        final Document document = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum,
            "any",
            LocalDateTime.now().minusHours(2)
        );
        repository.save(document);

        // when
        Optional<Document> documentFound = repository.findOneCreatedAfter(checkSum, "something different",
            LocalDateTime.now().minusHours(1));

        // then
        assertThat(documentFound).isEmpty();
    }

    @Test
    void findOneCreatedAfter_should_return_not_empty_if_single_document_created_after() {
        // given
        final Letter letter = SampleData.letterEntityWithRecipients("aService", LocalDateTime.now(),
            Arrays.asList("one", "two"));
        final Letter savedLetter = letterRepository.save(letter);

        final String checkSum = UUID.randomUUID().toString();
        final Document document = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum,
            "any",
            LocalDateTime.now().minusMinutes(30)
        );
        repository.save(document);

        // when
        Optional<Document> documentFound = repository.findOneCreatedAfter(checkSum, "any",
            LocalDateTime.now().minusHours(1));

        // then
        assertThat(documentFound).isNotEmpty();
    }

    @Test
    void findOneCreatedAfter_should_return_empty_if_multiple_document_created_after() {
        // given
        final Letter letter = SampleData.letterEntityWithRecipients("aService", LocalDateTime.now(),
            Arrays.asList("two", "three"));
        final Letter savedLetter = letterRepository.save(letter);

        final String checkSum = UUID.randomUUID().toString();
        final Document document1 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum,
            "checksumRecipients",
            LocalDateTime.now().minusMinutes(30)
        );
        final Document document2 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum,
            "checksumRecipients",
            LocalDateTime.now().minusMinutes(40)
        );
        final Document document3 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum,
            "checksumRecipients",
            LocalDateTime.now().minusHours(2)
        );
        repository.save(document1);
        repository.save(document2);
        repository.save(document3);

        // when
        Optional<Document> documentFound = repository.findOneCreatedAfter(checkSum,
            "checksumRecipients",
            LocalDateTime.now().minusHours(1));

        // then
        assertThat(documentFound).isNotEmpty();
    }

    @Test
    void findOneCreatedAfter_should_return_empty_if_document_with_different_checksums_created_after() {
        // given
        final Letter letter = SampleData.letterEntity("aService", LocalDateTime.now());
        final Letter savedLetter = letterRepository.save(letter);

        final String checkSum1 = UUID.randomUUID().toString();
        final String checkSum2 = UUID.randomUUID().toString();
        final Document document1 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum1,
            "any",
            LocalDateTime.now().minusMinutes(30)
        );
        final Document document2 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum2,
            "any number 2",
            LocalDateTime.now().minusHours(2)
        );
        repository.save(document1);
        repository.save(document2);

        // when
        Optional<Document> documentFound = repository.findOneCreatedAfter(checkSum2, "any number 3",
            LocalDateTime.now().minusHours(1));

        // then
        assertThat(documentFound).isEmpty();
    }

    @Test
    void findOneCreatedAfter_should_return_not_null_values() {
        // given
        final Letter letter = SampleData.letterEntity("aService", LocalDateTime.now());
        final Letter savedLetter = letterRepository.save(letter);

        final String checkSum1 = UUID.randomUUID().toString();
        final Document document1 = SampleData.documentEntity(
            savedLetter.getId(),
            checkSum1,
            "any",
            LocalDateTime.now().minusMinutes(30)
        );
        final Document savedDocument = repository.save(document1);

        // when
        Optional<Document> documentFound = repository.findById(savedDocument.getId());

        // then
        assertThat(documentFound).isNotEmpty();
        assertThat(documentFound.get().getLetterId()).isEqualTo(savedLetter.getId());
        assertThat(documentFound.get().getChecksum()).isEqualTo(checkSum1);
        assertThat(documentFound.get().getCreatedAt()).isNotNull();
    }
}
