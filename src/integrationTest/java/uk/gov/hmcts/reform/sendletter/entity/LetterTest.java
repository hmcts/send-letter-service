package uk.gov.hmcts.reform.sendletter.entity;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.assertj.core.util.Lists;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.test.context.junit4.SpringRunner;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.data.model.DbLetter;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.UUID;
import javax.sql.DataSource;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(SpringRunner.class)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class LetterTest {

    @Autowired
    private LetterRepository repository;

    @Autowired
    private DataSource dataSource;

    public static Letter getTestLetter() {
        return SampleData.letterEntity("a_service");
    }

    @Test
    public void should_successfully_save_report_in_db() {
        repository.save(getTestLetter());
        List<Letter> letters = Lists.newArrayList(repository.findAll());
        assertThat(letters.size()).isEqualTo(1);
        assertThat(letters.get(0).getState()).isEqualTo(LetterState.Created);
    }

    @Test
    public void compatible_with_existing_records() throws Exception {
        // Save a letter using the existing repository code.
        NamedParameterJdbcTemplate jdbcTemplate = new NamedParameterJdbcTemplate(dataSource);
        ObjectMapper objectMapper = new ObjectMapper();
        uk.gov.hmcts.reform.sendletter.data.LetterRepository repo =
            new uk.gov.hmcts.reform.sendletter.data.LetterRepository(jdbcTemplate, objectMapper);
        DbLetter dbLetter = new DbLetter(UUID.randomUUID(), "cmc", SampleData.letter());
        Instant instant = Instant.now();
        String messageId = UUID.randomUUID().toString();
        repo.save(dbLetter, instant, messageId);

        List<Letter> letters = Lists.newArrayList(repository.findAll());
        assertThat(letters.size()).isEqualTo(1);

        Letter loaded = letters.get(0);
        String expectedData = objectMapper.writeValueAsString(SampleData.letter().additionalData);
        assertThat(loaded.getAdditionalData().toString()).isEqualTo(expectedData);
        assertThat(loaded.getCreatedAt()).isEqualTo(Timestamp.from(instant));
        assertThat(loaded.getMessageId()).isEqualTo(messageId);
        assertThat(loaded.getService()).isEqualTo("cmc");
        assertThat(loaded.getType()).isEqualTo(dbLetter.type);
    }

    @Test
    public void finds_letters_by_id_and_service() {
        repository.save(getTestLetter());
        Letter second = SampleData.letterEntity("different");
        repository.save(second);

        Letter found = repository.findOptionalByIdAndService(second.getId(), second.getService()).get();
        assertThat(found.getId()).isEqualTo(second.getId());
    }
}
