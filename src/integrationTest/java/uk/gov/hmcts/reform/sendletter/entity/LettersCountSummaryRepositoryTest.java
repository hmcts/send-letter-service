package uk.gov.hmcts.reform.sendletter.entity;

import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCount;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class LettersCountSummaryRepositoryTest {

    @Autowired
    private LettersCountSummaryRepository repository;

    @Test
    void should_get_letters_count_group_by_service() throws ParseException {
        //given
        LocalDateTime date = LocalDateTime.parse("2019-04-24T10:15:30");
        repository.save(letterEntity("a.service", date.minusHours(1)));
        repository.save(letterEntity("a.service", date.plusHours(5)));
        repository.save(letterEntity("a.service", date.minusDays(1))); //previous day
        repository.save(letterEntity("b.service", date.minusHours(2)));

        List<Letter> letters = Lists.newArrayList(repository.findAll());
        assertThat(letters.size()).isEqualTo(4);

        //when
        Date dateFrom = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse("2019-04-23 17:00");
        Date dateTo = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse("2019-04-24 16:00");

        List<ServiceLettersCountSummary> result = repository.countByDate(dateFrom, dateTo);

        //then
        assertThat(result)
            .isNotEmpty()
            .hasSize(2)
            .usingFieldByFieldElementComparator()
            .containsExactlyElementsOf(
                Arrays.asList(
                    new ServiceLettersCount("a.service", 2),
                    new ServiceLettersCount("b.service", 1)
                )
            );
    }

    @Test
    void should_return_empty_when_no_letters_uploaded_for_the_requested_date() throws ParseException {
        //given
        LocalDateTime date = LocalDateTime.parse("2019-04-24T14:00:00");
        repository.save(letterEntity("a.service", date.minusDays(1))); //previous day
        repository.save(letterEntity("b.service", date.minusDays(2)));

        List<Letter> letters = Lists.newArrayList(repository.findAll());
        assertThat(letters.size()).isEqualTo(2);

        //when
        Date dateFrom = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse("2019-04-24 17:00");
        Date dateTo = new SimpleDateFormat("yyyy-MM-dd HH:mm").parse("2019-04-25 16:00");

        List<ServiceLettersCountSummary> result = repository.countByDate(dateFrom, dateTo);

        //then
        assertThat(result).isEmpty();
    }

    private Letter letterEntity(String service, LocalDateTime sentToPrintAt) {
        Letter letter = SampleData.letterEntity(service);
        letter.setSentToPrintAt(sentToPrintAt);
        return letter;
    }
}
