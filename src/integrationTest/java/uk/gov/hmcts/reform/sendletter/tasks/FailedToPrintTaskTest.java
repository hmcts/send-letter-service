package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit4.SpringRunner;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.services.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.util.MessageIdProvider;

import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@RunWith(SpringRunner.class)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
public class FailedToPrintTaskTest {

    @Autowired
    LetterRepository repository;

    @Value("${ftp.downtime.from}")
    private String downtimeFromHour;

    @Value("${ftp.downtime.to}")
    private String downtimeToHour;

    private LocalTime cutOff;

    private LocalTime secondBeforeCutOff;

    @Mock
    private AppInsights insights;

    private FailedToPrintTask task;

    @Before
    public void setUp() {
        FtpAvailabilityChecker availabilityChecker = new FtpAvailabilityChecker(downtimeFromHour, downtimeToHour);
        task = new FailedToPrintTask(repository, insights, availabilityChecker);
        cutOff = availabilityChecker.getDowntimeStart();
        secondBeforeCutOff = cutOff.minusSeconds(1);

        // making sure each test has fresh table
        repository.deleteAll();
    }

    @Test
    public void should_do_nothing_when_there_are_no_unprinted_letters() {
        // given
        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        // when
        task.run();

        // then
        verify(insights, never()).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).isEmpty();
    }

    @Test
    @SuppressWarnings("VariableDeclarationUsageDistance")
    public void should_report_to_insights_when_there_is_an_unprinted_letter() {
        // given
        Letter letter = createLetter(secondBeforeCutOff);

        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        // when
        task.run();

        // then
        verify(insights).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).hasSize(1);
        assertThat(captor.getValue().getId()).isEqualByComparingTo(letter.getId());
        assertThat(captor.getValue().getSentToPrintAt()).isEqualToIgnoringMillis(
            Date.from(ZonedDateTime.now().minusDays(1).with(secondBeforeCutOff).toInstant())
        );
    }

    @Test
    public void should_not_pick_up_letter_if_sent_to_print_happened_after_the_cutoff() {
        // given
        createLetter(cutOff);

        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        // when
        task.run();

        // then
        verify(insights, never()).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).isEmpty();
    }

    @Test
    public void should_not_pick_up_letter_if_not_sent_to_print() {
        // given
        createLetter(null);

        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        // when
        task.run();

        // then
        verify(insights, never()).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).isEmpty();
    }

    @Test
    public void should_not_pick_up_letter_if_it_is_marked_as_failed() {
        // given
        Letter letter = createLetter(secondBeforeCutOff);
        letter.hasFailed(true);

        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        //when
        repository.save(letter);
        task.run();

        // then
        verify(insights, never()).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).isEmpty();
    }

    @Test
    public void should_not_pick_up_letter_if_it_is_already_printed() {
        // given
        Letter letter = createLetter(secondBeforeCutOff);
        letter.setState(LetterState.Posted);
        letter.setPrintedAt(Timestamp.from(Instant.now()));

        ArgumentCaptor<Letter> captor = ArgumentCaptor.forClass(Letter.class);

        // when
        repository.save(letter);
        task.run();

        // then
        verify(insights, never()).trackNotPrintedLetter(captor.capture());

        // and
        assertThat(captor.getAllValues()).isEmpty();
    }

    private Letter createLetter(LocalTime withSentToPrintAt) {
        Letter letter = new Letter(MessageIdProvider.randomMessageId(), "service", null, "type", null);

        if (withSentToPrintAt != null) {
            letter.setState(LetterState.Uploaded);
            letter.setSentToPrintAt(
                Timestamp.valueOf(LocalDateTime.now()
                    .minusDays(1)
                    .with(withSentToPrintAt)
                )
            );
        }

        return repository.save(letter);
    }
}
