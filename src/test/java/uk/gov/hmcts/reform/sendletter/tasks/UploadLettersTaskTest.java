package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.services.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.zip.ZippedDoc;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;

import java.time.LocalTime;
import java.util.stream.Stream;

import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.entity.LetterState.Created;
import static uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask.SMOKE_TEST_LETTER_TYPE;

@RunWith(MockitoJUnitRunner.class)
public class UploadLettersTaskTest {

    @Mock private LetterRepository repo;
    @Mock private Zipper zipper;
    @Mock private FtpClient ftpClient;
    @Mock private FtpAvailabilityChecker availabilityChecker;

    private UploadLettersTask task;

    @Before
    public void setUp() throws Exception {
        given(availabilityChecker.isFtpAvailable(any(LocalTime.class))).willReturn(true);
        this.task = new UploadLettersTask(repo, zipper, ftpClient, availabilityChecker);
    }

    @Test
    public void should_handle_smoke_test_letters() throws Exception {
        given(zipper.zip(any(), any())).willReturn(new ZippedDoc("hello.zip", "hello".getBytes()));

        givenDbContains(letterOfType(SMOKE_TEST_LETTER_TYPE));
        task.run();
        verify(ftpClient).upload(any(), eq(true));

        givenDbContains(letterOfType("not_" + SMOKE_TEST_LETTER_TYPE));
        task.run();
        verify(ftpClient).upload(any(), eq(false));
    }

    private Letter letterOfType(String type) {
        return new Letter("msgId", "cmc", null, type, "hello".getBytes());
    }

    private void givenDbContains(Letter letter) {
        given(repo.findByState(Created))
            .willReturn(Stream.of(letter));
    }

}
