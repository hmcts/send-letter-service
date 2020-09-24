package uk.gov.hmcts.reform.sendletter;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.io.IOException;
import java.util.function.BiConsumer;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(SpringExtension.class)
public class ProcessSaveV3Asyn extends FunctionalTestSuite {
    private static Logger logger = LoggerFactory.getLogger(ProcessSaveV3Asyn.class);

    @Override
    String getContentType() {
        return MediaTypes.LETTER_V3;
    }

    @Test
    public void testSaveLetterAsync() throws IOException {
        String letterId = sendPrintLetterRequestAsync(
                signIn(),
                samplePdfLetterRequestJson("letter-with-document-count.json", "test.pdf")
        );
        System.out.println("Async Letter id created " + letterId);

        logger.info("Letter id created {}", letterId);
        String letterStatus = verifyLetterCreated(letterId, this::logRetry);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.name());
    }

    private void logRetry(String letterId, Integer counter) {
        logger.info("Letter id {} not found so, retrying {}", letterId, counter);
    }

    private String verifyLetterCreated(String letterId, BiConsumer<String, Integer> logger) {
        int counter = 1;
        String letterStatus = "Not found";
        while (letterStatus.equals("Not found") && counter <= 10) {
            try {
                System.out.println(String.format(" TRYING TO FIND Letter"
                        + " id %s not found so, retrying %s", letterId, counter));
                logger.accept(letterId, counter++);
                letterStatus = getLetterStatus(letterId);
            } catch (AssertionError e) {
                System.out.println(String.format("ERROR Letter id %s "
                        + "not found so, retrying %s", letterId, counter));
                logger.accept(letterId, counter++);
                try {
                    Thread.sleep(100);
                } catch (InterruptedException interruptedException) {
                    interruptedException.printStackTrace();
                }
            }
        }
        return letterStatus;
    }
}
