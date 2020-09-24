package uk.gov.hmcts.reform.sendletter;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.util.function.BiConsumer;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(SpringExtension.class)
class ProcessMessageTestForPdfEndpointV2Async extends FunctionalTestSuite {
    private static Logger logger = LoggerFactory.getLogger(ProcessMessageTestForPdfEndpointV2Async.class);

    @Test
    @Disabled
    void should_send_letter_and_upload_file_on_sftp_server_when_letter_contains_two_pdf_document() throws Exception {
        String letterId = sendPrintLetterRequestAsync(
            signIn(),
            samplePdfLetterRequestJson("letter-with-ten-pdfs.json", "test.pdf")
        );

        String letterStatus = verifyLetterCreated(letterId, this::logRetry);
        assertThat(letterStatus).isNotEqualTo(LetterStatus.Created.toString());
    }

    private void logRetry(String letterId, Integer counter) {
        logger.info("Letter id {} not found so, retrying {}", letterId, counter);
    }

    @Override
    String getContentType() {
        return MediaTypes.LETTER_V2;
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
