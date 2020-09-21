package uk.gov.hmcts.reform.sendletter;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.gov.hmcts.reform.sendletter.controllers.MediaTypes;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;

import java.io.IOException;

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
                samplePdfLetterRequestJson("letter-with-document-count.json", "Docmosis-Template-Guide.pdf")
        );
        logger.info("Letter id created {}", letterId);
        String letterStatus = verifyLetterCreated(letterId, this::logRetry);
        assertThat(letterStatus).isEqualTo(LetterStatus.Created.toString());
    }

    private void logRetry(String letterId, Integer counter) {
        logger.info("Letter id {} not found so, retrying {}", letterId, counter);
    }

}
