package uk.gov.hmcts.reform.sendletter.services.pdf;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.exception.DuplexException;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * This class represents the duplex preparator.
 */
@Component
public class DuplexPreparator {
    private static final Logger logger = LoggerFactory.getLogger(DuplexPreparator.class);

    /**
     * Adds an extra blank page if the total number of pages is odd.
     */
    public byte[] prepare(byte[] pdf, String loggingContext) {
        logger.info("File size is {} KB", pdf.length / 1024);
        try (var pdDoc = PDDocument.load(pdf)) {
            int numberOfPages = pdDoc.getNumberOfPages();
            logger.info("File has {} pages.", numberOfPages);
            if (numberOfPages % 2 == 1) {
                PDRectangle lastPageMediaBox = pdDoc.getPage(numberOfPages - 1).getMediaBox();
                pdDoc.addPage(new PDPage(lastPageMediaBox));
                var out = new ByteArrayOutputStream();
                pdDoc.save(out);

                return out.toByteArray();
            } else {
                return pdf;
            }
        } catch (IOException exc) {
            String errorMessage = "Failed to parse the documents for " + loggingContext;
            logger.error(errorMessage);
            throw new DuplexException(errorMessage, exc);
        }
    }
}
