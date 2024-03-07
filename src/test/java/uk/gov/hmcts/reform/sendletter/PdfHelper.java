package uk.gov.hmcts.reform.sendletter;

import org.apache.pdfbox.preflight.PreflightDocument;
import org.apache.pdfbox.preflight.parser.PreflightParser;
import org.apache.pdfbox.preflight.utils.ByteArrayDataSource;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.zip.ZipInputStream;

public final class PdfHelper {

    /**
     * Validate that data is a zipped pdf or throw an exception.
     */
    public static void validateZippedPdf(byte[] data) throws IOException {
        try (ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(data))) {

            zip.getNextEntry(); //positions the stream at the beginning of the entry data
            PreflightParser preflightParser = new PreflightParser(new ByteArrayDataSource(zip));
            preflightParser.parse();
            PreflightDocument document = preflightParser.getPreflightDocument();

            document.validate();
        }
    }

    private PdfHelper() {
        // Prevent instantiation.
    }
}
