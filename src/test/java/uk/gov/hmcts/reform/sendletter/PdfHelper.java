package uk.gov.hmcts.reform.sendletter;

import org.apache.pdfbox.io.RandomAccessReadBuffer;
import org.apache.pdfbox.pdfparser.PDFParser;
import org.apache.pdfbox.preflight.PreflightDocument;

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

            PDFParser pdfParser1 = new PDFParser(new RandomAccessReadBuffer(zip));
            pdfParser1.parse();
            PreflightDocument document = (PreflightDocument) pdfParser1.getPDDocument();

            document.validate();
        }
    }

    private PdfHelper() {
        // Prevent instantiation.
    }
}
