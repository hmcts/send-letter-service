package uk.gov.hmcts.reform.sendletter;

import org.apache.pdfbox.io.RandomAccessReadBuffer;
import org.apache.pdfbox.pdfparser.PDFParser;

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

            PDFParser pdfParser = new PDFParser(new RandomAccessReadBuffer(zip));
            pdfParser.parse();
        }
    }

    private PdfHelper() {
        // Prevent instantiation.
    }
}
