package uk.gov.hmcts.reform.sendletter.services.zip;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.exception.DocumentZipException;
import uk.gov.hmcts.reform.sendletter.model.PdfDoc;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Utility class to zip PDF documents.
 */
@Component
public class Zipper {

    /**
     * Zips a PDF document.
     *
     * @param filename The filename
     * @param input    The PDF document
     * @return The zipped PDF document
     * @throws IOException If an error occurs while zipping the PDF document
     */
    private byte[] zipBytes(String filename, byte[] input) throws IOException {

        ZipEntry entry = new ZipEntry(filename);
        entry.setSize(input.length);

        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try (ZipOutputStream zos = new ZipOutputStream(baos)) {
            zos.putNextEntry(entry);
            zos.write(input);
            zos.closeEntry();
        }

        return baos.toByteArray();
    }

    /**
     * Zips a PDF document.
     *
     * @param pdfDoc The PDF document
     * @return The zipped PDF document
     */
    public byte[] zip(PdfDoc pdfDoc) {
        try {
            return zipBytes(pdfDoc.filename, pdfDoc.content);
        } catch (IOException exception) {
            throw new DocumentZipException(exception);
        }
    }
}
