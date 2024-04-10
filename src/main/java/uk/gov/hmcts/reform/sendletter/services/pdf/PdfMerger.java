package uk.gov.hmcts.reform.sendletter.services.pdf;

import org.apache.pdfbox.io.RandomAccessRead;
import org.apache.pdfbox.io.RandomAccessReadBuffer;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import uk.gov.hmcts.reform.sendletter.exception.PdfMergeException;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.apache.pdfbox.io.MemoryUsageSetting.setupMainMemoryOnly;

/**
 * Utility class to merge PDF documents.
 */
public final class PdfMerger {

    /**
     * Utility class constructor.
     */
    private PdfMerger() {
    }

    /**
     * Merges a list of PDF documents into a single PDF document.
     *
     * @param documents       The list of PDF documents
     * @param loggingContext  The logging context
     * @return The merged PDF document
     */
    public static byte[] mergeDocuments(List<byte[]> documents, String loggingContext) {
        if (documents.size() == 1) {
            return documents.get(0);
        }

        ByteArrayOutputStream docOutputStream = new ByteArrayOutputStream();

        List<RandomAccessRead> inputStreams = documents.stream()
            .map(RandomAccessReadBuffer::new)
            .collect(toList());

        PDFMergerUtility pdfMergerUtility = new PDFMergerUtility();
        pdfMergerUtility.addSources(inputStreams);
        pdfMergerUtility.setDestinationStream(docOutputStream);

        try {
            pdfMergerUtility.mergeDocuments(setupMainMemoryOnly().streamCache);
            return docOutputStream.toByteArray();
        } catch (IOException e) {
            throw new PdfMergeException("Exception occurred while merging PDF files." + loggingContext, e);
        }
    }
}
