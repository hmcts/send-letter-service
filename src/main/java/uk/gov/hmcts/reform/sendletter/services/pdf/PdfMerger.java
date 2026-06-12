package uk.gov.hmcts.reform.sendletter.services.pdf;

import lombok.extern.slf4j.Slf4j;
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
@Slf4j
public final class PdfMerger {

    /**
     * Utility class constructor.
     */
    private PdfMerger() {
    }

    /**
     * Merges a list of PDF documents into a single PDF document.
     *
     * @param documents      The list of PDF documents
     * @param loggingContext The logging context
     * @return The merged PDF document
     */
    public static byte[] mergeDocuments(List<byte[]> documents, String loggingContext) {
        if (documents.size() == 1) {
            return documents.getFirst();
        }

        try {
            return mergeDocuments(documents, loggingContext, false);
        } catch (IOException e) {
            try {
                log.warn("Error while merging documents, attempting optimised merge", e);
                return mergeDocuments(documents, loggingContext, true);
            } catch (IOException e1) {
                log.warn("Optimised merge failed, throwing original exception");
                throw new PdfMergeException("Exception occurred while merging PDF files." + loggingContext, e);
            }
        }
    }

    private static byte[] mergeDocuments(List<byte[]> documents, String loggingContext, boolean optimizeResourcesMode)
        throws IOException {
        ByteArrayOutputStream docOutputStream = new ByteArrayOutputStream();

        List<RandomAccessRead> inputStreams = documents.stream()
            .map(RandomAccessReadBuffer::new)
            .collect(toList());

        PDFMergerUtility pdfMergerUtility = new PDFMergerUtility();
        pdfMergerUtility.addSources(inputStreams);
        pdfMergerUtility.setDestinationStream(docOutputStream);

        if (optimizeResourcesMode) {
            // FACT-2863 - adding mode switch to prevent documents with unsupported structures from
            // causing merge failure. This mode effectively reduces supported content to "Page
            // content and resources".
            pdfMergerUtility.setDocumentMergeMode(PDFMergerUtility.DocumentMergeMode.OPTIMIZE_RESOURCES_MODE);
        }

        pdfMergerUtility.mergeDocuments(setupMainMemoryOnly().streamCache);
        return docOutputStream.toByteArray();
    }
}
