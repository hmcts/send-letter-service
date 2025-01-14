package uk.gov.hmcts.reform.sendletter.services.pdf;

import org.apache.hc.core5.util.Asserts;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.cmc.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.pdf.service.client.PDFServiceClient;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static java.util.stream.Collectors.toList;

/**
 * Service for creating PDFs.
 */
@Service
public class PdfCreator {
    private static final Logger logger = LoggerFactory.getLogger(PdfCreator.class);
    private final DuplexPreparator duplexPreparator;
    private final IHtmlToPdfConverter converter;
    private final PDFServiceClient pdfServiceClient;


    /**
     * Constructor for the PdfCreator.
     * @param duplexPreparator The duplex preparator
     * @param converter The HTML to PDF converter
     */
    public PdfCreator(DuplexPreparator duplexPreparator, IHtmlToPdfConverter converter, PDFServiceClient pdfServiceClient) {
        this.duplexPreparator = duplexPreparator;
        this.converter = converter;
        this.pdfServiceClient = pdfServiceClient;
    }

    /**
     * Create a PDF from a list of documents.
     * @param documents The documents
     * @param loggingContext The logging context
     * @return The PDF
     */
    public byte[] createFromTemplates(List<Document> documents, String loggingContext) {
        Asserts.notNull(documents, "documents");

        List<byte[]> docs =
            documents
                .stream()
                .map(this::generatePdfWithClient)
                .map(pdf -> duplexPreparator.prepare(pdf, loggingContext))
                .collect(toList());

        return PdfMerger.mergeDocuments(docs, loggingContext);
    }

    /**
     * Create a PDF from a list of base64 decoded PDFs.
     * @param base64decodedDocs The base64 decoded PDFs
     * @param loggingContext The logging context
     * @return The PDF
     */
    public byte[] createFromBase64Pdfs(List<byte[]> base64decodedDocs, String loggingContext) {
        Asserts.notNull(base64decodedDocs, "base64decodedDocs");

        List<byte[]> docs = base64decodedDocs
            .stream()
            .map(pdf -> duplexPreparator.prepare(pdf, loggingContext))
            .collect(toList());

        return PdfMerger.mergeDocuments(docs, loggingContext);
    }

    /**
     * Create a PDF from a list of base64 decoded PDFs with copies.
     * @param docs The documents
     * @param loggingContext The logging context
     * @return The PDF
     */
    public byte[] createFromBase64PdfWithCopies(List<Doc> docs, String loggingContext) {
        Asserts.notNull(docs, "base64decodedDocs");

        List<byte[]> pdfs = docs
            .stream()
            .peek(doc -> logger.info("Number of copies request {}", doc.copies))
            .map(doc -> new Doc(duplexPreparator.prepare(doc.content, loggingContext), doc.copies))
            .map(d -> Collections.nCopies(d.copies, d.content))
            .flatMap(Collection::stream)
            .collect(toList());

        return PdfMerger.mergeDocuments(pdfs, loggingContext);
    }

    /**
     * Create a PDF from a document.
     * @param document The document
     * @return The PDF
     */
    private byte[] generatePdf(Document document) {
        synchronized (PdfCreator.class) {
            return converter.apply(document.template.getBytes(), document.values);
        }
    }

    /**
     * Create a PDF from a document using cmc PDF service client.
     * @param document The document
     * @return The PDF
     */
    private byte[] generatePdfWithClient(Document document) {
        System.out.println("NEW PDF CONVERTER!");
        synchronized (PdfCreator.class) {
            return pdfServiceClient.generateFromHtml(document.template.getBytes(), document.values);
        }
    }
}
