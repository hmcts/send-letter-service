package uk.gov.hmcts.reform.sendletter.services.pdf;

import org.apache.hc.core5.util.Asserts;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class PdfCreator {
    private static final Logger logger = LoggerFactory.getLogger(PdfCreator.class);
    private final DuplexPreparator duplexPreparator;
    private final IHtmlToPdfConverter converter;

    public PdfCreator(DuplexPreparator duplexPreparator, IHtmlToPdfConverter converter) {
        this.duplexPreparator = duplexPreparator;
        this.converter = converter;
    }

    public byte[] createFromTemplates(List<Document> documents, String loggingContext) {
        Asserts.notNull(documents, "documents");

        List<byte[]> docs =
            documents
                .stream()
                .map(this::generatePdf)
                .map(pdf -> duplexPreparator.prepare(pdf, loggingContext))
                .collect(toList());

        return PdfMerger.mergeDocuments(docs, loggingContext);
    }

    public byte[] createFromBase64Pdfs(List<byte[]> base64decodedDocs, String loggingContext) {
        Asserts.notNull(base64decodedDocs, "base64decodedDocs");

        List<byte[]> docs = base64decodedDocs
            .stream()
            .map(pdf -> duplexPreparator.prepare(pdf, loggingContext))
            .collect(toList());

        return PdfMerger.mergeDocuments(docs, loggingContext);
    }

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

    private byte[] generatePdf(Document document) {
        synchronized (PdfCreator.class) {
            return converter.apply(document.template.getBytes(), document.values);
        }
    }
}
