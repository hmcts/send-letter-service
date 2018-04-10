package uk.gov.hmcts.reform.sendletter.services;

import org.apache.http.util.Asserts;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.exception.InvalidPdfException;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.services.util.DuplexPreparator;

import java.util.Base64;
import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class PdfCreator {

    private final DuplexPreparator duplexPreparator;
    private final PdfConverter converter;

    public PdfCreator(DuplexPreparator duplexPreparator, PdfConverter converter) {
        this.duplexPreparator = duplexPreparator;
        this.converter = converter;
    }

    public byte[] createFromTemplates(List<Document> documents) {
        Asserts.notNull(documents, "documents");

        List<byte[]> docs =
            documents
                .stream()
                .map(converter::generatePdf)
                .map(duplexPreparator::prepare)
                .collect(toList());

        return PdfMerger.mergeDocuments(docs);
    }

    public byte[] createFromBase64Pdfs(List<String> base64encodedDocs) {
        Asserts.notNull(base64encodedDocs, "base64encodedDocs");

        List<byte[]> docs =
            base64encodedDocs
                .stream()
                .map(this::decodePdf)
                .map(duplexPreparator::prepare)
                .collect(toList());

        return PdfMerger.mergeDocuments(docs);
    }

    private byte[] decodePdf(String base64encodedPdf) {
        try {
            return Base64.getDecoder().decode(base64encodedPdf);
        } catch (IllegalArgumentException exc) {
            throw new InvalidPdfException(exc);
        }
    }
}
