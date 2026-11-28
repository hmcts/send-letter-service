package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.pdfwriter.compress.CompressParameters;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.springframework.stereotype.Service;
import org.springframework.util.DigestUtils;
import uk.gov.hmcts.reform.sendletter.exception.ChecksumGenerationException;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

import static org.springframework.util.SerializationUtils.serialize;

/**
 * Service for letter checksum.
 */
@Service
public class LetterChecksumService {

    /**
     * Creates a document for a PDF based upon the contents of each page.
     * This is done because the entire PDF will have metadata against it. This means
     * the checksum will be different if the same document is created at different times.
     *
     * @param pdfBytes the bytes for the pdf document
     * @return a checksum of the collective pages for the pdf
     */
    public String calculateContentChecksum(byte[] pdfBytes) {
        try (PDDocument document = Loader.loadPDF(pdfBytes)) {
            MessageDigest md = MessageDigest.getInstance("MD5");

            try (var sink = new DigestOutputStream(OutputStream.nullOutputStream(), md)) {
                for (PDPage page : document.getPages()) {
                    if (page.getResources() != null && page.getResources().getXObjectNames().iterator().hasNext()) {
                        // A draw instruction names a resource. Two different photos can have the same name.
                        md.reset();
                        hashPageTree(document, sink);
                        break;
                    }
                    // Preserve stored keys for documents without external images or forms.
                    try (InputStream contents = page.getContents()) {
                        contents.transferTo(sink);
                    }
                }
            }
            return HexFormat.of().formatHex(md.digest());
        } catch (IOException | NoSuchAlgorithmException e) {
            // Only case above that throws this is when the MD5 for documents are being checked
            throw new ChecksumGenerationException(e.getMessage());
        }
    }

    private static void hashPageTree(PDDocument source, OutputStream sink) throws IOException {
        // Let PDFBox follow the page references, including nested forms, image masks and shared resources.
        // A fresh catalog excludes the submission's document metadata and trailer ID.
        try (PDDocument pages = new PDDocument()) {
            pages.getDocumentCatalog().getCOSObject().setItem(COSName.PAGES, source.getPages());
            pages.setDocumentId(0L);
            pages.save(sink, CompressParameters.NO_COMPRESSION);
        }
    }

    /**
     * Generates a checksum for a given object.
     *
     * @param obj the object to generate a checksum for
     * @return the checksum
     */
    public String generateChecksumForPdfPages(Object obj) {
        return (obj instanceof Doc doc)
            ? calculateContentChecksum(doc.content)
            : generateChecksum(obj);
    }

    /**
     * Generates a checksum for a given object.
     *
     * @param obj the object to generate a checksum for
     * @return the checksum
     */
    public String generateChecksum(Object obj) {
        return DigestUtils.md5DigestAsHex(serialize(obj));
    }
}
