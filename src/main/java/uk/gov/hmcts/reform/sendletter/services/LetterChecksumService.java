package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.springframework.stereotype.Service;
import org.springframework.util.DigestUtils;
import uk.gov.hmcts.reform.sendletter.exception.ChecksumGenerationException;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import static org.springframework.util.SerializationUtils.serialize;

/**
 * Service for letter checksum.
 */
@Service
public class LetterChecksumService {

    private final LaunchDarklyClient launchDarklyClient;

    /**
     * Constructor for the LetterChecksumService.
     * @param launchDarklyClient The client for launch darkly
     */
    public LetterChecksumService(LaunchDarklyClient launchDarklyClient) {
        this.launchDarklyClient = launchDarklyClient;
    }

    /**
     * Creates a document for a PDF based upon the contents of each page.
     * This is done because the entire PDF will have metadata against it. This means
     * the checksum will be different if the same document is created at different times.
     *
     * @param pdfBytes the bytes for the pdf document
     * @return a checksum of the collective pages for the pdf
     */
    public String calculateContentChecksum(byte[] pdfBytes) {
        try (PDDocument document = PDDocument.load(pdfBytes)) {
            MessageDigest md = MessageDigest.getInstance("MD5");

            for (PDPage page : document.getPages()) {
                // Extract and update content stream
                InputStream contentStream = page.getContents();
                byte[] contentBytes = contentStream.readAllBytes();
                md.update(contentBytes);
            }

            byte[] digest = md.digest();
            StringBuilder sb = new StringBuilder();
            for (byte b : digest) {
                sb.append(String.format("%02x", b & 0xff));
            }
            return sb.toString();
        } catch (IOException | NoSuchAlgorithmException e) {
            // Only case above that throws this is when the MD5 for documents are being checked
            throw new ChecksumGenerationException(e.getMessage());
        }
    }

    /**
     * Generates a checksum for a given object.
     *
     * @param obj the object to generate a checksum for
     * @return the checksum
     */
    public String generateChecksumForPdfPages(Object obj) {
        return launchDarklyClient.isFeatureEnabled("FACT-1388")
            ? obj instanceof Doc
                ? calculateContentChecksum(((Doc) obj).content)
                // If we have an old request for the old Document type: at present this is unavailable.
                // This is unlikely to be hit as the client being used targets the latest version of the endpoint
                : generateChecksum(obj)
            // if toggle disabled, go to default check
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
