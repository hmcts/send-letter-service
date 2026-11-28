package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.collect.ImmutableMap;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDFormContentStream;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.PDResources;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.graphics.form.PDFormXObject;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.function.Supplier;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

class LetterChecksumServiceTest {

    private final LetterChecksumService letterChecksumService = new LetterChecksumService();

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void distinguishes_different_photos_but_matches_a_regenerated_copy(boolean inForm) throws IOException {
        byte[] red = photoPdf(Color.RED, "first submission", inForm);
        byte[] blue = photoPdf(Color.BLUE, "first submission", inForm);
        byte[] retry = photoPdf(Color.RED, "regenerated copy", inForm);

        assertThat(retry).isNotEqualTo(red);
        assertThat(letterChecksumService.calculateContentChecksum(red))
            .isNotEqualTo(letterChecksumService.calculateContentChecksum(blue))
            .isEqualTo(letterChecksumService.calculateContentChecksum(retry));
    }

    @Test
    void retains_the_stored_checksum_for_a_pdf_without_xobjects() throws IOException {
        try (PDDocument pdf = new PDDocument()) {
            pdf.addPage(new PDPage());
            var bytes = new ByteArrayOutputStream();
            pdf.save(bytes);
            // The legacy key for an empty page is MD5 of its empty content stream.
            assertThat(letterChecksumService.calculateContentChecksum(bytes.toByteArray()))
                .isEqualTo("d41d8cd98f00b204e9800998ecf8427e");
        }
    }

    private static byte[] photoPdf(Color colour, String title, boolean inForm) throws IOException {
        try (PDDocument pdf = new PDDocument()) {
            pdf.getDocumentInformation().setTitle(title);
            PDPage page = new PDPage();
            pdf.addPage(page);
            var pixels = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
            pixels.setRGB(0, 0, colour.getRGB());
            var photo = LosslessFactory.createFromImage(pdf, pixels);
            try (var contents = new PDPageContentStream(pdf, page)) {
                if (inForm) {
                    var form = new PDFormXObject(pdf);
                    form.setBBox(PDRectangle.A4);
                    form.setResources(new PDResources());
                    try (var nested = new PDFormContentStream(form)) {
                        nested.drawImage(photo, 20, 20, 100, 100);
                    }
                    contents.drawForm(form);
                } else {
                    contents.drawImage(photo, 20, 20, 100, 100);
                }
            }
            var bytes = new ByteArrayOutputStream();
            pdf.save(bytes);
            return bytes.toByteArray();
        }
    }

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_document_objects_version_3() throws Exception {

        final LetterWithPdfsAndNumberOfCopiesRequest letterWithPdfsAndNumberOfCopiesRequest =
            SampleData.letterWithPdfAndCopiesRequest(5, 10);

        Doc docOne = letterWithPdfsAndNumberOfCopiesRequest.documents.get(0);
        Doc docTwo = letterWithPdfsAndNumberOfCopiesRequest.documents.get(1);

        // Simply send in the same documents through the check twice and assert they match
        // Check that the parent method calls the calculateContentChecksum as well
        assertThat(letterChecksumService.generateChecksumForPdfPages(docOne))
            .isEqualTo(letterChecksumService.calculateContentChecksum(docOne.content));
        assertThat(letterChecksumService.generateChecksumForPdfPages(docTwo))
            .isEqualTo(letterChecksumService.calculateContentChecksum(docTwo.content));
    }

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_objects() {

        Supplier<LetterRequest> letterSupplier =
            () -> new LetterRequest(
                singletonList(
                    new Document(
                        "cmc-template",
                        ImmutableMap.of(
                            "key11", "value11",
                            "key21", "value21"
                        )
                    )
                ),
                "print-job-1234",
                ImmutableMap.of(
                    "doc_type", "my doc type",
                    "caseId", "123"
                )
            );

        LetterRequest letter1 = letterSupplier.get();
        LetterRequest letter2 = letterSupplier.get();

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_with_pdfs_objects() {

        Supplier<LetterWithPdfsRequest> letterSupplier =
            () -> new LetterWithPdfsRequest(
                asList(
                    "foo".getBytes(),
                    "bar".getBytes()
                ),
                "print-job-1234",
                ImmutableMap.of(
                    "doc_type", "my doc type",
                    "caseId", "123"
                )
            );

        LetterWithPdfsRequest letter1 = letterSupplier.get();
        LetterWithPdfsRequest letter2 = letterSupplier.get();

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_different_md5_checksum_hex_for_different_letter_objects() {

        LetterRequest letter1 = new LetterRequest(
            singletonList(new Document(
                "cmc-template",
                ImmutableMap.of(
                    "key11", "value11",
                    "key12", "value12"
                )
            )),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        LetterRequest letter2 = new LetterRequest(
            singletonList(new Document(
                "cmc-template",
                ImmutableMap.of(
                    "key21", "key21",
                    "key22", "value22"
                )
            )),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isNotEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_different_md5_checksum_hex_for_different_letter_with_pdf_objects() {

        LetterWithPdfsRequest letter1 = new LetterWithPdfsRequest(
            asList(
                "foo".getBytes(),
                "bar".getBytes()
            ),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        LetterWithPdfsRequest letter2 = new LetterWithPdfsRequest(
            asList(
                "foo".getBytes(),
                "bar!".getBytes()
            ),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isNotEqualTo(letterChecksumService.generateChecksum(letter2));
    }
}
