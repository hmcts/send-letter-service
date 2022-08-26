package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.io.ByteStreams;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.IHtmlToPdfConverter;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

@ExtendWith(MockitoExtension.class)
class PdfCreatorTest {

    @Mock private DuplexPreparator duplexPreparator;
    @Mock private IHtmlToPdfConverter converter;

    private PdfCreator pdfCreator;

    @BeforeEach
    void setUp() {
        pdfCreator = new PdfCreator(this.duplexPreparator, this.converter);
    }

    @Test
    void should_require_documents_to_not_be_null() {
        assertThatThrownBy(() -> pdfCreator.createFromTemplates(null, "test"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("documents");
    }

    @Test
    void should_handle_documents_with_number_of_copies_specified() throws Exception {
        // given
        final String loggingContext = "test_service";
        final byte[] test1Pdf = loadResource("test1.pdf");
        final byte[] test2Pdf = loadResource("test2.pdf");

        given(duplexPreparator.prepare(test1Pdf, loggingContext)).willReturn(test1Pdf);
        given(duplexPreparator.prepare(test2Pdf, loggingContext)).willReturn(test2Pdf);

        Doc doc1 = new Doc(test1Pdf, 5);
        Doc doc2 = new Doc(test2Pdf, 10);

        // when
        byte[] result = pdfCreator.createFromBase64PdfWithCopies(asList(doc1, doc2), loggingContext);

        // then
        verify(duplexPreparator, times(1)).prepare(doc1.content, loggingContext);
        verify(duplexPreparator, times(1)).prepare(doc2.content, loggingContext);

        try (PDDocument doc = PDDocument.load(result)) {
            assertThat(doc.getNumberOfPages()).isEqualTo(doc1.copies + doc2.copies);
        }
    }

    @Test
    void should_return_a_merged_pdf_when_multiple_documents_are_passed() throws Exception {
        final byte[] test1Pdf = loadResource("test1.pdf");
        final byte[] test2Pdf = loadResource("test2.pdf");
        final byte[] expectedMergedPdf = loadResource("merged.pdf");
        final String loggingContext = "test_service logging";

        given(duplexPreparator.prepare(test1Pdf, loggingContext)).willReturn(test1Pdf);
        given(duplexPreparator.prepare(test2Pdf, loggingContext)).willReturn(test2Pdf);

        given(converter.apply(eq("t1".getBytes()), any())).willReturn(test1Pdf);
        given(converter.apply(eq("t2".getBytes()), any())).willReturn(test2Pdf);

        List<Document> docs = asList(
            new Document("t1", emptyMap()),
            new Document("t2", emptyMap())
        );

        // when
        byte[] pdfContent = pdfCreator.createFromTemplates(docs, loggingContext);

        // then
        try (
            InputStream actualPdfPage1 = getPdfPageContents(pdfContent, 0);
            InputStream actualPdfPage2 = getPdfPageContents(pdfContent, 1);

            InputStream expectedPdfPage1 = getPdfPageContents(expectedMergedPdf, 0);
            InputStream expectedPdfPage2 = getPdfPageContents(expectedMergedPdf, 1)
        ) {
            assertThat(actualPdfPage1).hasSameContentAs(expectedPdfPage1);
            assertThat(actualPdfPage2).hasSameContentAs(expectedPdfPage2);
        }

        // and
        verify(duplexPreparator, times(2)).prepare(any(byte[].class), eq(loggingContext));
    }

    private InputStream getPdfPageContents(byte[] pdf, int pageNumber) throws Exception {
        try (PDDocument doc = PDDocument.load(pdf)) {
            byte[] data = ByteStreams.toByteArray(doc.getPage(pageNumber).getContents());
            return new ByteArrayInputStream(data);
        }
    }

}
