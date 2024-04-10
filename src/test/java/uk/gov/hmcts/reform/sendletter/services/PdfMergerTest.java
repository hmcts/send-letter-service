package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.Loader;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.exception.PdfMergeException;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfMerger;

import java.io.IOException;
import java.io.InputStream;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

class PdfMergerTest {

    @Test
    void should_return_a_merged_pdf_when_multiple_documents_are_sent() throws Exception {
        //given
        byte[] test1Pdf = loadResource("test1.pdf");
        byte[] test2Pdf = loadResource("test2.pdf");
        byte[] expectedMergedPdf = loadResource("merged.pdf");

        //when
        byte[] actualMergedPdf = PdfMerger.mergeDocuments(asList(test1Pdf, test2Pdf), "test_service");

        // then
        try (
            InputStream actualPdfPage1 = getPdfPageContents(actualMergedPdf, 0);
            InputStream actualPdfPage2 = getPdfPageContents(actualMergedPdf, 1);

            InputStream expectedPdfPage1 = getPdfPageContents(expectedMergedPdf, 0);
            InputStream expectedPdfPage2 = getPdfPageContents(expectedMergedPdf, 1)
        ) {
            assertThat(actualPdfPage1).hasSameContentAs(expectedPdfPage1);
            assertThat(actualPdfPage2).hasSameContentAs(expectedPdfPage2);
        }
    }

    @Test
    void should_return_a_merged_pdf_same_as_original_pdf_when_single_pdf_is_sent() throws Exception {
        //given
        byte[] testPdf = loadResource("test1.pdf");

        //when
        byte[] actualMergedPdf = PdfMerger.mergeDocuments(singletonList(testPdf), "test_service");

        // then
        assertThat(actualMergedPdf).containsExactly(testPdf);
    }

    @Test
    void should_throw_pdf_merge_exception_when_doc_is_not_pdf_stream() {
        assertThatThrownBy(PdfMergerTest::merge)
            .isInstanceOf(PdfMergeException.class);
    }

    private InputStream getPdfPageContents(byte[] pdf, int pageNumber) throws IOException {
        return Loader.loadPDF(pdf).getPage(pageNumber).getContents();
    }

    private static void merge() {
        PdfMerger.mergeDocuments(asList("test1".getBytes(), "test2".getBytes()), "test_service");
    }
}
