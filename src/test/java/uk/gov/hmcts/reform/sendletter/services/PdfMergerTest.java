package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.cos.COSArray;
import org.apache.pdfbox.cos.COSInteger;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.common.PDNumberTreeNode;
import org.apache.pdfbox.pdmodel.documentinterchange.logicalstructure.PDStructureTreeRoot;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.exception.PdfMergeException;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfMerger;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

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
    void should_still_create_a_merged_pdf_when_source_pdfs_contain_unsupported_structures() throws Exception {
        //given
        byte[] test1Pdf = createStructTreePdf();
        byte[] test2Pdf = loadResource("test2.pdf");

        //when
        byte[] actualMergedPdf = PdfMerger.mergeDocuments(asList(test1Pdf, test2Pdf), "test_service");

        // then
        try (
            // the struct tree PDF doesn't have a page we'll just check that page 0 is
            // the same as page 0 of the second PDF
            InputStream actualPdfPage1 = getPdfPageContents(actualMergedPdf, 0);
            InputStream expectedPdfPage1 = getPdfPageContents(test2Pdf, 0)
        ) {
            assertThat(actualPdfPage1).hasSameContentAs(expectedPdfPage1);
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

    // FACT-2683 - creates a PDF with a struct tree containing a COSArray/COSInteger to test
    // that the merge code can handle this without throwing an exception
    private byte[] createStructTreePdf() {
        try (PDDocument document = new PDDocument();
             ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            PDNumberTreeNode pdNumberTreeNode = new PDNumberTreeNode(COSArray.class);
            pdNumberTreeNode.setNumbers(Map.of(1, COSInteger.get(100)));
            PDStructureTreeRoot structureTreeRoot = new PDStructureTreeRoot();
            structureTreeRoot.setParentTree(pdNumberTreeNode);
            document.getDocumentCatalog().setStructureTreeRoot(structureTreeRoot);
            document.save(outputStream);
            return outputStream.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
