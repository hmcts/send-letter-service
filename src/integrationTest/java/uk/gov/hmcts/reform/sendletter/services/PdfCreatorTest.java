package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.exception.DuplexException;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

class PdfCreatorTest {

    private PdfCreator pdfCreator;

    @BeforeEach
    void setUp() {
        this.pdfCreator = new PdfCreator(new DuplexPreparator());
    }

    @Test
    void should_handle_base64_encoded_pdfs() throws Exception {
        // given

        List<byte[]> pdfs = asList(
            loadResource("pdfs/test1.pdf"),
            loadResource("pdfs/test2.pdf")
        );

        // when
        byte[] pdfContent = pdfCreator.createFromBase64Pdfs(pdfs, "logging");

        // then
        assertThat(pdfContent).isNotNull();
        // and no exception is thrown
    }

    @Test
    void should_handle_base64_encoded_pdfs_with_number_of_copies() throws Exception {
        // given
        List<Doc> docs = asList(
            new Doc(loadResource("pdfs/test1.pdf"), 1),
            new Doc(loadResource("pdfs/test2.pdf"), 10)
        );

        // when
        byte[] pdfContent = pdfCreator.createFromBase64PdfWithCopies(docs, "logging");

        // then
        assertThat(pdfContent).isNotNull();
        // and no exception is thrown
    }

    @Test
    void should_throw_an_exception_if_bytes_do_not_represent_pdf() {
        // given
        List<byte[]> pdfs = Collections.singletonList(
            "clearly not a pdf".getBytes()
        );

        // when
        var exc = catchThrowable(() -> pdfCreator.createFromBase64Pdfs(pdfs, "logging"));

        // then
        assertThat(exc)
            .isNotNull()
            .isInstanceOf(DuplexException.class);
    }
}
