package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

@ExtendWith(MockitoExtension.class)
class PdfCreatorTest {

    @Mock private DuplexPreparator duplexPreparator;

    private PdfCreator pdfCreator;

    @BeforeEach
    void setUp() {
        pdfCreator = new PdfCreator(this.duplexPreparator);
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

        try (PDDocument doc = Loader.loadPDF(result)) {
            assertThat(doc.getNumberOfPages()).isEqualTo(doc1.copies + doc2.copies);
        }
    }
}
