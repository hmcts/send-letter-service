package uk.gov.hmcts.reform.sendletter.services;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.exception.DuplexException;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

class DuplexPreparatorTest {

    @Test
    void should_add_blank_page_if_total_number_of_pages_is_odd() throws Exception {
        // given
        byte[] before = loadResource("single_page.pdf");

        // when
        byte[] after = new DuplexPreparator().prepare(before, "test_service");

        // then
        assertThat(after).isNotEqualTo(before);
        try (PDDocument pdDoc = PDDocument.load(after)) {
            assertThat(pdDoc.getNumberOfPages()).as("number of pages").isEqualTo(2);
        }
    }

    @Test
    void should_not_add_a_blank_page_if_total_number_of_pages_is_even() throws Exception {
        // given
        byte[] before = loadResource("two_pages.pdf");

        // when
        byte[] after = new DuplexPreparator().prepare(before, "test_service");

        // then
        assertThat(after).isEqualTo(before);
    }

    @Test
    void should_throw_duplex_exception_when_stream_is_not_pdf() {
        assertThatThrownBy(DuplexPreparatorTest::prepare)
            .isInstanceOf(DuplexException.class)
            .hasMessage("Failed to parse the documents for test_service logs");
    }

    private static void prepare() {
        new DuplexPreparator().prepare("corruptedStream".getBytes(), "test_service logs");
    }
}
