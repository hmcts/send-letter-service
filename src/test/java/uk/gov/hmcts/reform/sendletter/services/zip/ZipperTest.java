package uk.gov.hmcts.reform.sendletter.services.zip;

import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.model.PdfDoc;

import java.io.ByteArrayInputStream;
import java.util.zip.ZipInputStream;

import static org.assertj.core.api.Assertions.assertThat;
import static uk.gov.hmcts.reform.sendletter.util.ResourceLoader.loadResource;

class ZipperTest {

    @Test
    void should_zip_file() throws Exception {
        byte[] fileContent = loadResource("hello.pdf");
        byte[] expectedZipFileContent = loadResource("hello.zip");

        byte[] result = new Zipper().zip(
            new PdfDoc("hello.pdf", fileContent)
        );

        assertThat(result).isNotNull();
        assertThat(asZip(result)).hasSameContentAs(asZip(expectedZipFileContent));
    }

    private ZipInputStream asZip(byte[] bytes) {
        return new ZipInputStream(new ByteArrayInputStream(bytes));
    }
}
