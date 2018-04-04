package uk.gov.hmcts.reform.slc.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.gov.hmcts.reform.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.IHtmlToPdfConverter;

@Configuration
public class PdfConversionConfig {

    @Bean
    public IHtmlToPdfConverter htmlToPdfConverter() {
        HTMLToPDFConverter converter = new HTMLToPDFConverter();
        return converter::convert;
    }
}
