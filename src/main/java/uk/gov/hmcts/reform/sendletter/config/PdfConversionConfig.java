package uk.gov.hmcts.reform.sendletter.config;

import static java.net.URI.create;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
import uk.gov.hmcts.reform.cmc.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.pdf.service.client.PDFServiceClient;
import uk.gov.hmcts.reform.sendletter.services.pdf.IHtmlToPdfConverter;

@Configuration
public class PdfConversionConfig {

    @Value("http://rpe-pdf-service-aat.service.core-compute-aat.internal")
    private String pdfApiUrl;

    @Bean
    public PDFServiceClient pdfServiceClient(
        RestTemplate restTemplate,
        ObjectMapper objectMapper
    ) {
        return PDFServiceClient.builder()
            .restOperations(restTemplate)
            .objectMapper(objectMapper)
            .build(create(pdfApiUrl));
    }

    /**
     * Create a bean of IHtmlToPdfConverter.
     * @return The IHtmlToPdfConverter
     */
    @Bean
    public IHtmlToPdfConverter htmlToPdfConverter() {
//        return new HTMLToPDFConverter()::convert;
          return new HTMLToPDFConverter()::convert;
    }
}
