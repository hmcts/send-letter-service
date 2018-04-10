package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.services.util.IHtmlToPdfConverter;

@Component
public class PdfConverter {

    private final IHtmlToPdfConverter converter;

    public PdfConverter(IHtmlToPdfConverter converter) {
        this.converter = converter;
    }

    public byte[] generatePdf(Document document) {
        synchronized (PdfConverter.class) {
            return converter.apply(document.template.getBytes(), document.values);
        }
    }
}
