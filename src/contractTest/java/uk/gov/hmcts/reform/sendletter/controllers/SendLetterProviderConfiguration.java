package uk.gov.hmcts.reform.sendletter.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;
import uk.gov.hmcts.reform.authorisation.validators.AuthTokenValidator;
import uk.gov.hmcts.reform.cmc.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.sendletter.config.PdfConversionConfig;
import uk.gov.hmcts.reform.sendletter.entity.DocumentRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;
import uk.gov.hmcts.reform.sendletter.services.AuthService;
import uk.gov.hmcts.reform.sendletter.services.DocumentService;
import uk.gov.hmcts.reform.sendletter.services.DuplicateLetterService;
import uk.gov.hmcts.reform.sendletter.services.ExceptionLetterService;
import uk.gov.hmcts.reform.sendletter.services.ExecusionService;
import uk.gov.hmcts.reform.sendletter.services.LetterChecksumService;
import uk.gov.hmcts.reform.sendletter.services.LetterService;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@TestConfiguration
@ComponentScan(basePackageClasses = PdfCreator.class)
@Import(PdfConversionConfig.class)
public class SendLetterProviderConfiguration {

    @MockBean
    private AuthService authService;

    @MockBean
    private LetterRepository letterRepository;

    @MockBean
    private LetterEventRepository letterEventRepository;

    @MockBean
    private ServiceFolderMapping serviceFolderMapping;

    @MockBean
    private DuplicateLetterService duplicateLetterService;

    @MockBean
    private ExceptionLetterService exceptionLetterService;

    @MockBean
    private DocumentRepository documentRepository;

    @Bean
    @Primary
    PdfCreator pdfCreator() {
        return new PdfCreator(new DuplexPreparator(), new HTMLToPDFConverter()::convert);
    }

    private final LaunchDarklyClient launchDarklyClient = mock(LaunchDarklyClient.class);
    private final LetterChecksumService letterChecksumService = new LetterChecksumService(launchDarklyClient);

    @Bean
    @Primary
    DocumentService documentService() {
        return new DocumentService(documentRepository, 1, letterChecksumService);
    }

    @Bean
    @Primary
    LetterService letterService() {
        when(launchDarklyClient.isFeatureEnabled("FACT-1388")).thenReturn(true);
        return new LetterService(
            pdfCreator(),
            letterRepository,
            letterEventRepository,
            documentService(),
            new Zipper(),
            new ObjectMapper(),
            false,
            "",
            serviceFolderMapping,
            new ExecusionService(),
            duplicateLetterService,
            exceptionLetterService,
            letterChecksumService
        );
    }

    @MockBean
    AuthTokenValidator authTokenValidator;

    @Bean
    @Primary
    SendLetterController sendLetterController() {
        return new SendLetterController(letterService(), authService);

    }
}
