package uk.gov.hmcts.reform.sendletter.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.Mockito;
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
import uk.gov.hmcts.reform.sendletter.model.in.RecipientsValidator;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@TestConfiguration
@ComponentScan(basePackageClasses = PdfCreator.class)
@Import({PdfConversionConfig.class})
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
    public LetterChecksumService letterChecksumService() {
        return new LetterChecksumService(launchDarklyClient());
    }

    @Bean
    @Primary
    public LaunchDarklyClient launchDarklyClient() {
        LaunchDarklyClient launchDarklyClientMock = Mockito.mock(LaunchDarklyClient.class);
        when(launchDarklyClientMock.isFeatureEnabled(anyString())).thenReturn(true);
        return launchDarklyClientMock;
    }

    @Bean
    @Primary
    public RecipientsValidator recipientsValidator() {
        // Mock the RecipientsValidator interface
        RecipientsValidator recipientsValidatorMock = Mockito.mock(RecipientsValidator.class);
        when(recipientsValidatorMock.isValid(any(), any())).thenReturn(true);
        return recipientsValidatorMock;
    }

    @Bean
    @Primary
    PdfCreator pdfCreator() {
        return new PdfCreator(new DuplexPreparator(), new HTMLToPDFConverter()::convert);
    }

    @Bean
    @Primary
    DocumentService documentService() {
        return new DocumentService(documentRepository, 1, letterChecksumService());
    }

    @Bean
    @Primary
    LetterService letterService() {
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
            letterChecksumService()
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
