package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.BDDMockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.sendletter.PdfHelper;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;

import java.io.IOException;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Uploaded;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class LetterServiceTest {

    private static final String SERVICE_NAME = "bulkprint";

    private LetterService service;
    private ObjectMapper objectMapper;
    private AsyncService asyncService;

    @Autowired
    private LetterRepository letterRepository;

    @BeforeEach
    void setUp() {
        asyncService = spy(AsyncService.class);
        ServiceFolderMapping serviceFolderMapping = mock(ServiceFolderMapping.class);
        BDDMockito.given(serviceFolderMapping.getFolderFor(any())).willReturn(Optional.of("some_folder_name"));
        objectMapper = new ObjectMapper();
        service = new LetterService(
            new PdfCreator(new DuplexPreparator(), new HTMLToPDFConverter()::convert),
            letterRepository,
            new Zipper(),
            new ObjectMapper(),
            false,
            null,
            serviceFolderMapping,
            asyncService
        );
    }

    @AfterEach
    void tearDown() {
        letterRepository.deleteAll();
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void generates_and_saves_zipped_pdf(String async) throws IOException {
        UUID id = service.save(SampleData.letterRequest(), SERVICE_NAME, async);

        Letter result = letterRepository.findById(id).get();

        assertThat(result.isEncrypted()).isFalse();
        assertThat(result.getEncryptionKeyFingerprint()).isNull();
        PdfHelper.validateZippedPdf(result.getFileContent());
        if (Boolean.parseBoolean(async)) {
            verify(asyncService).run(any());
        }
    }


    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void generatesLetterWithAddionalDataAndSaveZippedPdf(String async) throws IOException {
        UUID id = service.save(SampleData.letterWithPdfsRequestWithAdditionalData(), SERVICE_NAME, async);

        Letter result = letterRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Letter not found " + id.toString()));
        assertThat(result.isEncrypted()).isFalse();
        assertThat(result.getEncryptionKeyFingerprint()).isNull();
        PdfHelper.validateZippedPdf(result.getFileContent());

        String expectedAdditionData = "{\"reference\":\"ABD-123-WAZ\",\"count\":10,\"additionInfo\":\"present\"}";
        JsonNode expectedAdditionalData = objectMapper.readTree(expectedAdditionData);
        assertThat(result.getAdditionalData()).isEqualTo(expectedAdditionalData);

        if (Boolean.parseBoolean(async)) {
            verify(asyncService).run(any());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void generatesLetterWithNoAddionalDataAndSaveZippedPdf(String async) throws IOException {
        UUID id = service.save(SampleData.letterWithPdfsRequestWithNoAdditionalData(), SERVICE_NAME, async);

        Letter result = letterRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Letter not found " + id.toString()));
        assertThat(result.getAdditionalData()).isEmpty();
        assertThat(result.isEncrypted()).isFalse();
        assertThat(result.getEncryptionKeyFingerprint()).isNull();
        PdfHelper.validateZippedPdf(result.getFileContent());

        if (Boolean.parseBoolean(async)) {
            verify(asyncService).run(any());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"false","true"})
    void returns_same_id_on_resubmit(String async) {
        // given
        LetterRequest sampleRequest = SampleData.letterRequest();
        UUID id1 = service.save(sampleRequest, SERVICE_NAME, async);
        Letter letter = letterRepository.findById(id1).get();

        // and
        assertThat(letter.getStatus()).isEqualByComparingTo(Created);

        // when
        UUID id2 = service.save(sampleRequest, SERVICE_NAME, async);

        // then
        assertThat(id1).isEqualByComparingTo(id2);

        if (Boolean.parseBoolean(async)) {
            verify(asyncService).run(any());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void saves_an_new_letter_if_previous_one_has_been_sent_to_print(String async) {
        // given
        LetterRequest sampleRequest = SampleData.letterRequest();
        UUID id1 = service.save(sampleRequest, SERVICE_NAME, async);
        Letter letter = letterRepository.findById(id1).get();

        // and
        assertThat(letter.getStatus()).isEqualByComparingTo(Created);

        // when
        letter.setStatus(Uploaded);
        letterRepository.saveAndFlush(letter);
        UUID id2 = service.save(sampleRequest, SERVICE_NAME, async);

        // then
        assertThat(id1).isNotEqualByComparingTo(id2);

        if (Boolean.parseBoolean(async)) {
            verify(asyncService, times(2)).run(any());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void should_not_allow_null_service_name(String async) {
        assertThatThrownBy(() -> service.save(SampleData.letterRequest(), null, async))
            .isInstanceOf(IllegalStateException.class);
    }

    @ParameterizedTest
    @ValueSource(strings = {"false", "true"})
    void should_not_allow_empty_service_name(String async) {
        assertThatThrownBy(() -> service.save(SampleData.letterRequest(), "", async))
            .isInstanceOf(IllegalStateException.class);
    }

    @Test
    void handles_null_timestamps() {
        assertThat(LetterService.toDateTime(null)).isNull();
    }
}
