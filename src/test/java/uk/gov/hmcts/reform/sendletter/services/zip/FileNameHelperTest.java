package uk.gov.hmcts.reform.sendletter.services.zip;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.exception.UnableToExtractIdFromFileNameException;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static java.time.LocalDateTime.now;
import static java.util.Arrays.asList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class FileNameHelperTest {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private static final String SSCS = "sscs";

    @Test
    void should_generate_file_name_in_expected_format() {
        // given
        UUID letterId = UUID.randomUUID();
        Letter letter = createLetter(letterId, "typeA", "cmc");

        // when
        String result = FileNameHelper.generatePdfName(letter);

        // then
        assertThat(result).isEqualTo("typeA_cmc_" + letterId + ".pdf");
    }

    @Test
    void should_generate_file_name_in_expected_format_when_letter_is_sscs_ib() {
        final Map<String, Object> additionalData = new HashMap<>();

        additionalData.put("isIbca", "true");

        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "sscs",
            objectMapper.valueToTree(additionalData),
            "SSCS001",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        String result = FileNameHelper.generatePdfName(letter);
        assertThat(result).isEqualTo("SSCS001_IB_sscs_" + letter.getId() + ".pdf");
    }

    @Test
    void should_generate_file_name_in_expected_format_when_letter_is_sscs_but_not_ib() {
        final Map<String, Object> additionalData = new HashMap<>();

        additionalData.put("isIbca", "false");

        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "sscs",
            objectMapper.valueToTree(additionalData),
            "SSCS001",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        String result = FileNameHelper.generatePdfName(letter);
        assertThat(result).isEqualTo("SSCS001_sscs_" + letter.getId() + ".pdf");
    }

    @Test
    void should_generate_name_when_there_is_no_additional_data() {
        final Map<String, Object> additionalData = new HashMap<>();

        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "prlcosapi",
            objectMapper.valueToTree(additionalData),
            "PRL001",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        String result = FileNameHelper.generatePdfName(letter);
        assertThat(result).isEqualTo("PRL001_prlcosapi_" + letter.getId() + ".pdf");
    }

    @Test
    void should_generate_name_without_ib_if_not_sscs() {
        final Map<String, Object> additionalData = new HashMap<>();
        additionalData.put("isIbca", "true");
        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "prlcosapi",
            objectMapper.valueToTree(additionalData),
            "PRL001",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        String result = FileNameHelper.generatePdfName(letter);
        assertThat(result).isEqualTo("PRL001_prlcosapi_" + letter.getId() + ".pdf");
    }

    @Test
    void should_always_generate_the_same_name_for_same_letter() {
        // given
        UUID letterId = UUID.randomUUID();
        Letter letter1 = createLetter(letterId, "A", "B");
        Letter letter2 = createLetter(letterId, "A", "B");

        String result1 = FileNameHelper.generatePdfName(letter1);
        String result2 = FileNameHelper.generatePdfName(letter2);

        // then
        assertThat(result1).isEqualTo(result2);
    }

    @Test
    void should_generate_different_names_for_different_letters() {
        // given
        Letter letter1 = createLetter(UUID.randomUUID(), "A", "B");
        Letter letter2 = createLetter(UUID.randomUUID(), "C", "D");

        String result1 = FileNameHelper.generatePdfName(letter1);
        String result2 = FileNameHelper.generatePdfName(letter2);

        // then
        assertThat(result1).isNotEqualTo(result2);
    }

    @Test
    void should_generate_different_names_for_same_letters_with_different_id() {
        // given
        Letter letter1 = createLetter(UUID.randomUUID(), "A", "B");
        Letter letter2 = createLetter(UUID.randomUUID(), "A", "B");

        String result1 = FileNameHelper.generatePdfName(letter1);
        String result2 = FileNameHelper.generatePdfName(letter2);

        // then
        assertThat(result1).isNotEqualTo(result2);
    }

    @Test
    void should_strip_out_underscores_from_service_name() {
        UUID letterId = UUID.randomUUID();
        Letter letter = createLetter(letterId, "typeA", "cmc_claim_store");

        String result = FileNameHelper.generatePdfName(letter);

        assertThat(result).isEqualTo("typeA_cmcclaimstore_" + letterId + ".pdf");
    }

    @Test
    void should_extract_letter_id_from_file_name() {
        asList(
            createLetter(UUID.randomUUID(), "type", "cmc"),
            createLetter(UUID.randomUUID(), "smoke_test", "cmc"),
            createLetter(UUID.randomUUID(), "type", "my_service_"),
            createLetter(UUID.randomUUID(), "some_type", "my_service")
        ).forEach(letter -> {
            String name = FileNameHelper.generatePdfName(letter);
            UUID extractedId = FileNameHelper.extractIdFromPdfName(name);

            assertThat(extractedId).isEqualTo(letter.getId());
        });
    }

    @Test
    void should_throw_custom_exception_when_id_cannot_be_extracted_from_file_name() {
        assertThatThrownBy(
            () -> FileNameHelper.extractIdFromPdfName("a_b.pdf")
        ).isInstanceOf(UnableToExtractIdFromFileNameException.class);
    }

    @Test
    void should_throw_custom_exception_when_uuid_invalid() {
        assertThatThrownBy(
            () -> FileNameHelper.extractIdFromPdfName("a_b_notauuid.pdf")
        ).isInstanceOf(UnableToExtractIdFromFileNameException.class);
    }

    @Test
    void should_generate_expected_file_name() {
        // given
        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        // when
        String name = FileNameHelper.generateName(letter);

        // then
        assertThat(name).isEqualTo(
            "type_cmc_"
                + letter.getCreatedAt().format(FileNameHelper.dateTimeFormatter)
                + "_"
                + letter.getId()
                + ".zip"
        );
    }

    @Test
    void should_generate_expected_file_name_with_explicit_parameters_as_input() {
        UUID letterId = randomUUID();
        LocalDateTime created = now();

        String name = FileNameHelper.generateName("type", "cmc", created, letterId, true, null);

        assertThat(name).isEqualTo(
            "type_cmc_"
                + created.format(FileNameHelper.dateTimeFormatter)
                + "_"
                + letterId
                + ".pgp"
        );
    }

    @Test
    void should_return_infected_blood_infix_when_type_is_sscs_and_isIbca_is_true_as_boolean() {
        Map<String, Object> additionalData = Map.of("isIbca", true);

        String result = FileNameHelper.infectedBloodInfix(SSCS, additionalData);

        assertThat(result).isEqualTo("_IB");
    }

    @Test
    void should_return_infected_blood_infix_when_type_is_sscs_and_isIbca_is_true_as_string() {
        Map<String, Object> additionalData = Map.of("isIbca", "true");

        String result = FileNameHelper.infectedBloodInfix(SSCS, additionalData);

        assertThat(result).isEqualTo("_IB");
    }

    @Test
    void should_return_empty_string_when_type_is_not_sscs() {
        String type = "notSscs";
        Map<String, Object> additionalData = Map.of("isIbca", true);

        String result = FileNameHelper.infectedBloodInfix(type, additionalData);

        assertThat(result).isEqualTo("");
    }

    @Test
    void should_return_empty_string_when_additional_data_is_null() {
        Map<String, Object> additionalData = null;

        String result = FileNameHelper.infectedBloodInfix(SSCS, additionalData);

        assertThat(result).isEqualTo("");
    }

    @Test
    void should_return_empty_string_when_isIbca_is_missing_or_false() {
        Map<String, Object> missingFieldData = Map.of();
        Map<String, Object> falseFieldData = Map.of("isIbca", false);

        String resultMissing = FileNameHelper.infectedBloodInfix(SSCS, missingFieldData);
        String resultFalse = FileNameHelper.infectedBloodInfix(SSCS, falseFieldData);

        assertThat(resultMissing).isEqualTo("");
        assertThat(resultFalse).isEqualTo("");
    }

    @Test
    void should_remove_underscores_from_service_name() {
        // given
        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "cmc_claim_store",
            null,
            "type",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        // when
        String name = FileNameHelper.generateName(letter);

        // then
        assertThat(name).contains("cmcclaimstore");
    }

    @Test
    void should_remove_underscores_from_letter_type() {
        // given
        Letter letter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "service",
            null,
            "some_type",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        // when
        String name = FileNameHelper.generateName(letter);

        // then
        assertThat(name).contains("sometype");
    }

    @Test
    void should_set_file_extension_based_on_whether_letter_is_encrypted() {
        Letter zippedLetter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "cmc",
            null,
            "type",
            null,
            false,
            null,
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        Letter encryptedLetter = new Letter(
            randomUUID(),
            randomUUID().toString(),
            "cmc",
            null,
            "type",
            null,
            true,
            "752c14ea195c369bac3c3b7896975ee9fd15eeb7",
            now(),
            objectMapper.valueToTree(Map.of("Document_1", 1))
        );

        assertThat(FileNameHelper.generateName(zippedLetter)).endsWith(".zip");
        assertThat(FileNameHelper.generateName(encryptedLetter)).endsWith(".pgp");
    }

    private Letter createLetter(UUID id, String type, String service) {
        Letter result = mock(Letter.class);
        when(result.getId()).thenReturn(id);
        when(result.getType()).thenReturn(type);
        when(result.getService()).thenReturn(service);
        return result;
    }
}
