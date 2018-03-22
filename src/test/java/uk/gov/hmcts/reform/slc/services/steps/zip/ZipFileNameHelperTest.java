package uk.gov.hmcts.reform.slc.services.steps.zip;

import org.junit.Test;
import uk.gov.hmcts.reform.sendletter.entity.Letter;

import java.util.regex.Pattern;

import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;

public class ZipFileNameHelperTest {

    @Test
    public void should_generate_expected_file_name() {
        // given
        Letter letter = new Letter(randomUUID().toString(), "cmc", null, "type", null);

        // when
        String name = ZipFileNameHelper.generateName(letter);

        // then
        assertThat(name)
            .matches(Pattern.compile("type_cmc_[0-9]{14}_" + letter.getId() + ".zip"));
    }

    @Test
    public void should_remove_underscores_from_service_name() {
        // given
        Letter letter = new Letter(randomUUID().toString(), "cmc_claim_store", null, "type", null);

        // when
        String name = ZipFileNameHelper.generateName(letter);

        // then
        assertThat(name).contains("cmcclaimstore");
    }
}
