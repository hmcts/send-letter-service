package uk.gov.hmcts.reform.sendletter.util;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.time.Instant;
import java.util.Locale;

import static org.assertj.core.api.Assertions.assertThat;

class TotpHelperTest {

    private static final String SECRET = "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ";

    @ParameterizedTest
    @CsvSource({
        "59, 287082",
        "1111111109, 081804",
        "1111111111, 050471",
        "1234567890, 005924",
        "2000000000, 279037",
        "20000000000, 353130"
    })
    void shouldGenerateSixDigitTotp(long epochSeconds, String expected) {
        assertThat(TotpHelper.generate(SECRET, Instant.ofEpochSecond(epochSeconds))).isEqualTo(expected);
    }

    @Test
    void shouldAcceptLowercaseBase32Secret() {
        assertThat(TotpHelper.generate(SECRET.toLowerCase(Locale.ROOT), Instant.ofEpochSecond(59)))
            .isEqualTo("287082");
    }

    @Test
    void shouldGenerateSixDigitsForCurrentTime() {
        assertThat(TotpHelper.generate(SECRET)).matches("[0-9]{6}");
    }
}
