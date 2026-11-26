package uk.gov.hmcts.reform.sendletter.util;

import com.eatthepath.otp.TimeBasedOneTimePasswordGenerator;
import org.apache.commons.codec.binary.Base32;

import java.security.InvalidKeyException;
import java.time.Instant;
import java.util.Locale;
import javax.crypto.spec.SecretKeySpec;

public final class TotpHelper {

    private TotpHelper() {
    }

    public static String generate(String base32Secret) {
        return generate(base32Secret, Instant.now());
    }

    /* default */ static String generate(String base32Secret, Instant timestamp) {
        byte[] key = new Base32().decode(base32Secret.toUpperCase(Locale.ROOT));

        try {
            return new TimeBasedOneTimePasswordGenerator().generateOneTimePasswordString(
                new SecretKeySpec(key, "HmacSHA1"), timestamp, Locale.ROOT
            );
        } catch (InvalidKeyException exception) {
            throw new IllegalStateException("Failed to generate one time password", exception);
        }
    }
}

