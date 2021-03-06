package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.util.DigestUtils;

import static org.springframework.util.SerializationUtils.serialize;

public final class LetterChecksumGenerator {

    private LetterChecksumGenerator() {
    }

    public static String generateChecksum(Object letter) {
        return DigestUtils.md5DigestAsHex(serialize(letter));
    }
}
