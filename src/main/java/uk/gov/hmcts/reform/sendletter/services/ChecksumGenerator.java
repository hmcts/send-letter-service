package uk.gov.hmcts.reform.sendletter.services;

import org.springframework.util.DigestUtils;

import static org.springframework.util.SerializationUtils.serialize;

public final class ChecksumGenerator {

    private ChecksumGenerator() {
    }

    public static String generateChecksum(Object obj) {
        return DigestUtils.md5DigestAsHex(serialize(obj));
    }
}
