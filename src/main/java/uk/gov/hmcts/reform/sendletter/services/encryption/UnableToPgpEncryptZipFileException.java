package uk.gov.hmcts.reform.sendletter.services.encryption;

/**
 * This class represents the unable to PGP encrypt zip file exception.
 */
public class UnableToPgpEncryptZipFileException extends RuntimeException {

    private static final long serialVersionUID = 262685880220521685L;

    public UnableToPgpEncryptZipFileException(Throwable cause) {
        super(cause);
    }
}
