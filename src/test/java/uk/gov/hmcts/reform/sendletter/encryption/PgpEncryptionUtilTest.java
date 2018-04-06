package uk.gov.hmcts.reform.sendletter.encryption;

import com.google.common.io.Resources;
import org.bouncycastle.openpgp.PGPPublicKey;
import org.junit.Test;

import static com.google.common.io.Resources.getResource;
import static org.assertj.core.api.Assertions.assertThat;

public class PgpEncryptionUtilTest {

    @Test
    public void should_pgp_encrypt_and_create_pgp_encrypted_zip_file_when_valid_public_key_is_passed() throws Exception {
        //Given
        byte[] inputZipFile = Resources.toByteArray(getResource("unencrypted.zip"));

        byte[] pubKey = Resources.toByteArray(getResource("pubkey.asc"));
        PGPPublicKey pgpPublicKey = PgpEncryptionUtil.loadPublicKey(pubKey);

        String fileNamePrefix = "encrypted";
        String fileNameSuffix = "pgp";

        //when
        byte[] pgpEncryptedZip = PgpEncryptionUtil.encryptFile(
            inputZipFile,
            fileNamePrefix,
            fileNameSuffix,
            pgpPublicKey,
            true
        );

        //We are decrypting it using BountyCastle to validate if the decrypted zip is same as input file.
        //Currently this seems to be the only way to validate the file contents.
        byte[] decryptedZip = PgpDecryptionHelper.decryptFile(
            pgpEncryptedZip,
            getClass().getResourceAsStream("/privatekey.asc"),
            "Password1".toCharArray()
        );

        //then
        assertThat(inputZipFile).containsExactly(decryptedZip);
    }

    @Test
    public void should_pgp_encrypt_and_create_pgp_encrypted_zip_file_when_valid_public_key_is_passed_and_without_integrity_check()
        throws Exception {
        //Given
        byte[] inputZipFile = Resources.toByteArray(getResource("unencrypted.zip"));

        byte[] pubKey = Resources.toByteArray(getResource("pubkey.asc"));
        PGPPublicKey pgpPublicKey = PgpEncryptionUtil.loadPublicKey(pubKey);

        String fileNamePrefix = "encrypted";
        String fileNameSuffix = "pgp";

        //when
        byte[] pgpEncryptedZip = PgpEncryptionUtil.encryptFile(
            inputZipFile,
            fileNamePrefix,
            fileNameSuffix,
            pgpPublicKey,
            false
        );

        //We are decrypting it using BountyCastle to validate if the decrypted zip is same as input file.
        //Currently this seems to be the only way to validate the file contents.
        byte[] decryptedZip = PgpDecryptionHelper.decryptFile(
            pgpEncryptedZip,
            getClass().getResourceAsStream("/privatekey.asc"),
            "Password1".toCharArray()
        );

        //then
        assertThat(inputZipFile).containsExactly(decryptedZip);
    }
}
