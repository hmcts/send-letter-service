package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.util.Asserts;
import org.bouncycastle.openpgp.PGPException;
import org.bouncycastle.openpgp.PGPPublicKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.pdf.generator.HTMLToPDFConverter;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.UnsupportedLetterRequestTypeException;
import uk.gov.hmcts.reform.sendletter.model.PdfDoc;
import uk.gov.hmcts.reform.sendletter.model.in.ILetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;
import uk.gov.hmcts.reform.sendletter.services.encryption.PgpEncryptionUtil;
import uk.gov.hmcts.reform.sendletter.services.encryption.UnableToLoadPgpPublicKeyException;
import uk.gov.hmcts.reform.sendletter.services.encryption.UnableToPgpEncryptZipFileException;
import uk.gov.hmcts.reform.sendletter.services.pdf.DuplexPreparator;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;
import uk.gov.hmcts.reform.sendletter.services.util.FinalPackageFileNameHelper;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;

import java.io.IOException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.UUID;

import static java.time.LocalDateTime.now;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.services.LetterChecksumGenerator.generateChecksum;

@Service
public class LetterService {

    private static final Logger log = LoggerFactory.getLogger(LetterService.class);

    private final PdfCreator pdfCreator;
    private final LetterRepository letterRepository;
    private final Zipper zipper;
    private final ObjectMapper mapper;
    private final boolean isEncryptionEnabled;
    private final String encryptionPublicKey;
    private final PGPPublicKey pgpPublicKey;

    public LetterService(
        PdfCreator pdfCreator,
        LetterRepository letterRepository,
        Zipper zipper,
        ObjectMapper mapper,
        @Value("${encryption.enabled}") Boolean isEncryptionEnabled,
        @Value("${encryption.publicKey}") String encryptionPublicKey
    ) {
        this.pdfCreator = pdfCreator;
        this.letterRepository = letterRepository;
        this.zipper = zipper;
        this.mapper = mapper;
        this.isEncryptionEnabled = isEncryptionEnabled;
        this.encryptionPublicKey = encryptionPublicKey;
        this.pgpPublicKey = loadPgpPublicKey(encryptionPublicKey);
    }

    // TODO: remove
    @Autowired
    public LetterService(
        LetterRepository letterRepository,
        Zipper zipper,
        ObjectMapper mapper,
        @Value("${encryption.enabled}") Boolean isEncryptionEnabled,
        @Value("${encryption.publicKey}") String encryptionPublicKey
    ) {
        this(
            new PdfCreator(new DuplexPreparator(), new HTMLToPDFConverter()::convert),
            letterRepository,
            zipper,
            mapper,
            isEncryptionEnabled,
            encryptionPublicKey
        );
    }

    @Transactional
    public UUID send(ILetterRequest letter, String serviceName) {
        String messageId = generateChecksum(letter);
        Asserts.notEmpty(serviceName, "serviceName");

        return letterRepository
            .findByMessageIdAndStatusOrderByCreatedAtDesc(messageId, Created)
            .map(duplicate -> {
                UUID id = duplicate.getId();
                log.info("Same message found already created. Returning letter id {} instead", id);
                return id;
            })
            .orElseGet(() -> saveNewLetter(letter, messageId, serviceName));
    }

    private UUID saveNewLetter(ILetterRequest letter, String messageId, String serviceName) {
        UUID id = UUID.randomUUID();

        byte[] zipContent = zipper.zip(
            new PdfDoc(
                FileNameHelper.generatePdfName(letter.getType(), serviceName, id),
                getPdfContent(letter)
            )
        );

        byte[] letterContent;
        LocalDateTime createdAtTime = now();

        if (isEncryptionEnabled) {
            letterContent = encryptZipContents(letter, serviceName, id, zipContent, createdAtTime);
        } else {
            letterContent = zipContent;
        }


        Letter dbLetter = new Letter(
            id,
            messageId,
            serviceName,
            mapper.valueToTree(letter.getAdditionalData()),
            letter.getType(),
            letterContent,
            isEncryptionEnabled,
            Timestamp.valueOf(createdAtTime)
        );

        letterRepository.save(dbLetter);

        log.info("Created new letter {}", id);

        return id;
    }

    private byte[] encryptZipContents(
        ILetterRequest letter,
        String serviceName,
        UUID id,
        byte[] zipContent,
        LocalDateTime createdAt
    ) {
        Asserts.notNull(encryptionPublicKey, "encryptionPublicKey");

        String zipFileName = FinalPackageFileNameHelper.generateName(letter.getType(), serviceName, createdAt, id);

        try {
            return PgpEncryptionUtil.encryptFile(zipContent, zipFileName, pgpPublicKey, true);

        } catch (IOException ioException) {
            log.error(
                "Error occurred while loading public key encryption",
                ioException
            );
            throw new UnableToLoadPgpPublicKeyException("PGP Public key object could not be constructed", ioException);
        } catch (PGPException pgpException) {
            log.error(
                String.format("Error occurred during encrypting zip file: %s", zipFileName),
                pgpException
            );

            throw new UnableToPgpEncryptZipFileException(pgpException);
        }
    }

    private PGPPublicKey loadPgpPublicKey(String encryptionPublicKey) {
        if (!isEncryptionEnabled) {
            log.info("Encryption is not enabled hence not loading the public key");
            return null;
        }

        Asserts.notNull(encryptionPublicKey, "encryptionPublicKey");

        try {
            return PgpEncryptionUtil.loadPublicKey(encryptionPublicKey.getBytes());
        } catch (IOException ioException) {
            log.error(
                "Error occurred while loading public key encryption",
                ioException
            );
            throw new UnableToLoadPgpPublicKeyException("PGP Public key object could not be constructed", ioException);
        }
    }

    private byte[] getPdfContent(ILetterRequest letter) {
        if (letter instanceof LetterRequest) {
            return pdfCreator.createFromTemplates(((LetterRequest) letter).documents);
        } else if (letter instanceof LetterWithPdfsRequest) {
            return pdfCreator.createFromBase64Pdfs(((LetterWithPdfsRequest) letter).documents);
        } else {
            throw new UnsupportedLetterRequestTypeException();
        }
    }

    public LetterStatus getStatus(UUID id, String serviceName) {
        Letter letter = letterRepository
            .findByIdAndService(id, serviceName)
            .orElseThrow(() -> new LetterNotFoundException(id));

        return new LetterStatus(
            id,
            letter.getMessageId(),
            toDateTime(letter.getCreatedAt()),
            toDateTime(letter.getSentToPrintAt()),
            toDateTime(letter.getPrintedAt()),
            letter.isFailed()
        );
    }

    public static ZonedDateTime toDateTime(Timestamp stamp) {
        if (null == stamp) {
            return null;
        }
        return stamp.toInstant().atZone(ZoneId.of("UTC"));
    }
}
