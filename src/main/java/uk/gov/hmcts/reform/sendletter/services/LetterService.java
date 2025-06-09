package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.hc.client5.http.utils.Hex;
import org.apache.hc.core5.util.Asserts;
import org.bouncycastle.openpgp.PGPPublicKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.DuplicateLetter;
import uk.gov.hmcts.reform.sendletter.entity.ExceptionLetter;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterEvent;
import uk.gov.hmcts.reform.sendletter.entity.LetterEventRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterSaveException;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.exception.UnsupportedLetterRequestTypeException;
import uk.gov.hmcts.reform.sendletter.model.PdfDoc;
import uk.gov.hmcts.reform.sendletter.model.in.ILetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;
import uk.gov.hmcts.reform.sendletter.model.out.ExtendedLetterStatus;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatus;
import uk.gov.hmcts.reform.sendletter.model.out.LetterStatusEvent;
import uk.gov.hmcts.reform.sendletter.model.out.v2.LetterStatusV2;
import uk.gov.hmcts.reform.sendletter.services.encryption.PgpEncryptionUtil;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.pdf.PdfCreator;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;
import uk.gov.hmcts.reform.sendletter.services.zip.Zipper;
import uk.gov.hmcts.reform.sendletter.util.TimeZones;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.IntStream;

import static java.time.LocalDateTime.now;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;

/**
 * Service to handle letters.
 */
@Service(value = "LetterService")
public class LetterService {

    private static final Logger log = LoggerFactory.getLogger(LetterService.class);

    private final PdfCreator pdfCreator;
    private final LetterRepository letterRepository;
    private final LetterEventRepository letterEventRepository;
    private final DocumentService documentService;
    private final Zipper zipper;
    private final ObjectMapper mapper;
    private final boolean isEncryptionEnabled;
    private final PGPPublicKey pgpPublicKey;
    private final ServiceFolderMapping serviceFolderMapping;
    private final ExecusionService asyncService;
    private final DuplicateLetterService duplicateLetterService;
    private final ExceptionLetterService exceptionLetterService;
    private final LetterChecksumService letterChecksumService;
    private static final Map<String, Integer> DEFAULT_COPY = Map.of(getCopiesKey(1), 1);

    /**
     * Constructor for the LetterService.
     *
     * @param pdfCreator             The pdf creator
     * @param letterRepository       The repository for letter
     * @param letterEventRepository  The repository for letter event
     * @param documentService        The document service
     * @param zipper                 The zipper
     * @param mapper                 The object mapper
     * @param isEncryptionEnabled    The encryption enabled flag
     * @param encryptionPublicKey    The encryption public key
     * @param serviceFolderMapping   The service folder mapping
     * @param asyncService           The async service
     * @param duplicateLetterService The duplicate letter service
     * @param exceptionLetterService The exception letter service
     * @param letterChecksumService  The letter checksum service
     */
    @SuppressWarnings("java:S107")
    public LetterService(
        PdfCreator pdfCreator,
        LetterRepository letterRepository,
        LetterEventRepository letterEventRepository,
        DocumentService documentService,
        Zipper zipper,
        ObjectMapper mapper,
        @Value("${encryption.enabled}") Boolean isEncryptionEnabled,
        @Value("${encryption.publicKey}") String encryptionPublicKey,
        ServiceFolderMapping serviceFolderMapping,
        ExecusionService asyncService,
        DuplicateLetterService duplicateLetterService,
        ExceptionLetterService exceptionLetterService,
        LetterChecksumService letterChecksumService
    ) {
        this.pdfCreator = pdfCreator;
        this.letterRepository = letterRepository;
        this.letterEventRepository = letterEventRepository;
        this.documentService = documentService;
        this.zipper = zipper;
        this.mapper = mapper;
        this.isEncryptionEnabled = isEncryptionEnabled;
        this.pgpPublicKey = loadPgpPublicKey(encryptionPublicKey);
        this.serviceFolderMapping = serviceFolderMapping;
        this.asyncService = asyncService;
        this.duplicateLetterService = duplicateLetterService;
        this.exceptionLetterService = exceptionLetterService;
        this.letterChecksumService = letterChecksumService;
    }

    /**
     * Save a letter.
     *
     * @param letter      The letter to save
     * @param serviceName The name of the service
     * @param isAsync     The async flag
     * @return The id of the saved letter
     */
    @Transactional
    public UUID save(ILetterRequest letter, String serviceName, String isAsync) {
        String checksum = letterChecksumService.generateChecksum(letter);
        Asserts.notEmpty(serviceName, "serviceName");

        log.info("Saving letter, service {}, messageId {}", serviceName, checksum);

        if (serviceFolderMapping.getFolderFor(serviceName).isEmpty()) {
            String message = "No configuration for service " + serviceName + " found";
            log.error(message);
            throw new ServiceNotConfiguredException(message);
        }

        return letterRepository
            .findByChecksumAndStatusOrderByCreatedAtDesc(checksum, Created)
            .map(duplicate -> {
                UUID id = duplicate.getId();
                log.info(
                    "Same message found already created. Returning letter id {} instead. Service {}, messageId {}",
                    id,
                    serviceName,
                    checksum
                );
                return id;
            })
            .orElseGet(() -> saveNewLetter(letter, checksum, serviceName, isAsync));
    }

    /**
     * Save a new letter.
     *
     * @param letter      The letter to save
     * @param messageId   The message id
     * @param serviceName The name of the service
     * @param isAsync     The async flag
     * @return The id of the saved letter
     */
    private UUID saveNewLetter(ILetterRequest letter, String messageId, String serviceName, String isAsync) {
        Optional<String> recipientsChecksum =
            (!(letter.getAdditionalData() == null || letter.getAdditionalData().isEmpty())
                && Objects.requireNonNull(letter.getAdditionalData()).containsKey("recipients"))
                ? Optional.of(letterChecksumService.generateChecksum(
                mapper.valueToTree(letter.getAdditionalData().get("recipients"))))
                : Optional.empty();

        if (recipientsChecksum.isPresent()) {
            Optional<UUID> duplicateDocUUID = documentService.checkDocumentDuplicates(
                getDocumentsFromLetter(letter), recipientsChecksum.get());
            if (duplicateDocUUID.isPresent()) {
                return duplicateDocUUID.get();
            }
        }

        UUID letterId = UUID.randomUUID();
        String loggingContext = String.format(
            "letter  %s, service %s, messageId %s, additionalData %s",
            letterId,
            serviceName,
            messageId, mapper.valueToTree(letter.getAdditionalData())
        );

        byte[] zipContent = zipper.zip(
            new PdfDoc(
                FileNameHelper.generatePdfName(letter.getType(), serviceName, letterId, letter.getAdditionalData()),
                getPdfContent(letter, loggingContext)
            )
        );

        Function<LocalDateTime, byte[]> fileContent = localDateTime -> getFileContent(letterId, letter,
            serviceName, localDateTime, zipContent);

        if (letter instanceof LetterRequest) {
            log.info(
                "Team {} is still using the v1 api call and renders pdf templates. Letter letterId = {}, messageId {}",
                serviceName,
                letterId,
                messageId
            );
        }

        if (Boolean.parseBoolean(isAsync)) {
            Runnable logger = () -> log.info(
                "Saving letter id {} in async mode as flag value is {}, service {}, messageId {}",
                letterId,
                isAsync,
                serviceName,
                messageId
            );
            asyncService.run(() -> saveLetter(letter, messageId, serviceName, letterId,
                fileContent, recipientsChecksum), logger,
                () -> saveDuplicate(letter, letterId, messageId, serviceName, isAsync),
                message -> saveException(letter, letterId, serviceName, message, isAsync));
        } else {
            try {
                log.info(
                    "Saving letter id {} in sync mode as flag value is {}, service {}, messageId {}",
                    letterId,
                    isAsync,
                    serviceName,
                    messageId
                );
                asyncService.execute(() -> saveLetter(letter, messageId, serviceName, letterId,
                    fileContent, recipientsChecksum));
            } catch (DataIntegrityViolationException dataIntegrityViolationException) {
                Runnable logger = () -> log.error(
                    "Duplicate record, letter id {}, service {}, messageId {}",
                    letterId,
                    serviceName,
                    messageId,
                    dataIntegrityViolationException
                );
                asyncService.run(() -> saveDuplicate(letter, letterId, messageId, serviceName, isAsync), logger,
                    () -> {
                    }, message -> saveException(letter, letterId, serviceName,
                        zipContent.length + ":" + message, isAsync));
                throw dataIntegrityViolationException;
            }
        }
        log.info("Returning letter letterId {} for service {}, messageId {}", letterId, serviceName, messageId);

        return letterId;
    }

    /**
     * Save a letter.
     *
     * @param letter             The letter to save
     * @param messageId          The message id
     * @param serviceName        The name of the service
     * @param id                 The id of the letter
     * @param zipContent         The zip content
     * @param recipientsChecksum The recipients checksum
     */
    @Transactional
    public void saveLetter(ILetterRequest letter, String messageId, String serviceName, UUID id,
                           Function<LocalDateTime, byte[]> zipContent, Optional<String> recipientsChecksum) {
        LocalDateTime createdAtTime = now();
        Letter dbLetter = new Letter(
            id,
            messageId,
            serviceName,
            mapper.valueToTree(letter.getAdditionalData()),
            letter.getType(),
            zipContent.apply(createdAtTime),
            isEncryptionEnabled,
            getEncryptionKeyFingerprint(),
            createdAtTime,
            mapper.valueToTree(getCopies(letter))
        );

        letterRepository.save(dbLetter);

        documentService.saveDocuments(id, getDocumentsFromLetter(letter), recipientsChecksum.orElse(null));

        log.info("Created new letter record with id {} for service {}, messageId {}", id, serviceName, messageId);
    }

    /**
     * Save a duplicate letter.
     *
     * @param letter      The letter to save
     * @param id          The id of the letter
     * @param checksum    The checksum
     * @param serviceName The name of the service
     * @param isAsync     The async flag
     */
    @Transactional
    public void saveDuplicate(ILetterRequest letter, UUID id, String checksum, String serviceName,
                              String isAsync) {
        DuplicateLetter duplicateLetter = getDuplicateLetter(letter, id, checksum, serviceName,
            isAsync);
        duplicateLetterService.save(duplicateLetter);
        log.info("Created new duplicate record with id {} for service {}", id, serviceName);
    }

    /**
     * Save an exception.
     *
     * @param letter      The letter to save
     * @param id          The id of the letter
     * @param serviceName The name of the service
     * @param message     The message
     * @param isAsync     The async flag
     */
    @Transactional
    public void saveException(ILetterRequest letter, UUID id, String serviceName, String message, String isAsync) {
        ExceptionLetter exceptionLetter = new ExceptionLetter(id, serviceName, LocalDateTime.now(),
            letter.getType(), message, isAsync);
        exceptionLetterService.save(exceptionLetter);
        log.info("Created new exception record with id {} for service {}", id, serviceName);
    }

    /**
     * Get a duplicate letter.
     *
     * @param letter      The letter
     * @param id          The id of the letter
     * @param checksum    The checksum
     * @param serviceName The name of the service
     * @param isAsync     The async flag
     * @return The duplicate letter
     */
    private DuplicateLetter getDuplicateLetter(ILetterRequest letter, UUID id,
                                               String checksum, String serviceName,
                                               String isAsync) {
        LocalDateTime createdAtTime = now();
        return new DuplicateLetter(
            id,
            checksum,
            serviceName,
            mapper.valueToTree(letter.getAdditionalData()),
            letter.getType(),
            createdAtTime,
            mapper.valueToTree(getCopies(letter)),
            isAsync
        );
    }

    /**
     * Get the file content.
     *
     * @param id            The id of the letter
     * @param letter        The letter
     * @param serviceName   The name of the service
     * @param createdAtTime The created at time
     * @param zipContent    The zip content
     * @return The file content
     */
    private byte[] getFileContent(UUID id, ILetterRequest letter, String serviceName,
                                  LocalDateTime createdAtTime, byte[] zipContent) {
        if (isEncryptionEnabled) {
            zipContent = encryptZipContents(letter, serviceName, id, zipContent, createdAtTime);
        }
        return zipContent;
    }

    /**
     * Encrypt the zip contents.
     *
     * @param letter      The letter
     * @param serviceName The name of the service
     * @param id          The id of the letter
     * @param zipContent  The zip content
     * @param createdAt   The created at time
     * @return The encrypted zip contents
     */
    private byte[] encryptZipContents(
        ILetterRequest letter,
        String serviceName,
        UUID id,
        byte[] zipContent,
        LocalDateTime createdAt
    ) {
        Asserts.notNull(pgpPublicKey, "pgpPublicKey");
        String zipFileName = FileNameHelper.generateName(
            letter.getType(),
            serviceName,
            createdAt,
            id,
            false,
            letter.getAdditionalData()
        );

        return PgpEncryptionUtil.encryptFile(zipContent, zipFileName, pgpPublicKey);
    }

    /**
     * Get the encryption key fingerprint.
     *
     * @return The encryption key fingerprint
     */
    private String getEncryptionKeyFingerprint() {
        if (isEncryptionEnabled) {
            return Hex.encodeHexString(this.pgpPublicKey.getFingerprint());
        } else {
            return null;
        }
    }

    /**
     * Load a PGP public key.
     *
     * @param encryptionPublicKey The encryption public key
     * @return The PGP public key
     */
    private PGPPublicKey loadPgpPublicKey(String encryptionPublicKey) {
        if (!isEncryptionEnabled) {
            log.info("""
                Encryption is not enabled hence not loading the public key
                """);
            return null;
        } else {
            Asserts.notNull(encryptionPublicKey, "encryptionPublicKey");
            return PgpEncryptionUtil.loadPublicKey(encryptionPublicKey.getBytes());
        }
    }

    /**
     * Get the documents from a letter.
     *
     * @param letter The letter
     * @return The documents
     */
    private List<?> getDocumentsFromLetter(ILetterRequest letter) {
        if (letter instanceof LetterRequest) {
            return ((LetterRequest) letter).documents;
        } else if (letter instanceof LetterWithPdfsRequest) {
            return ((LetterWithPdfsRequest) letter).documents;
        } else if (letter instanceof LetterWithPdfsAndNumberOfCopiesRequest) {
            return ((LetterWithPdfsAndNumberOfCopiesRequest) letter).documents;
        } else {
            throw new UnsupportedLetterRequestTypeException();
        }
    }

    /**
     * Get the PDF content.
     *
     * @param letter         The letter
     * @param loggingContext The logging context
     * @return The PDF content
     */
    private byte[] getPdfContent(ILetterRequest letter, String loggingContext) {
        switch (letter) {
            case LetterWithPdfsRequest letterWithPdfsRequest -> {
                return pdfCreator.createFromBase64Pdfs(letterWithPdfsRequest.documents, loggingContext);
            }
            case LetterWithPdfsAndNumberOfCopiesRequest letterWithPdfsAndNumberOfCopiesRequest -> {
                return pdfCreator
                    .createFromBase64PdfWithCopies(letterWithPdfsAndNumberOfCopiesRequest.documents, loggingContext);
            }
            default -> throw new UnsupportedLetterRequestTypeException();
        }
    }

    /**
     * Get the copies.
     *
     * @param letter The letter
     * @return The copies
     */
    private Map<String, Integer> getCopies(LetterWithPdfsAndNumberOfCopiesRequest letter) {
        return IntStream.range(0, letter.documents.size())
            .collect(HashMap::new, (map, count) -> map.put(getCopiesKey(count + 1),
                letter.documents.get(count).copies), Map::putAll);
    }

    /**
     * Get the copies.
     *
     * @param letter The letter
     * @return The copies as a LetterWithPdfsAndNumberOfCopiesRequest
     */
    private Map<String, Integer> getCopies(ILetterRequest letter) {
        if (letter instanceof LetterWithPdfsAndNumberOfCopiesRequest) {
            return getCopies((LetterWithPdfsAndNumberOfCopiesRequest) letter);
        }
        return DEFAULT_COPY;
    }

    /**
     * Get the copies.
     *
     * @param count The count
     * @return The copies key
     */
    private static String getCopiesKey(int count) {
        return String.join("_", "Document", String.valueOf(count));
    }

    /**
     * Get the letter status.
     *
     * @param id                       The id of the letter
     * @param isAdditionalDataRequired The additional data required flag
     * @param isDuplicate              The duplicate flag
     * @return The letter status
     */
    public LetterStatus getStatus(UUID id, String isAdditionalDataRequired, String isDuplicate) {
        exceptionCheck(id);
        duplicateCheck(id, isDuplicate);

        Function<JsonNode, Map<String, Object>> additionDataFunction = additionalData -> {
            if (Boolean.parseBoolean(isAdditionalDataRequired)) {
                return Optional.ofNullable(additionalData)
                    .map(data -> mapper.convertValue(data, new TypeReference<Map<String, Object>>() {
                    }))
                    .orElse(Collections.emptyMap());
            }
            return null;
        };

        Optional<LetterStatus> optionalLetterStatus = getStatusWithRetries(id, additionDataFunction);
        if (optionalLetterStatus.isPresent()) {
            return optionalLetterStatus.get();
        } else {
            throw new LetterNotFoundException(id);
        }
    }

    /**
     * The function `getStatusWithRetries` attempts to retrieve the status of a letter with retries in case of delays,
     * returning an optional `LetterStatus`.
     *
     * @param id The `id` parameter is a unique identifier (UUID) for the letter whose status is being retrieved.
     * @param additionDataFunction The `additionDataFunction` parameter in the `getStatusWithRetries`
     *                             method is a `Function` that takes a `JsonNode` as input and
     *                             returns a map of string/object.
     * @return The `getStatusWithRetries` method returns an `Optional` object that may contain
     *      a `LetterStatus` if it is successfully retrieved from the repository after up to 3 retries.
     *      If the status is not found even after the retries, an empty `Optional` is returned.
     */
    private Optional<LetterStatus> getStatusWithRetries(
        // Attempt to get the status of the letter. This happens right after calling the endpoint
        // to save it, so there can be a delay between the commit happening. Therefore it needs
        // to be polled briefly to avoid exceptions being raised alongside multiple retries from the client.
        UUID id, Function<JsonNode, Map<String, Object>> additionDataFunction) {
        for (int retryCount = 0; retryCount < 3; retryCount++) {
            Optional<LetterStatus> optionalLetterStatus = getLetterStatusFromRepository(id, additionDataFunction);
            if (optionalLetterStatus.isPresent()) {
                return optionalLetterStatus;
            }
            try {
                TimeUnit.SECONDS.sleep(1);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
        }
        // Should not get here, but if so, return empty
        return Optional.empty();
    }

    /**
     * This function retrieves a LetterStatus object from a repository based on a given UUID
     * and additional data function.
     *
     * @param id The `id` parameter is a UUID (Universally Unique Identifier) used to uniquely
     *           identify a letter in the repository.
     * @param additionDataFunction The `additionDataFunction` parameter is a `Function`
     *                             that takes a `JsonNode` as input and returns a map of string/object.
     * @return An Optional object containing a LetterStatus object is being returned. The LetterStatus
     *      object is created using data retrieved from the letterRepository based on the provided id.
     *      The data includes the status name, checksum, creation date, sent to print date, printed date,
     *      additional data obtained by applying the additionDataFunction to the letter's additional
     *      data, and a null value for another field.
     */
    private Optional<LetterStatus> getLetterStatusFromRepository(
        UUID id, Function<JsonNode, Map<String, Object>> additionDataFunction) {
        return letterRepository
            .findById(id)
            .map(letter -> new LetterStatus(
                id,
                letter.getStatus().name(),
                letter.getChecksum(),
                toDateTime(letter.getCreatedAt()),
                toDateTime(letter.getSentToPrintAt()),
                toDateTime(letter.getPrintedAt()),
                additionDataFunction.apply(letter.getAdditionalData()),
                null
            ));
    }

    /**
     * Get the extended letter status.
     *
     * @param id                       The id of the letter
     * @param isAdditionalDataRequired The additional data required flag
     * @param isDuplicate              The duplicate flag
     * @return The extended letter status
     */
    public ExtendedLetterStatus getExtendedStatus(
        UUID id,
        String isAdditionalDataRequired,
        String isDuplicate
    ) {
        log.info("Getting letter status for id {} ", id);
        exceptionCheck(id);
        duplicateCheck(id, isDuplicate);

        Function<JsonNode, Map<String, Object>> additionDataFunction = additionalData -> {
            if (Boolean.parseBoolean(isAdditionalDataRequired)) {
                return Optional.ofNullable(additionalData)
                    .map(data -> mapper.convertValue(data, new TypeReference<Map<String, Object>>() {
                    }))
                    .orElse(Collections.emptyMap());
            }
            return null;
        };

        ExtendedLetterStatus letterStatus = letterRepository
            .findById(id)
            .map(letter -> new ExtendedLetterStatus(
                id,
                letter.getStatus().name(),
                letter.getChecksum(),
                toDateTime(letter.getCreatedAt()),
                toDateTime(letter.getSentToPrintAt()),
                toDateTime(letter.getPrintedAt()),
                additionDataFunction.apply(letter.getAdditionalData()),
                null,
                getLetterStatusEvents(letter)
            ))
            .orElseThrow(() -> new LetterNotFoundException(id));
        log.info("Returning  letter status for letter {}, letter id {}", letterStatus.status, id);
        return letterStatus;
    }

    /**
     * Get the latest status.
     *
     * @param id The id of the letter
     * @return The latest status (V2)
     */
    public LetterStatusV2 getLatestStatus(UUID id) {
        log.info("Getting v2 letter status for id {} ", id);

        LetterStatusV2 letterStatus = letterRepository
            .findById(id)
            .map(letter -> new LetterStatusV2(
                id,
                letter.getStatus().name(),
                letter.getChecksum(),
                toDateTime(letter.getCreatedAt()),
                toDateTime(letter.getSentToPrintAt()),
                toDateTime(letter.getPrintedAt()),
                mapper.convertValue(letter.getAdditionalData(), new TypeReference<>() {
                }),
                mapper.convertValue(letter.getCopies(), new TypeReference<>() {
                })
            ))
            .orElseThrow(() -> new LetterNotFoundException(id));
        log.info("Returning v2 letter status for letter {}, letter id {}", letterStatus, id);
        return letterStatus;
    }

    /**
     * Check for exception.
     *
     * @param id The id of the letter
     */
    private void exceptionCheck(UUID id) {
        if (exceptionLetterService.isException(id).isPresent()) {
            throw new LetterSaveException();
        }
    }

    /**
     * Check for duplicate.
     *
     * @param id          The id of the letter
     * @param isDuplicate The duplicate flag
     */
    private void duplicateCheck(UUID id, String isDuplicate) {
        if (Boolean.parseBoolean(isDuplicate)) {
            Optional<DuplicateLetter> optDuplicateLetter = duplicateLetterService.isDuplicate(id);
            if (optDuplicateLetter.isPresent()) {
                DuplicateLetter duplicateLetter = optDuplicateLetter.get();
                String duplicateMessage = String.join(",",
                    "Duplicate record for service:", duplicateLetter.getService(),
                    " with checksum:", duplicateLetter.getChecksum());
                throw new DataIntegrityViolationException(duplicateMessage);
            }
        }
    }

    /**
     * Get the letter status events.
     *
     * @param letter The letter
     * @return The letter status events
     */
    private List<LetterStatusEvent> getLetterStatusEvents(Letter letter) {
        List<LetterEvent> letterEvents = letterEventRepository.findAllByLetterOrderByCreatedAt(letter);
        return letterEvents.stream().map(
            letterEvent -> new LetterStatusEvent(
                letterEvent.getType().name(),
                letterEvent.getNotes(),
                toDateTime(
                    LocalDateTime.ofInstant(
                        letterEvent.getCreatedAt(),
                        ZoneId.of(TimeZones.EUROPE_LONDON)
                    )
                )
            )
        ).toList();
    }

    /**
     * Convert a LocalDateTime to a ZonedDateTime.
     *
     * @param dateTime The date time
     * @return The zoned date time
     */
    static ZonedDateTime toDateTime(LocalDateTime dateTime) {
        if (null == dateTime) {
            return null;
        }

        return dateTime.atZone(ZoneId.of("UTC"));
    }
}
