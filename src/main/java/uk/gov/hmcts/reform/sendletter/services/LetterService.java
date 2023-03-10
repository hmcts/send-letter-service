package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.codec.binary.Hex;
import org.apache.http.util.Asserts;
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
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.IntStream;

import static java.time.LocalDateTime.now;
import static uk.gov.hmcts.reform.sendletter.entity.LetterStatus.Created;
import static uk.gov.hmcts.reform.sendletter.services.LetterChecksumGenerator.generateChecksum;

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
    private static final Map<String, Integer> DEFAULT_COPY = Map.of(getCopiesKey(1), 1);

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
        ExceptionLetterService exceptionLetterService
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
    }

    @Transactional
    public UUID save(ILetterRequest letter, String serviceName, String isAsync) {
        String checksum = generateChecksum(letter);
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

    private UUID saveNewLetter(ILetterRequest letter, String messageId, String serviceName, String isAsync) {
        UUID letterId = UUID.randomUUID();
        String loggingContext = String.format(
            "letter  %s, service %s, messageId %s, additionalData %s",
            letterId,
            serviceName,
            messageId, mapper.valueToTree(letter.getAdditionalData())
        );

        log.info("letterId {}, service {}, messageId {}", letterId, serviceName, messageId);
        byte[] zipContent = zipper.zip(
            new PdfDoc(
                FileNameHelper.generatePdfName(letter.getType(), serviceName, letterId),
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
            asyncService.run(() -> saveLetter(letter, messageId, serviceName, letterId, fileContent), logger,
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
                asyncService.execute(() -> saveLetter(letter, messageId, serviceName, letterId, fileContent));
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

    @Transactional
    public void saveLetter(ILetterRequest letter, String messageId, String serviceName, UUID id,
                           Function<LocalDateTime, byte[]> zipContent) {
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

        documentService.saveDocuments(id, getDocumentsFromLetter(letter));

        log.info("Created new letter record with id {} for service {}, messageId {}", id, serviceName, messageId);
    }

    @Transactional
    public void saveDuplicate(ILetterRequest letter, UUID id, String checksum, String serviceName,
                              String isAsync) {
        DuplicateLetter duplicateLetter = getDuplicateLetter(letter, id, checksum, serviceName,
            isAsync);
        duplicateLetterService.save(duplicateLetter);
        log.info("Created new duplicate record with id {} for service {}", id, serviceName);
    }

    @Transactional
    public void saveException(ILetterRequest letter, UUID id, String serviceName, String message, String isAsync) {
        ExceptionLetter exceptionLetter = new ExceptionLetter(id, serviceName, LocalDateTime.now(),
            letter.getType(), message, isAsync);
        exceptionLetterService.save(exceptionLetter);
        log.info("Created new exception record with id {} for service {}", id, serviceName);
    }

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

    private byte[] getFileContent(UUID id, ILetterRequest letter, String serviceName,
                                  LocalDateTime createdAtTime, byte[] zipContent) {
        if (isEncryptionEnabled) {
            zipContent = encryptZipContents(letter, serviceName, id, zipContent, createdAtTime);
        }
        return zipContent;
    }

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
            false
        );

        return PgpEncryptionUtil.encryptFile(zipContent, zipFileName, pgpPublicKey);
    }

    private String getEncryptionKeyFingerprint() {
        if (isEncryptionEnabled) {
            return Hex.encodeHexString(this.pgpPublicKey.getFingerprint());
        } else {
            return null;
        }
    }

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

    private byte[] getPdfContent(ILetterRequest letter, String loggingContext) {
        if (letter instanceof LetterRequest) {
            return pdfCreator.createFromTemplates(((LetterRequest) letter).documents, loggingContext);
        } else if (letter instanceof LetterWithPdfsRequest) {
            return pdfCreator.createFromBase64Pdfs(((LetterWithPdfsRequest) letter).documents, loggingContext);
        } else if (letter instanceof LetterWithPdfsAndNumberOfCopiesRequest) {
            return pdfCreator
                .createFromBase64PdfWithCopies(
                    ((LetterWithPdfsAndNumberOfCopiesRequest) letter).documents,
                    loggingContext
                );
        } else {
            throw new UnsupportedLetterRequestTypeException();
        }
    }

    private Map<String, Integer> getCopies(LetterWithPdfsAndNumberOfCopiesRequest letter) {
        return IntStream.range(0, letter.documents.size())
            .collect(HashMap::new, (map, count) -> map.put(getCopiesKey(count + 1),
                letter.documents.get(count).copies), Map::putAll);
    }

    private Map<String, Integer> getCopies(ILetterRequest letter) {
        if (letter instanceof LetterWithPdfsAndNumberOfCopiesRequest) {
            return getCopies((LetterWithPdfsAndNumberOfCopiesRequest) letter);
        }
        return DEFAULT_COPY;
    }

    private static String getCopiesKey(int count) {
        return String.join("_", "Document", String.valueOf(count));
    }

    public LetterStatus getStatus(UUID id, String isAdditionalDataRequired, String isDuplicate) {
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

        LetterStatus letterStatus = letterRepository
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
            ))
            .orElseThrow(() -> new LetterNotFoundException(id));
        log.info("Returning  letter status for letter {}, letter id {}", letterStatus.status, id);
        return letterStatus;
    }

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

    private void exceptionCheck(UUID id) {
        if (exceptionLetterService.isException(id).isPresent()) {
            throw new LetterSaveException();
        }
    }

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

    static ZonedDateTime toDateTime(LocalDateTime dateTime) {
        if (null == dateTime) {
            return null;
        }

        return dateTime.atZone(ZoneId.of("UTC"));
    }
}
