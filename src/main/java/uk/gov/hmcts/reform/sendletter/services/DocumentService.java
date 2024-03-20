package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.gov.hmcts.reform.sendletter.entity.Document;
import uk.gov.hmcts.reform.sendletter.entity.DocumentRepository;
import uk.gov.hmcts.reform.sendletter.exception.DuplicateDocumentException;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static java.time.LocalDateTime.now;

/**
 * Service for document data.
 */
@Service
public class DocumentService {

    private static final Logger log = LoggerFactory.getLogger(DocumentService.class);

    private final DocumentRepository documentRepository;
    private final LetterChecksumService letterChecksumService;

    private final int cutOff;

    /**
     * Constructor for the DocumentService.
     * @param documentRepository The repository for document
     * @param cutOff The cut off
     * @param letterChecksumService The service for letter checksum
     */
    public DocumentService(
        DocumentRepository documentRepository,
        @Value("${documents.duplicate.cut-off-time:0}") int cutOff,
        LetterChecksumService letterChecksumService
    ) {
        this.documentRepository = documentRepository;
        this.cutOff = cutOff;
        this.letterChecksumService = letterChecksumService;
    }

    /**
     * Check for duplicate documents.
     * @param documents The documents
     * @param recipientListChecksum The recipient list checksum
     * @return The letter id
     */
    public Optional<UUID> checkDocumentDuplicates(List<?> documents, String recipientListChecksum) {
        for (Object document : documents) {
            String checkSum = letterChecksumService.generateChecksumForPdfPages(document);
            Optional<Document> documentFound = documentRepository.findOneCreatedAfter(
                checkSum,
                recipientListChecksum,
                now().minusHours(cutOff)
            );
            if (documentFound.isPresent()) {
                String msg = String.format(
                    "Duplicate document found, id %s, checkSum %s, recipientsChecksum %s. Returning letterId: %s",
                    documentFound.get().getId(),
                    checkSum,
                    recipientListChecksum,
                    documentFound.get().getLetterId()
                );
                log.warn(msg);
                return Optional.of(documentFound.get().getLetterId());
            }
        }
        return Optional.empty();
    }

    /**
     * Save documents.
     * @param letterId The letter id
     * @param documents The documents
     * @param recipientsChecksum The recipients checksum
     */
    @Transactional
    public void saveDocuments(UUID letterId, List<?> documents, String recipientsChecksum) {
        log.info("Saving {} documents, letterId {}", documents.size(), letterId);
        documents.forEach((document) -> {
            UUID id = UUID.randomUUID();
            log.debug("Saving document, id {}, letterId {}", id, letterId);
            String checkSum = letterChecksumService.generateChecksumForPdfPages(document);
            Optional<Document> documentFound = documentRepository.findOneCreatedAfter(
                checkSum,
                recipientsChecksum,
                now().minusHours(cutOff)
            );
            if (documentFound.isEmpty()) {
                Document documentToSave =
                    new Document(
                        id,
                        letterId,
                        checkSum,
                        recipientsChecksum,
                        now()
                    );
                documentRepository.save(documentToSave);
            } else {
                String msg = String.format(
                    "Duplicate document found, id %s, checkSum %s, letterId %s",
                    documentFound.get().getId(),
                    checkSum,
                    letterId
                );
                log.error(msg);
                throw new DuplicateDocumentException(msg);
            }
        });
    }
}
