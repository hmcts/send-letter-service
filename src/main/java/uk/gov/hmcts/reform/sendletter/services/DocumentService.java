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

@Service
public class DocumentService {

    private static final Logger log = LoggerFactory.getLogger(DocumentService.class);

    private final DocumentRepository documentRepository;

    private final int cutOff;

    public DocumentService(
        DocumentRepository documentRepository,
        @Value("${documents.duplicate.cut-off-time:0}") int cutOff
    ) {
        this.documentRepository = documentRepository;
        this.cutOff = cutOff;
    }

    public Optional<UUID> checkDocumentDuplicates(List<?> documents, String recipientListChecksum) {
        for (Object document : documents) {
            String checkSum = LetterChecksumGenerator.generateChecksumForPdfPages(document);
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

    @Transactional
    public void saveDocuments(UUID letterId, List<?> documents, String recipientsChecksum) {
        log.info("Saving {} documents, letterId {}", documents.size(), letterId);
        documents.forEach((document) -> {
            UUID id = UUID.randomUUID();
            log.debug("Saving document, id {}, letterId {}", id, letterId);
            String checkSum = LetterChecksumGenerator.generateChecksumForPdfPages(document);
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
