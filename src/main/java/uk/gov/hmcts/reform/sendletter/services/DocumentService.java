package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
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

    public DocumentService(DocumentRepository documentRepository) {
        this.documentRepository = documentRepository;
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void saveDocuments(UUID letterId, List<uk.gov.hmcts.reform.sendletter.model.in.Document> documents) {
        log.info("Saving {} documents, letterId {}", documents.size(), letterId);
        documents.forEach((document) -> {
            UUID id = UUID.randomUUID();
            log.info("Saving document, id {}, letterId {}", id, letterId);
            String checkSum = ChecksumGenerator.generateChecksum(document);
            Optional<Document> documentFound = documentRepository.findOneCreatedAfter(checkSum, now().minusHours(1));
            if (documentFound.isEmpty()) {
                Document documentToSave =
                    new Document(
                        id,
                        letterId,
                        checkSum,
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
