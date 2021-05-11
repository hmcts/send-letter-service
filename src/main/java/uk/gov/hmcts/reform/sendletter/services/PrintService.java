package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.model.Document;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintJob;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;
import uk.gov.hmcts.reform.sendletter.model.out.PrintUploadInfo;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import javax.transaction.Transactional;

import static java.util.stream.Collectors.toList;

@Service
public class PrintService {

    private final PrintRepository repository;
    private final ObjectMapper mapper;

    @Autowired
    public PrintService(PrintRepository repository, ObjectMapper mapper) {
        this.repository = repository;
        this.mapper = mapper;
    }

    @Transactional
    public PrintResponse save(String id, String service, PrintRequest request, String idempotencyKey) {
        var printRequest = new Print(
            UUID.fromString(id),
            service,
            LocalDateTime.now(),
            request.type,
            idempotencyKey,
            mapper.valueToTree(request.documents),
            request.caseId,
            request.caseRef,
            request.letterType
        );
        Print printSaved = repository.save(printRequest);

        return getResponse(printSaved);
    }

    private PrintResponse getResponse(Print print) {
        return new PrintResponse(
           getPrintJob(print),
           getPrintUploadInfo(print)
        );
    }

    private PrintUploadInfo getPrintUploadInfo(Print print) {
        String manifest = String.join("-",
            "manifest",
            print.getId().toString(),
            print.getService());

        return new PrintUploadInfo(
            null,
            null,
            String.join(".",
                manifest,
                "json"
            )
        );

    }

    private PrintJob getPrintJob(Print print) {
        List<Document> documents = getDocuments(print);
        return new PrintJob(
            print.getId(),
            print.getCreatedAt(),
            print.getPrintedAt(),
            print.getSentToPrintAt(),
            print.getService(),
            print.getStatus(),
            documents,
            print.getCaseId(),
            print.getCaseRef(),
            print.getLetterType()
        );
    }

    private List<Document> getDocuments(final Print print) {
        List<Document> documents = mapper.convertValue(
            print.getDocuments(),
            new TypeReference<>() {
            }
        );

        return documents.stream()
            .map(document -> new Document(
                document.fileName,
                String.join("-",
                    print.getId().toString(),
                    print.getService(),
                    print.getType(),
                    document.fileName
                ),
                document.copies
            ))
            .collect(toList());
    }
}
