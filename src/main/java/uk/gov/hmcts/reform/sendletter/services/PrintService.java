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
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import javax.transaction.Transactional;

import static java.util.stream.Collectors.toList;

@Service
public class PrintService {

    private final PrintRepository repository;
    private final ObjectMapper mapper;
    private final SasTokenGeneratorService sasTokenGeneratorService;

    @Autowired
    public PrintService(
        PrintRepository repository,
        ObjectMapper mapper,
        SasTokenGeneratorService sasTokenGeneratorService) {
        this.repository = repository;
        this.mapper = mapper;
        this.sasTokenGeneratorService = sasTokenGeneratorService;
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

        return getResponse(printSaved, service);
    }

    private PrintResponse getResponse(Print print, String service) {
        String containerName = sasTokenGeneratorService.getContainerName(service);
        return new PrintResponse(
           getPrintJob(print, containerName),
           getPrintUploadInfo(print, service, containerName)
        );
    }

    private PrintUploadInfo getPrintUploadInfo(Print print,
                                               String service,
                                               String containerName) {
        return new PrintUploadInfo(
            String.join("/",
                sasTokenGeneratorService.getAccountUrl(),
                containerName
            ),
            sasTokenGeneratorService.generateSasToken(service),
            String.format(
                "manifest-%s-%s.json",
                print.getId().toString(), print.getService()
            )
        );

    }

    private PrintJob getPrintJob(Print print, String containerName) {
        List<Document> documents = getDocuments(print);
        return new PrintJob(
            print.getId(),
            toDateTime(print.getCreatedAt()),
            toDateTime(print.getPrintedAt()),
            toDateTime(print.getSentToPrintAt()),
            print.getService(),
            print.getType(),
            containerName,
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

    static ZonedDateTime toDateTime(LocalDateTime dateTime) {
        return Optional.ofNullable(dateTime)
            .map(value -> value.atZone(ZoneId.of("UTC")))
            .orElse(null);
    }
}
