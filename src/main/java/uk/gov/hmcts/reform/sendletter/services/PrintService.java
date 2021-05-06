package uk.gov.hmcts.reform.sendletter.services;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.entity.PrintRepository;
import uk.gov.hmcts.reform.sendletter.model.in.PrintRequest;
import uk.gov.hmcts.reform.sendletter.model.out.PrintResponse;

import java.time.LocalDateTime;
import java.util.UUID;
import javax.transaction.Transactional;

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
    public PrintResponse save(String service, PrintRequest request, String idempotencyKey) {
        var uuid = UUID.randomUUID();
        var print = new Print(
            uuid,
            service,
            LocalDateTime.now(),
            request.type,
            idempotencyKey,
            mapper.valueToTree(request.documents),
            request.caseId,
            request.caseRef,
            request.letterType
        );
        repository.save(print);

        return null;
    }
}
