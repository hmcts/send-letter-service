package uk.gov.hmcts.reform.sendletter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Maps;
import uk.gov.hmcts.reform.sendletter.model.LetterPrintStatus;
import uk.gov.hmcts.reform.sendletter.model.ParsedReport;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Base64;
import java.util.List;
import java.util.UUID;

import static java.time.LocalDateTime.now;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public final class SampleData {

    public static LetterWithPdfsRequest letterWithPdfsRequest() {
        return new LetterWithPdfsRequest(
            singletonList(
                Base64.getEncoder().encode("hello world".getBytes())
            ),
            "someType",
            Maps.newHashMap()
        );
    }

    public static LetterWithPdfsAndNumberOfCopiesRequest letterWithPdfAndCopiesRequest() {
        return new LetterWithPdfsAndNumberOfCopiesRequest(
            asList(
                new Doc(
                    Base64.getEncoder().encode("hello".getBytes()),
                    1
                ),
                new Doc(
                    Base64.getEncoder().encode("world".getBytes()),
                    10
                )
            ),
            "some_type",
            Maps.newHashMap()
        );
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(String service) {
        return letterEntity(service, now());
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(String service, LocalDateTime createdAt) {
        return letterEntity(service, createdAt, "letterType1", null);
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(
        String service,
        LocalDateTime createdAt,
        String type
    ) {
        return letterEntity(service, createdAt, type, null);
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(
        String service,
        LocalDateTime createdAt,
        String type,
        String fingerprint
    ) {
        try {
            return new uk.gov.hmcts.reform.sendletter.entity.Letter(
                UUID.randomUUID(),
                "messageId",
                service,
                new ObjectMapper().readTree("{}"),
                type,
                new byte[1],
                false,
                fingerprint,
                createdAt
            );
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static ParsedReport parsedReport(String filename, List<UUID> letterIds, boolean allParsed) {
        return new ParsedReport(
            filename,
            letterIds
                .stream()
                .map(id -> new LetterPrintStatus(id, ZonedDateTime.now()))
                .collect(toList()),
            allParsed
        );
    }

    public static ParsedReport parsedReport(String filename, boolean allParsed) {
        return parsedReport(filename, asList(UUID.randomUUID(), UUID.randomUUID()), allParsed);
    }

    private SampleData() {
    }
}
