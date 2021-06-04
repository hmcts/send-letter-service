package uk.gov.hmcts.reform.sendletter;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.io.Resources;
import uk.gov.hmcts.reform.sendletter.entity.Print;
import uk.gov.hmcts.reform.sendletter.model.LetterPrintStatus;
import uk.gov.hmcts.reform.sendletter.model.ParsedReport;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Supplier;

import static com.google.common.io.Resources.getResource;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.time.LocalDateTime.now;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;

public final class SampleData {
    private static final String ENCODED_PDF_FILE = "encodedPdfFile.txt";
    private static final String ENCODED_PDF_FILE2 = "encodedPdfFile2.txt";
    public static final Supplier<String> checkSumSupplier = () -> UUID.randomUUID().toString();
    private static ObjectMapper objectMapper = new ObjectMapper();

    public static LetterRequest letterRequest() {
        try {
            return new LetterRequest(
                singletonList(
                    new Document(
                        Resources.toString(getResource("template.html"), UTF_8),
                        ImmutableMap.of(
                            "name", "John",
                            "reference", UUID.randomUUID()
                        )
                    )
                ),
                "someType",
                Maps.newHashMap()
            );
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static LetterWithPdfsRequest letterWithPdfsRequest() {
        return new LetterWithPdfsRequest(
            singletonList(
                Base64.getEncoder().encode("hello world".getBytes())
            ),
            "someType",
            Maps.newHashMap()
        );
    }

    public static LetterWithPdfsAndNumberOfCopiesRequest letterWithPdfAndCopiesRequest(int copies1, int copies2)
            throws IOException {
        return new LetterWithPdfsAndNumberOfCopiesRequest(
            asList(
                new Doc(Base64.getDecoder().decode(Resources.toString(getResource(ENCODED_PDF_FILE),
                                Charsets.UTF_8)), copies1),
                new Doc(Base64.getDecoder().decode(Resources.toString(getResource(ENCODED_PDF_FILE2),
                            Charsets.UTF_8)), copies2)
            ),
            "some_type",
            Map.of("caseid",1111)
        );
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(String service) {
        return letterEntity(service, now());
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(String service, Supplier<String> checkSum) {
        return letterEntity(service, now(), "letterType1",null, Map.of("Document_1", 1), checkSum);
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(String service, LocalDateTime createdAt) {
        return letterEntity(service, createdAt, "letterType1", null, Map.of("Document_1", 1), checkSumSupplier);
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(
        String service,
        LocalDateTime createdAt,
        String type
    ) {
        return letterEntity(service, createdAt, type, null, Map.of("Document_1", 1), checkSumSupplier);
    }

    public static uk.gov.hmcts.reform.sendletter.entity.Letter letterEntity(
        String service,
        LocalDateTime createdAt,
        String type,
        String fingerprint,
        Map<String, Integer> copies,
        Supplier<String> checkSum
    ) {

        try {
            return new uk.gov.hmcts.reform.sendletter.entity.Letter(
                UUID.randomUUID(),
                checkSum.get(),
                service,
                objectMapper.readTree("{}"),
                type,
                new byte[1],
                false,
                fingerprint,
                createdAt,
                objectMapper.valueToTree(copies)
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

    public static LetterWithPdfsRequest letterWithPdfsRequestWithAdditionalData() throws IOException {
        return new LetterWithPdfsRequest(
                singletonList(
                        Base64.getDecoder().decode(Resources.toString(getResource(ENCODED_PDF_FILE),
                                Charsets.UTF_8))), "someType",
                Map.of("reference", "ABD-123-WAZ",
                        "count", 10,
                        "additionInfo", "present")
        );
    }

    public static LetterWithPdfsRequest letterWithPdfsRequestWithNoAdditionalData() throws IOException {
        return new LetterWithPdfsRequest(
                singletonList(
                        Base64.getDecoder().decode(Resources.toString(getResource(ENCODED_PDF_FILE),
                                Charsets.UTF_8))
                ),
                "someType",
                null
        );
    }

    private SampleData() {
    }

    public static Print printEntity(
        UUID id,
        String service,
        LocalDateTime createdAt,
        String type,
        String idempotencyKey,
        JsonNode documents,
        String caseId,
        String caseRef,
        String letterType
    ) {
        return new Print(
            id,
            service,
            createdAt,
            type,
            idempotencyKey,
            documents,
            caseId,
            caseRef,
            letterType
        );
    }
}
