package uk.gov.hmcts.reform.sendletter.services;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.model.in.Doc;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.model.in.LetterRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsAndNumberOfCopiesRequest;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;

import java.util.function.Supplier;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

class LetterChecksumServiceTest {

    private final LetterChecksumService letterChecksumService = new LetterChecksumService();

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_document_objects_version_3() throws Exception {

        final LetterWithPdfsAndNumberOfCopiesRequest letterWithPdfsAndNumberOfCopiesRequest =
            SampleData.letterWithPdfAndCopiesRequest(5, 10);

        Doc docOne = letterWithPdfsAndNumberOfCopiesRequest.documents.get(0);
        Doc docTwo = letterWithPdfsAndNumberOfCopiesRequest.documents.get(1);

        // Simply send in the same documents through the check twice and assert they match
        // Check that the parent method calls the calculateContentChecksum as well
        assertThat(letterChecksumService.generateChecksumForPdfPages(docOne))
            .isEqualTo(letterChecksumService.calculateContentChecksum(docOne.content));
        assertThat(letterChecksumService.generateChecksumForPdfPages(docTwo))
            .isEqualTo(letterChecksumService.calculateContentChecksum(docTwo.content));
    }

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_objects() {

        Supplier<LetterRequest> letterSupplier =
            () -> new LetterRequest(
                singletonList(
                    new Document(
                        "cmc-template",
                        ImmutableMap.of(
                            "key11", "value11",
                            "key21", "value21"
                        )
                    )
                ),
                "print-job-1234",
                ImmutableMap.of(
                    "doc_type", "my doc type",
                    "caseId", "123"
                )
            );

        LetterRequest letter1 = letterSupplier.get();
        LetterRequest letter2 = letterSupplier.get();

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_same_md5_checksum_hex_for_same_letter_with_pdfs_objects() {

        Supplier<LetterWithPdfsRequest> letterSupplier =
            () -> new LetterWithPdfsRequest(
                asList(
                    "foo".getBytes(),
                    "bar".getBytes()
                ),
                "print-job-1234",
                ImmutableMap.of(
                    "doc_type", "my doc type",
                    "caseId", "123"
                )
            );

        LetterWithPdfsRequest letter1 = letterSupplier.get();
        LetterWithPdfsRequest letter2 = letterSupplier.get();

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_different_md5_checksum_hex_for_different_letter_objects() {

        LetterRequest letter1 = new LetterRequest(
            singletonList(new Document(
                "cmc-template",
                ImmutableMap.of(
                    "key11", "value11",
                    "key12", "value12"
                )
            )),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        LetterRequest letter2 = new LetterRequest(
            singletonList(new Document(
                "cmc-template",
                ImmutableMap.of(
                    "key21", "key21",
                    "key22", "value22"
                )
            )),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isNotEqualTo(letterChecksumService.generateChecksum(letter2));
    }

    @Test
    void should_return_different_md5_checksum_hex_for_different_letter_with_pdf_objects() {

        LetterWithPdfsRequest letter1 = new LetterWithPdfsRequest(
            asList(
                "foo".getBytes(),
                "bar".getBytes()
            ),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        LetterWithPdfsRequest letter2 = new LetterWithPdfsRequest(
            asList(
                "foo".getBytes(),
                "bar!".getBytes()
            ),
            "print-job-1234",
            ImmutableMap.of(
                "doc_type", "my doc type",
                "caseId", "123"
            )
        );

        assertThat(letterChecksumService.generateChecksum(letter1))
            .isNotEqualTo(letterChecksumService.generateChecksum(letter2));
    }
}
