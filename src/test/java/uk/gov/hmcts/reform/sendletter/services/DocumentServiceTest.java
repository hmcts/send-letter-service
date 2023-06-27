package uk.gov.hmcts.reform.sendletter.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import uk.gov.hmcts.reform.sendletter.entity.DocumentRepository;
import uk.gov.hmcts.reform.sendletter.exception.DuplicateDocumentException;
import uk.gov.hmcts.reform.sendletter.model.in.Document;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Optional;
import java.util.UUID;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

class DocumentServiceTest {

    @Mock
    private DocumentRepository documentRepository;

    private DocumentService documentService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        documentService = new DocumentService(documentRepository, 1);
    }

    @Test
    void saveDocuments_should_save_multiple_documents() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        Document document2 = new Document("temp2", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.empty());

        //when
        documentService.saveDocuments(letterId, asList(document1, document2), "any");

        //then
        verify(documentRepository, times(2)).save(any(uk.gov.hmcts.reform.sendletter.entity.Document.class));
    }

    @Test
    void saveDocuments_should_save_single_document() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.empty());

        //when
        documentService.saveDocuments(letterId, singletonList(document1), "any");

        //then
        ArgumentCaptor<uk.gov.hmcts.reform.sendletter.entity.Document> documentArgumentCaptor =
            ArgumentCaptor.forClass(uk.gov.hmcts.reform.sendletter.entity.Document.class);
        verify(documentRepository).save(documentArgumentCaptor.capture());
        assertThat(documentArgumentCaptor.getValue().getLetterId()).isEqualTo(letterId);
        assertThat(documentArgumentCaptor.getValue().getChecksum()).isNotNull();
        assertThat(documentArgumentCaptor.getValue().getCreatedAt()).isNotNull();
        assertThat(documentArgumentCaptor.getValue().getId()).isNotNull();
    }

    @Test
    void saveDocuments_should_not_save_if_duplicated_document() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.of(mock(uk.gov.hmcts.reform.sendletter.entity.Document.class)));

        //when

        //then
        assertThatThrownBy(() ->
            documentService.saveDocuments(letterId, singletonList(document1), "any")
        ).isInstanceOf(DuplicateDocumentException.class);
    }

    @Test
    void saveDocuments_should_not_save_documents_if_empty_list() {
        //given
        UUID letterId = UUID.randomUUID();

        //when
        documentService.saveDocuments(letterId, emptyList(), "any");

        //then
        verifyNoInteractions(documentRepository);

    }

    @Test
    void checkDocumentDuplicates_should_check_multiple_documents() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        Document document2 = new Document("temp2", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.empty());

        //when
        documentService.checkDocumentDuplicates(asList(document1, document2), "any");

        //then
        verify(documentRepository, times(2)).findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class));
    }

    @Test
    void checkDocumentDuplicates_should_save_single_document() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.empty());

        //when
        documentService.checkDocumentDuplicates(singletonList(document1), "any");

        //then
        verify(documentRepository).findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class));
    }

    @Test
    void checkDocumentDuplicates_should_return_letter_id_if_duplicated() {
        //given
        UUID letterId = UUID.randomUUID();
        Document document1 = new Document("temp1", new HashMap<>());
        given(documentRepository.findOneCreatedAfter(anyString(), anyString(), any(LocalDateTime.class)))
            .willReturn(Optional.of(new uk.gov.hmcts.reform.sendletter.entity.Document(UUID.randomUUID(),
                letterId, "a checksum", "a rec checksum", LocalDateTime.now())));

        //then
        Optional<UUID> duplicateCheck = documentService.checkDocumentDuplicates(singletonList(document1), "any");
        assertThat(duplicateCheck.isPresent()).isTrue();
        assertThat(duplicateCheck.get()).isEqualTo(letterId);
    }

    @Test
    void checkDocumentDuplicates_should_not_save_documents_if_empty_list() {
        //given
        UUID letterId = UUID.randomUUID();

        //when
        documentService.saveDocuments(letterId, emptyList(), "any");

        //then
        verifyNoInteractions(documentRepository);

    }
}
