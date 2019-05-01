package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

@ExtendWith(MockitoExtension.class)
class UploadLettersTaskTest {

    private UploadLettersTask task;

    @BeforeEach
    void setUp() {
        this.task = new UploadLettersTask();
    }

    @Test
    void should_handle_smoke_test_letters() {
        assertThatThrownBy(() -> task.run()).isNotNull();
    }
}
