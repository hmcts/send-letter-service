package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.jdbc.core.JdbcTemplate;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.gov.hmcts.reform.sendletter.launchdarkly.Flags.SEND_LETTER_SERVICE_DELETE_LETTERS_CRON;

class DeleteOldLettersTaskTest {

    @Mock
    private JdbcTemplate jdbcTemplate;

    @Mock
    private LaunchDarklyClient launchDarklyClient;

    private DeleteOldLettersTask deleteOldLettersTask;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        deleteOldLettersTask = new DeleteOldLettersTask(jdbcTemplate, launchDarklyClient);
    }

    @Test
    void shouldExecuteDeleteQueryWhenFeatureFlagIsEnabled() {
        // Given
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(true);
        when(jdbcTemplate.update(anyString())).thenReturn(100);

        // When
        deleteOldLettersTask.run();

        // Then
        verify(launchDarklyClient, times(1)).isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON);
        verify(jdbcTemplate, times(1)).update(anyString());
    }

    @Test
    void shouldNotExecuteDeleteQueryWhenFeatureFlagIsDisabled() {
        // Given
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(false);

        // When
        deleteOldLettersTask.run();

        // Then
        verify(launchDarklyClient, times(1)).isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON);
        verify(jdbcTemplate, never()).update(anyString());
    }

    @Test
    void shouldHandleDatabaseErrorGracefullyWhenFeatureFlagIsEnabled() {
        // Given
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(true);
        doThrow(new RuntimeException("Database error")).when(jdbcTemplate).update(anyString());

        // When
        deleteOldLettersTask.run();

        // Then
        verify(launchDarklyClient, times(1)).isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON);
        verify(jdbcTemplate, times(1)).update(anyString());
    }
}
