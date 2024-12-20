package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.jdbc.core.JdbcTemplate;
import uk.gov.hmcts.reform.sendletter.launchdarkly.LaunchDarklyClient;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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
    void testRunWhenFeatureFlagIsEnabled() {
        // Arrange: Mock LaunchDarkly feature flag to return true
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(true);

        // Mock JdbcTemplate behavior for the deletion loop
        when(jdbcTemplate.queryForObject(anyString(), any(Object[].class), eq(Integer.class)))
            .thenReturn(100)
            .thenReturn(50)
            .thenReturn(0); // Simulate 3 batches with 100, 50 rows deleted, and then no more rows

        // Act: Call the run method
        deleteOldLettersTask.run();

        // Assert: Check if the correct number of deletions were made
        verify(jdbcTemplate, times(3)).queryForObject(anyString(), any(Object[].class), eq(Integer.class));
    }

    @Test
    void testRunWhenFeatureFlagIsDisabled() {
        // Arrange: Mock LaunchDarkly feature flag to return false
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(false);

        // Act: Call the run method
        deleteOldLettersTask.run();

        // Assert: Verify no deletion attempt is made
        verify(jdbcTemplate, never()).queryForObject(anyString(), any(Object[].class), eq(Integer.class));
    }

    @Test
    void testRunWhenErrorOccurs() {
        // Arrange: Mock LaunchDarkly feature flag to return true
        when(launchDarklyClient.isFeatureEnabled(SEND_LETTER_SERVICE_DELETE_LETTERS_CRON)).thenReturn(true);

        // Mock JdbcTemplate to throw an exception
        when(jdbcTemplate.queryForObject(anyString(), any(Object[].class), eq(Integer.class)))
            .thenThrow(new RuntimeException("Database error"));

        // Act: Call the run method
        deleteOldLettersTask.run();

    }
}
