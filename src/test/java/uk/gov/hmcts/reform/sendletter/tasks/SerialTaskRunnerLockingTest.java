package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import javax.sql.DataSource;

import static org.mockito.Matchers.startsWith;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SerialTaskRunnerLockingTest {

    @Mock
    private DataSource source;
    @Mock
    private Connection connection;
    @Mock
    private Statement statement;
    private SerialTaskRunner taskRunner;

    @Before
    public void setup() throws SQLException {
        when(connection.createStatement()).thenReturn(statement);
        when(source.getConnection()).thenReturn(connection);

        taskRunner = SerialTaskRunner.get(source);
    }

    @Test
    public void successfully_locks_and_unlocks() throws SQLException {
        lockedWithSuccess();
        unlockedWithSuccess();

        taskRunner.tryRun(1, () -> {
        });
    }

    @Test
    public void successfully_locks_and_fails_to_unlock() throws SQLException {
        lockedWithSuccess();
        unlockedWithFailure();

        taskRunner.tryRun(1, () -> {
        });
    }

    @Test
    public void fails_to_lock_and_unlock() throws SQLException {
        lockedWithFailure();
        unlockedWithFailure();

        taskRunner.tryRun(1, () -> {
        });
    }

    @Test
    public void failure_to_release_lock() throws SQLException {
        lockedWithFailure();
        unlockedWithSuccess();

        taskRunner.tryRun(1, () -> {
        });
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void datasource_throws_connection_error() throws SQLException {
        when(source.getConnection()).thenThrow(SQLException.class);

        taskRunner.tryRun(1, () -> {
        });
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void locking_throws_exception_successfully_unlocks() throws SQLException {
        when(statement.executeQuery(startsWith("SELECT pg_try_advisory_lock"))).thenThrow(SQLException.class);

        taskRunner.tryRun(1, () -> {
        });
    }

    @SuppressWarnings("unchecked")
    @Test(expected = RuntimeException.class)
    public void task_throws_exception_successfully_unlocks() throws SQLException {
        lockedWithSuccess();
        unlockedWithSuccess();

        taskRunner.tryRun(1, () -> {
            throw new RuntimeException();
        });
    }

    @SuppressWarnings("unchecked")
    @Test(expected = RuntimeException.class)
    public void task_throws_exception_fails_to_unlock() throws SQLException {
        lockedWithSuccess();
        unlockedWithFailure();

        taskRunner.tryRun(1, () -> {
            throw new RuntimeException();
        });
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void executing_query_throws_exception() throws SQLException {
        when(connection.createStatement()).thenThrow(SQLException.class);
        unlockedWithFailure();

        taskRunner.tryRun(1, () -> {
        });
    }

    private void unlockedWithSuccess() throws SQLException {
        unlockWith(true);
    }

    private void unlockedWithFailure() throws SQLException {
        unlockWith(false);
    }

    private void lockedWithSuccess() throws SQLException {
        lockWith(true);
    }

    private void lockedWithFailure() throws SQLException {
        lockWith(false);
    }

    private void unlockWith(boolean value) throws SQLException {
        ResultSet resultSet = mock(ResultSet.class);
        when(resultSet.next()).thenReturn(value);
        when(resultSet.getBoolean(1)).thenReturn(value);
        when(statement.executeQuery(startsWith("SELECT pg_advisory_unlock"))).thenReturn(resultSet);
    }

    private void lockWith(boolean value) throws SQLException {
        ResultSet resultSet = mock(ResultSet.class);
        when(resultSet.next()).thenReturn(value);
        when(resultSet.getBoolean(1)).thenReturn(value);
        when(statement.executeQuery(startsWith("SELECT pg_try_advisory_lock"))).thenReturn(resultSet);
    }
}
