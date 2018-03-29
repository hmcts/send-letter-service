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
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.only;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SerialTaskRunnerLockingTest {

    @Mock
    private DataSource source;
    @Mock
    private Connection connection;
    @Mock
    private Statement statement;
    @Mock
    private Runnable task;
    private SerialTaskRunner taskRunner;

    @Before
    public void setup() throws SQLException {
        when(connection.createStatement()).thenReturn(statement);
        when(source.getConnection()).thenReturn(connection);

        taskRunner = SerialTaskRunner.get(source);
    }

    @Test
    public void runs_task_after_successful_locking() throws SQLException {
        lockedWithSuccess();
        unlockedWithSuccess();

        taskRunner.tryRun(1, task);

        verify(task, only()).run();
    }

    @Test
    public void runs_task_after_failed_unlocking() throws SQLException {
        lockedWithSuccess();
        unlockedWithFailure();

        taskRunner.tryRun(1, task);

        verify(task, only()).run();
    }

    @Test
    public void does_not_run_task_after_failed_locking() throws SQLException {
        lockedWithFailure();
        unlockedWithFailure();

        taskRunner.tryRun(1, task);

        verify(task, never()).run();
    }

    @Test
    public void does_not_run_task_after_failed_locking_and_failed_unlocking() throws SQLException {
        lockedWithFailure();
        unlockedWithSuccess();

        taskRunner.tryRun(1, task);

        verify(task, never()).run();
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void does_not_run_task_when_obtaining_database_connection_fails() throws SQLException {
        when(source.getConnection()).thenThrow(SQLException.class);

        taskRunner.tryRun(1, task);

        verify(task, never()).run();
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void does_not_run_task_when_locking_throws_exception() throws SQLException {
        when(statement.executeQuery(startsWith("SELECT pg_try_advisory_lock"))).thenThrow(SQLException.class);

        taskRunner.tryRun(1, task);

        verify(task, never()).run();
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void runs_task_when_unlocking_throws_exception() throws SQLException {
        lockedWithSuccess();
        when(statement.executeQuery(startsWith("SELECT pg_advisory_unlock"))).thenThrow(SQLException.class);

        taskRunner.tryRun(1, task);

        verify(task, only()).run();
    }

    @SuppressWarnings("unchecked")
    @Test(expected = RuntimeException.class)
    public void throws_exception_threw_by_task() throws SQLException {
        lockedWithSuccess();
        unlockedWithSuccess();
        doThrow(RuntimeException.class).when(task).run();

        taskRunner.tryRun(1, task);
    }

    @SuppressWarnings("unchecked")
    @Test(expected = RuntimeException.class)
    public void throws_exception_threw_by_task_when_unlocking_failed() throws SQLException {
        lockedWithSuccess();
        unlockedWithFailure();
        doThrow(RuntimeException.class).when(task).run();

        taskRunner.tryRun(1, task);
    }

    @SuppressWarnings("unchecked")
    @Test(expected = SQLException.class)
    public void throws_exception_when_fails_to_prepare_db_query() throws SQLException {
        when(connection.createStatement()).thenThrow(SQLException.class);
        unlockedWithFailure();

        taskRunner.tryRun(1, task);
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
