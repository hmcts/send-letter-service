package uk.gov.hmcts.reform.sendletter.tasks;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import javax.sql.DataSource;

/**
 * Runs tasks in serial using Database locks
 * to ensure the same task is not run concurrently.
 */
public final class SerialTaskRunner {

    private final DataSource source;

    public static SerialTaskRunner get(DataSource source) {
        return new SerialTaskRunner(source);
    }

    private SerialTaskRunner(DataSource source) {
        this.source = source;
    }

    /**
     * Run the task with specified ID if no other task with matching
     * id is already running.
     * No error is thrown if such a task is already running, the
     * supplied Runnable is simply not executed.
     */
    public void tryRun(long id, Runnable runnable) throws SQLException {
        try (Connection connection = source.getConnection()) {
            boolean locked = false;
            try {
                if (tryLock(id, connection)) {
                    locked = true;
                    runnable.run();
                }
            } finally {
                if (locked) {
                    unlock(id, connection);
                }
            }
        }
    }

    private boolean tryLock(long id, Connection connection) throws SQLException {
        String sql = String.format("SELECT pg_try_advisory_lock(%d);", id);
        return executeReturningBool(connection, sql);
    }

    private void unlock(long id, Connection connection) throws SQLException {
        String sql = String.format("SELECT pg_advisory_unlock(%d);", id);
        executeReturningBool(connection, sql);
    }

    /**
     * Execute a SQL statement and interpret the result set
     * as 1 row 1 column single boolean value.
     */
    private boolean executeReturningBool(Connection connection, String sql) throws SQLException {
        try (ResultSet set = connection.createStatement().executeQuery(sql)) {
            if (set.next()) {
                return set.getBoolean(1);
            }
            return false;
        }
    }
}
