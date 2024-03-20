package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.function.Consumer;

/**
 * Service for execution of tasks.
 */
@Service
public class ExecusionService {
    private static final Logger logger = LoggerFactory.getLogger(ExecusionService.class);

    /**
     * Run a task asynchronously.
     * @param runnable The task to run
     * @param infoLogger The info logger
     * @param duplicate The duplicate task
     * @param exception The exception
     */
    @Async(value = "AsyncExecutor")
    public void run(final Runnable runnable, final Runnable infoLogger,
                    final Execute duplicate, final Consumer<String> exception)  {
        try {
            infoLogger.run();
            runnable.run();
        } catch (DataIntegrityViolationException dataIntegrityViolationException) {
            logger.error("Async duplicate record ", dataIntegrityViolationException);
            duplicate.invoke();
        } catch (RuntimeException e) {
            logger.error("Async task error", e);
            exception.accept(e.getMessage());
        }
    }

    /**
     * Execute a task.
     * @param execute The task to execute
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void execute(final Execute execute) {
        execute.invoke();
    }

    /**
     * The execute interface.
     */
    public interface Execute {
        void invoke();
    }
}


