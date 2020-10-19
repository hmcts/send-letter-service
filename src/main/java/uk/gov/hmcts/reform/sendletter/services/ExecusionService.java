package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ExecusionService {
    private static final Logger logger = LoggerFactory.getLogger(ExecusionService.class);

    @Async
    public void run(final Runnable runnable,  final Execute execute)  {
        try {
            runnable.run();
        } catch (DataIntegrityViolationException dataIntegrityViolationException) {
            logger.error("Async duplicate record ", dataIntegrityViolationException);
            execute.invoke();
        } catch (RuntimeException e) {
            logger.error("Async task error", e);
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void execute(final Execute execute) {
        execute.invoke();
    }

    public interface Execute {
        void invoke();
    }
}


