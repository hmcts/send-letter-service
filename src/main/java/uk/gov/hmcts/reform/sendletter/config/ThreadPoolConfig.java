package uk.gov.hmcts.reform.sendletter.config;

import com.microsoft.applicationinsights.web.internal.RequestTelemetryContext;
import com.microsoft.applicationinsights.web.internal.ThreadContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.Trigger;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.SchedulingConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.config.ScheduledTaskRegistrar;

import java.util.Date;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;

import static uk.gov.hmcts.reform.sendletter.util.TimeZones.getCurrentEuropeLondonInstant;

/**
 * Adds custom error handler to Scheduled Tasks. Followed suggestions by Microsoft.
 * {@see https://github.com/Microsoft/ApplicationInsights-Java/wiki/Distributed-Tracing-in-Asynchronous-Java-Applications#context-propagation-in-scheduled-events}
 */
@Configuration
@EnableScheduling
public class ThreadPoolConfig implements SchedulingConfigurer {

    private static AtomicInteger errorCount = new AtomicInteger(0);
    private static final Logger log = LoggerFactory.getLogger(ThreadPoolConfig.class);

    private static final Supplier<Long> CURRENT_MILLIS_SUPPLIER = () -> getCurrentEuropeLondonInstant().toEpochMilli();

    /**
     * Supplier for the request context.
     */
    private static final Supplier<RequestTelemetryContext> REQUEST_CONTEXT_SUPPLIER = () ->
        new RequestTelemetryContext(CURRENT_MILLIS_SUPPLIER.get(), null);

    /**
     * Configure the scheduled tasks.
     * @param taskRegistrar The task registrar
     */
    @Override
    public void configureTasks(ScheduledTaskRegistrar taskRegistrar) {
        taskRegistrar.setTaskScheduler(sendLetterTaskScheduler());
    }

    /**
     * Create a TaskScheduler.
     * @return The TaskScheduler
     */
    @Bean
    public TaskScheduler sendLetterTaskScheduler() {
        ThreadPoolTaskScheduler taskScheduler = new SendLetterTaskScheduler();
        taskScheduler.setThreadNamePrefix("SendLetterTask-");
        taskScheduler.setErrorHandler(t -> {
            log.error("Unhandled exception during task. {}: {}", t.getClass(), t.getMessage(), t);
            errorCount.incrementAndGet();
        });
        taskScheduler.initialize();

        return taskScheduler;
    }

    /**
     * Custom {@link ThreadPoolTaskScheduler} to be able to register scheduled tasks via AppInsights.
     */
    private static class SendLetterTaskScheduler extends ThreadPoolTaskScheduler {

        /**
         * Execute a command.
         * @param command The command
         */
        @Override
        public void execute(Runnable command) {
            super.execute(new WrappedRunnable(command, REQUEST_CONTEXT_SUPPLIER.get()));
        }

        /**
         * Schedule a task.
         * @param task The task
         * @param trigger The trigger
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> schedule(Runnable task, Trigger trigger) {
            return super.schedule(new WrappedRunnable(task, REQUEST_CONTEXT_SUPPLIER.get()), trigger);
        }

        /**
         * Schedule a task.
         * @param task The task
         * @param startTime The start time
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> schedule(Runnable task, Date startTime) {
            return super.schedule(new WrappedRunnable(task, REQUEST_CONTEXT_SUPPLIER.get()), startTime);
        }

        /**
         * Schedule a task at fixed rate.
         * @param task The task
         * @param startTime The start time
         * @param period The period
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> scheduleAtFixedRate(Runnable task, Date startTime, long period) {
            return super.scheduleAtFixedRate(new WrappedRunnable(
                task,
                REQUEST_CONTEXT_SUPPLIER.get()
            ), startTime, period);
        }

        /**
         * Schedule a task at fixed rate.
         * @param task The task
         * @param period The period
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> scheduleAtFixedRate(Runnable task, long period) {
            return super.scheduleAtFixedRate(new WrappedRunnable(task, REQUEST_CONTEXT_SUPPLIER.get()), period);
        }

        /**
         * Schedule a task with fixed delay.
         * @param task The task
         * @param startTime The start time
         * @param delay The delay
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> scheduleWithFixedDelay(Runnable task, Date startTime, long delay) {
            return super.scheduleWithFixedDelay(new WrappedRunnable(
                task,
                REQUEST_CONTEXT_SUPPLIER.get()
            ), startTime, delay);
        }

        /**
         * Schedule a task with fixed delay.
         * @param task The task
         * @param delay The delay
         * @return The ScheduledFuture
         */
        @Override
        public ScheduledFuture<?> scheduleWithFixedDelay(Runnable task, long delay) {
            return super.scheduleWithFixedDelay(new WrappedRunnable(task, REQUEST_CONTEXT_SUPPLIER.get()), delay);
        }
    }

    private static class WrappedRunnable implements Runnable {

        private final Runnable task;
        private RequestTelemetryContext requestContext;

        /**
         * Constructor for the WrappedRunnable.
         * @param task The task
         * @param requestContext The request context
         */
        WrappedRunnable(Runnable task, RequestTelemetryContext requestContext) {
            this.task = task;
            this.requestContext = requestContext;
        }

        /**
         * Run the task.
         */
        @Override
        public void run() {
            if (ThreadContext.getRequestTelemetryContext() != null) {
                ThreadContext.remove();

                // since this runnable is ran on schedule, update the context on every run
                requestContext = REQUEST_CONTEXT_SUPPLIER.get();
            }

            ThreadContext.setRequestTelemetryContext(requestContext);

            task.run();
        }
    }
}

