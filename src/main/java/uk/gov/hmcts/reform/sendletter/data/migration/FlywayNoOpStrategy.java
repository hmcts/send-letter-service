package uk.gov.hmcts.reform.sendletter.data.migration;

import org.flywaydb.core.Flyway;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationStrategy;
import uk.gov.hmcts.reform.sendletter.exception.PendingMigrationScriptException;

import java.util.stream.Stream;

public class FlywayNoOpStrategy implements FlywayMigrationStrategy {

    /**
     * Migrate the database.
     * @param flyway The flyway instance
     */
    @Override
    public void migrate(Flyway flyway) {
        Stream.of(flyway.info().all())
            .filter(info -> !info.getState().isApplied())
            .findFirst()
            .ifPresent(info -> {
                throw new PendingMigrationScriptException(info.getScript());
            });
    }
}
