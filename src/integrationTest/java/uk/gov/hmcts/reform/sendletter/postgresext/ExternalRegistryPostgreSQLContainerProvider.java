package uk.gov.hmcts.reform.sendletter.postgresext;

import org.testcontainers.containers.JdbcDatabaseContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.PostgreSQLContainerProvider;

public class ExternalRegistryPostgreSQLContainerProvider extends PostgreSQLContainerProvider {

    private static final String NAME = "postgresqlext";

    @Override
    public boolean supports(String databaseType) {
        return NAME.equals(databaseType);
    }

    @Override
    public JdbcDatabaseContainer newInstance(String tag) {
        return new ExternalRegistryPostgreSQLContainer(PostgreSQLContainer.IMAGE + ":" + tag);
    }
}

