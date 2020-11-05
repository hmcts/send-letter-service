package uk.gov.hmcts.reform.sendletter.postgresext;

import org.testcontainers.containers.JdbcDatabaseContainer;
import org.testcontainers.containers.PostgreSQLContainerProvider;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.utility.TestcontainersConfiguration;

public class ExternalRegistryPostgreSqlContainerProvider extends PostgreSQLContainerProvider {
    private static final String NAME = "postgresqlext";

    @Override
    public boolean supports(String databaseType) {
        return NAME.equals(databaseType);
    }


    @Override
    public JdbcDatabaseContainer newInstance(String tag) {
        final String localDockerImage = (String) TestcontainersConfiguration.getInstance()
                .getProperties().get("local.docker.postgres.image");
        final DockerImageName dockerImageName = DockerImageName.parse(localDockerImage)
                .asCompatibleSubstituteFor("postgres");
        return new ExternalRegistryPostgreSqlContainer(dockerImageName);
    }
}

