package uk.gov.hmcts.reform.sendletter.postgresext;

import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.utility.DockerImageName;

public class ExternalRegistryPostgreSqlContainer
        <T extends ExternalRegistryPostgreSqlContainer<T>> extends PostgreSQLContainer<T> {

    public ExternalRegistryPostgreSqlContainer(final DockerImageName dockerImageName) {
        super(dockerImageName);
    }
}
