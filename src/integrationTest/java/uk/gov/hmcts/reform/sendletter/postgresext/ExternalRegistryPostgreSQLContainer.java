package uk.gov.hmcts.reform.sendletter.postgresext;

import org.apache.commons.lang3.StringUtils;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.utility.TestcontainersConfiguration;

public class ExternalRegistryPostgreSQLContainer<SELF extends ExternalRegistryPostgreSQLContainer<SELF>> extends PostgreSQLContainer<SELF> {

    public ExternalRegistryPostgreSQLContainer(final String dockerImageName) {
        super(getRegistryUrl() + dockerImageName);
    }

    private static String getRegistryUrl() {
        String registryUrl = (String) TestcontainersConfiguration.getInstance().getProperties().get("docker.registry.url");
        if (StringUtils.isEmpty(registryUrl)) {
            return StringUtils.EMPTY;
        }
        return StringUtils.appendIfMissing(registryUrl, "/");
    }
}
