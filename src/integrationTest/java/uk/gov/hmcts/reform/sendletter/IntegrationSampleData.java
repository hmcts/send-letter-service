package uk.gov.hmcts.reform.sendletter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.gov.hmcts.reform.sendletter.model.in.LetterWithPdfsRequest;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Base64;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static java.util.Collections.singletonList;

public final class IntegrationSampleData {

    private static final String ENCODED_PDF_BYTES = "encoded.pdf.bytes";
    private static Properties appProps = new Properties();
    private static final Logger logger = LoggerFactory.getLogger(IntegrationSampleData.class);

    static {
        String rootPath = Optional.ofNullable(Thread.currentThread().getContextClassLoader().getResource(""))
                .orElseThrow().getPath();
        String appConfigPath = rootPath + "/../../../resources/integrationTest/application.properties";
        try {
            appProps.load(new FileInputStream(appConfigPath));
        } catch (IOException e) {
            logger.error("Error loading the application.properties", e);
        }
    }

    private IntegrationSampleData() {
    }

    public static LetterWithPdfsRequest letterWithPdfsRequestWithAdditionalData() {
        return new LetterWithPdfsRequest(
                singletonList(
                        Base64.getDecoder().decode(appProps.getProperty(ENCODED_PDF_BYTES))
                ),
                "someType",
                Map.of("reference", "ABD-123-WAZ",
                        "count", 10,
                        "additionInfo", "present")
        );
    }

    public static LetterWithPdfsRequest letterWithPdfsRequestWithNoAdditionalData() {
        return new LetterWithPdfsRequest(
                singletonList(
                        Base64.getDecoder().decode(appProps.getProperty(ENCODED_PDF_BYTES))
                ),
                "someType",
                null
        );
    }
}
