package uk.gov.hmcts.reform.sendletter.data;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.io.Resources;
import org.apache.commons.io.Charsets;
import org.assertj.core.util.Lists;
import uk.gov.hmcts.reform.sendletter.model.in.Document;
import uk.gov.hmcts.reform.sendletter.model.in.Letter;
import uk.gov.hmcts.reform.slc.services.steps.getpdf.PdfCreator;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class TestDataFactory {
    // Create a test letter using a template from test resources.
    public static Letter getTestLetterRequest() {
        Map<String, Object> content = ImmutableMap.of("name", "John");
        List<Document> documents = Lists.newArrayList(
            new Document(readResource("template.html"),
                ImmutableMap.of("name", "John"))
        );
        return new Letter(documents, "a type", Maps.newHashMap());
    }

    private static String readResource(String name) {
        try {
            return Resources.toString(Resources.getResource("template.html"),
                StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
