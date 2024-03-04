package uk.gov.hmcts.reform.sendletter.model.in;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;

import java.io.Serializable;
import java.util.Map;

public class Document implements Serializable {

    private static final long serialVersionUID = -2374030102074056382L;

    @Schema(description = "Template to be used to render PDF", required = true)
    @NotEmpty
    public final String template;

    @Schema(description = "Template values for the PDF", required = true)
    @NotEmpty
    public final Map<String, Object> values;

    public Document(
        @JsonProperty("template") String template,
        @JsonProperty("values") Map<String, Object> values
    ) {
        this.template = template;
        this.values = values;
    }
}
