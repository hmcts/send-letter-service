package uk.gov.hmcts.reform.sendletter.services.date.holidays.response;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class Holidays {
    public final String division;
    public final List<Event> events;

    public Holidays(
        @JsonProperty("division") String division,
        @JsonProperty("events") List<Event> events
    ) {
        this.division = division;
        this.events = events;
    }
}
