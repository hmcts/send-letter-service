package uk.gov.hmcts.reform.sendletter.services.date.holidays.response;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDate;

/**
 * This class represents the event.
 */
public class Event {

    public final LocalDate date;
    public final String title;

    /**
     * Constructor.
     *
     * @param date the date
     * @param title the title
     */
    public Event(
        @JsonProperty("date") LocalDate date,
        @JsonProperty("title") String title
    ) {
        this.date = date;
        this.title = title;
    }
}
