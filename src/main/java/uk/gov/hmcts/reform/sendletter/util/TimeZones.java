package uk.gov.hmcts.reform.sendletter.util;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

/**
 * Utility class for time zones.
 */
public final class TimeZones {

    public static final String EUROPE_LONDON = "Europe/London";
    public static final String UTC = "UTC";

    /**
     * Utility class constructor.
     */
    private TimeZones() {
    }

    /**
     * Returns the current local date time in UTC.
     * @param date the date
     * @param time the time
     * @return the current date time in UTC
     */
    public static LocalDateTime localDateTimeWithUtc(LocalDate date, LocalTime time) {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.from(ZoneOffset.UTC));
        return zonedDateTime.toLocalDateTime();
    }

    /**
     * Get the current date time in UTC in EUROPE_LONDON.
     *
     * @return the current date time in UTC
     */
    public static Instant getCurrentEuropeLondonInstant() {
        return LocalDateTime
            .now()
            .atZone(ZoneId.of(EUROPE_LONDON))
            .toInstant();
    }
}
