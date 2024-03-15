package uk.gov.hmcts.reform.sendletter.services.date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.services.date.holidays.BankHolidaysClient;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.List;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * This class represents the date calculator.
 */
@Service
public class DateCalculator {

    private static final Logger log = LoggerFactory.getLogger(DateCalculator.class);

    private final BankHolidaysClient bankHolidaysClient;

    /**
     * Constructor.
     *
     * @param bankHolidaysClient the bank holidays client
     */
    public DateCalculator(BankHolidaysClient bankHolidaysClient) {
        this.bankHolidaysClient = bankHolidaysClient;
    }

    /**
     * Subtract business days from a date time.
     *
     * @param dateTime the date time
     * @param numberOfBusinessDays the number of business days
     * @return the adjusted date time
     */
    public ZonedDateTime subtractBusinessDays(ZonedDateTime dateTime, int numberOfBusinessDays) {
        int daysSubtracted = 0;
        ZonedDateTime adjustedDateTime = dateTime;

        List<LocalDate> bankHolidays = getBankHolidayDates();

        while (daysSubtracted < numberOfBusinessDays) {
            adjustedDateTime = adjustedDateTime.minusDays(1);
            if (isBusinessDay(adjustedDateTime, bankHolidays)) {
                daysSubtracted++;
            }
        }

        return adjustedDateTime;
    }

    /**
     * Check if a date time is a business day.
     * @param dateTime the date time
     * @param bankHolidays the bank holidays
     */
    private boolean isBusinessDay(ZonedDateTime dateTime, List<LocalDate> bankHolidays) {
        return dateTime.getDayOfWeek().getValue() < DayOfWeek.SATURDAY.getValue()
            && !bankHolidays.contains(dateTime.toLocalDate());
    }

    /**
     * Get the bank holiday dates.
     */
    private List<LocalDate> getBankHolidayDates() {
        try {
            return this.bankHolidaysClient
                .getHolidays()
                .events
                .stream()
                .map(event -> event.date)
                .collect(toList());
        } catch (Exception exc) {
            log.error("Error fetching bank holidays", exc);
            return emptyList();
        }
    }
}
