package uk.gov.hmcts.reform.sendletter.services.ftp;

import java.time.LocalTime;

/**
 * This class represents the ftp availability checker.
 */
public class FtpAvailabilityChecker implements IFtpAvailabilityChecker {

    private final LocalTime downtimeStart;
    private final LocalTime downtimeEnd;

    /**
     * Constructor.
     *
     * @param downtimeFromHour the downtime from hour
     * @param downtimeToHour the downtime to hour
     */
    public FtpAvailabilityChecker(String downtimeFromHour, String downtimeToHour) {
        this.downtimeStart = LocalTime.parse(downtimeFromHour);
        this.downtimeEnd = LocalTime.parse(downtimeToHour);
    }

    /**
     * Is ftp available.
     *
     * @param time the time
     * @return the boolean
     */
    public boolean isFtpAvailable(LocalTime time) {
        if (downtimeStart.isBefore(downtimeEnd)) {
            return time.isBefore(downtimeStart) || time.isAfter(downtimeEnd);
        } else {
            return time.isBefore(downtimeStart) && time.isAfter(downtimeEnd);
        }
    }

    /**
     * Gets downtime end.
     *
     * @return the downtime end
     */
    public LocalTime getDowntimeStart() {
        return downtimeStart;
    }
}
