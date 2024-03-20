package uk.gov.hmcts.reform.sendletter.services.ftp;

import java.time.LocalTime;

/**
 * This interface represents the ftp availability checker.
 */
public interface IFtpAvailabilityChecker {

    boolean isFtpAvailable(LocalTime time);

    LocalTime getDowntimeStart();
}
