package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

/**
 * The interface Letters count summary repository.
 */
public interface LettersCountSummaryRepository extends JpaRepository<Letter, UUID> {

    /**
     * Count by date.
     *
     * @param dateFrom the date from
     * @param dateTo   the date to
     * @return the list
     */
    @Query(
        nativeQuery = true,
        value = "SELECT COUNT(1) as uploaded, service FROM letters\n"
            + "  WHERE sent_to_print_at >= :dateFrom\n"
            + "    AND sent_to_print_at <=  :dateTo\n"
            + "  GROUP BY service \n"
            + "  ORDER BY service ASC"
    )
    List<ServiceLettersCountSummary> countByDate(
        @Param("dateFrom") LocalDateTime dateFrom,
        @Param("dateTo") LocalDateTime dateTo);
}
