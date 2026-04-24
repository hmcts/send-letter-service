package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersReport;

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

    /**
     * Get distinct service/international letters created within date range.
     *
     * @param dateFrom the date from
     * @param dateTo   the date to
     * @return the list
     */
    @Query(
        nativeQuery = true,
        value = "SELECT DISTINCT \n"
            + "  created_at::date as \"createdAt\", \n"
            + "  (CASE \n"
            + "    WHEN service = 'sscs' THEN (CASE \n"
            + "      WHEN additional_data->>'isIbca' = 'true' THEN 'sscs-ib' \n"
            + "        ELSE 'sscs-reform' END) \n"
            + "      ELSE service \n"
            + "  END) as service,\n"
            + "  COALESCE(CAST(additional_data->>'isInternational' AS boolean), false) as international\n"
            + " FROM letters \n"
            + " WHERE status IN "
            + "('Created', 'Uploaded', 'FailedToUpload', 'Posted', 'PostedLocally', 'Aborted', 'NotSent')\n"
            + "  AND type <> 'smoke_test'\n"
            + "  AND created_at >= :dateFrom AND created_at <= :dateTo\n"
            + " ORDER BY \"createdAt\", service, international"
    )
    List<ServiceLettersReport> getServiceLettersReport(
        @Param("dateFrom") LocalDateTime dateFrom,
        @Param("dateTo") LocalDateTime dateTo);
}
