package uk.gov.hmcts.reform.sendletter.entity;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import uk.gov.hmcts.reform.sendletter.entity.reports.ServiceLettersCountSummary;

import java.util.Date;
import java.util.List;
import java.util.UUID;

public interface LettersCountSummaryRepository extends JpaRepository<Letter, UUID> {

    @Query(
        nativeQuery = true,
        value = "SELECT COUNT(1) as uploaded, service FROM letters\n"
            + "  WHERE \n"
            + "     sent_to_print_at >= :dateFrom\n"
            + "         AND \n"
            + "     sent_to_print_at <=  :dateTo\n"
            + "  GROUP BY service \n"
            + "  ORDER BY service ASC"
    )
    List<ServiceLettersCountSummary> countByDate(
        @Param("dateFrom") Date dateFrom,
        @Param("dateTo") Date dateTo);
}
