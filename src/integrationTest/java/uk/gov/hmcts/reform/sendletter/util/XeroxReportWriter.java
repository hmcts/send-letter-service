package uk.gov.hmcts.reform.sendletter.util;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.assertj.core.util.Lists;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class XeroxReportWriter {
    public static void writeReport(Stream<UUID> letterIds, File reportFolder) throws IOException {
        // We expect these fields in Xerox reports
        CSVFormat csvFileFormat = CSVFormat.EXCEL.withHeader(
            "Date",
            "Time",
            "Filename"
        );

        // Prepare a record for each of our letters.
        List<List<String>> records = letterIds.map(x -> Lists.newArrayList(
            "2018-01-01",
            "10:30:53",
            // Id is the third underscore separated part of filename.
            String.format("a_b_%s", x)
        )).collect(Collectors.toList());

        File report = new File(reportFolder, UUID.randomUUID() + ".csv");
        FileWriter fileWriter = new FileWriter(report);
        try (CSVPrinter printer = new CSVPrinter(fileWriter, csvFileFormat)) {
            printer.printRecords(records);
        }
    }

    // Prevent instantiation
    private XeroxReportWriter() {
    }
}
