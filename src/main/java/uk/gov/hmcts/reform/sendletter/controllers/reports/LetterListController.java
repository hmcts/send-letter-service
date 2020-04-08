package uk.gov.hmcts.reform.sendletter.controllers.reports;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.gov.hmcts.reform.sendletter.model.out.LettersInfoResponse;
import uk.gov.hmcts.reform.sendletter.services.LetterInfoService;

import java.time.LocalDate;

import static org.springframework.format.annotation.DateTimeFormat.ISO.DATE;

@RestController
public class LetterListController {

    private final LetterInfoService service;

    public LetterListController(LetterInfoService service) {
        this.service = service;
    }

    @GetMapping(path = "/letters")
    public LettersInfoResponse getLetters(@RequestParam(name = "date") @DateTimeFormat(iso = DATE) LocalDate date) {
        return new LettersInfoResponse(service.findAll(date));
    }
}
