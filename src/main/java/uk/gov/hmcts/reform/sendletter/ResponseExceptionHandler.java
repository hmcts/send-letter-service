package uk.gov.hmcts.reform.sendletter;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.DataBinder;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;
import uk.gov.hmcts.reform.authorisation.exceptions.InvalidTokenException;
import uk.gov.hmcts.reform.sendletter.exception.DuplexException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterSaveException;
import uk.gov.hmcts.reform.sendletter.exception.ServiceNotConfiguredException;
import uk.gov.hmcts.reform.sendletter.exception.UnableToGenerateSasTokenException;
import uk.gov.hmcts.reform.sendletter.exception.UnauthenticatedException;
import uk.gov.hmcts.reform.sendletter.model.out.errors.FieldError;
import uk.gov.hmcts.reform.sendletter.model.out.errors.ModelValidationError;

import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.CONFLICT;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.UNAUTHORIZED;
import static org.springframework.http.ResponseEntity.badRequest;
import static org.springframework.http.ResponseEntity.status;

@ControllerAdvice
public class ResponseExceptionHandler extends ResponseEntityExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(ResponseExceptionHandler.class);

    @InitBinder
    protected void activateDirectFieldAccess(DataBinder dataBinder) {
        dataBinder.initDirectFieldAccess();
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(
        MethodArgumentNotValidException exception,
        HttpHeaders headers,
        HttpStatus status,
        WebRequest request
    ) {
        List<FieldError> fieldErrors =
            exception
                .getBindingResult()
                .getFieldErrors()
                .stream()
                .map(err -> new FieldError(err.getField(), err.getDefaultMessage()))
                .collect(toList());

        ModelValidationError error = new ModelValidationError(fieldErrors);

        log.info("Bad request: {}", error);

        return badRequest().body(error);
    }

    @ExceptionHandler(InvalidTokenException.class)
    protected ResponseEntity<Void> handleInvalidTokenException(InvalidTokenException exc) {
        log.warn(exc.getMessage(), exc);
        return status(UNAUTHORIZED).build();
    }

    @ExceptionHandler(LetterNotFoundException.class)
    protected ResponseEntity<Void> handleLetterNotFoundException(LetterNotFoundException exc) {
        log.warn(exc.getMessage(), exc);
        return status(NOT_FOUND).build();
    }

    @ExceptionHandler(JsonProcessingException.class)
    protected ResponseEntity<String> handleJsonProcessingException() {
        return status(BAD_REQUEST).body("Exception occurred while parsing letter contents");
    }

    @ExceptionHandler({DuplexException.class, LetterSaveException.class})
    protected ResponseEntity<String> handleInvalidPdfException() {
        // only then pdf is actually checked hence invalid pdf message
        return status(BAD_REQUEST).body("Invalid pdf");
    }

    @ExceptionHandler(UnauthenticatedException.class)
    protected ResponseEntity<String> handleUnauthenticatedException(UnauthenticatedException exc) {
        log.warn(exc.getMessage(), exc);
        return status(UNAUTHORIZED).build();
    }

    @ExceptionHandler(ServiceNotConfiguredException.class)
    protected ResponseEntity<String> handleServiceNotConfiguredException(ServiceNotConfiguredException exc) {
        log.warn(exc.getMessage(), exc);
        return status(FORBIDDEN).body("Service not configured");
    }

    @ExceptionHandler(Exception.class)
    protected ResponseEntity<Void> handleInternalException(Exception exc) {
        log.error(exc.getMessage(), exc);
        return status(INTERNAL_SERVER_ERROR).build();
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<String> handleDataIntegrityViolationException(final DataIntegrityViolationException dve) {
        log.error(dve.getMessage(), dve);
        return status(CONFLICT).body("Duplicate request");
    }

    @ExceptionHandler(UnableToGenerateSasTokenException.class)
    protected ResponseEntity<String> handleUnableToGenerateSasTokenException() {
        return status(INTERNAL_SERVER_ERROR).body("Exception occurred while generating SAS Token");
    }
}
