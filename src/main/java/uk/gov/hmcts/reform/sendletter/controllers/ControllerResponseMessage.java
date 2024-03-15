package uk.gov.hmcts.reform.sendletter.controllers;

/**
 * Controller response messages.
 */
@SuppressWarnings({"squid:S1118", "HideUtilityClassConstructor"})
final class ControllerResponseMessage {

    static final String RESPONSE_401 = "Invalid service authorisation header";
    static final String RESPONSE_403 = "Service not configured";
    static final String RESPONSE_400 = "Bad Request";
}
