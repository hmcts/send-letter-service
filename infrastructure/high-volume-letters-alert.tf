module "send-letter-high-volume-alert" {
  source            = "git@github.com:hmcts/cnp-module-metric-alert"
  location          = azurerm_application_insights.appinsights.location
  app_insights_name = azurerm_application_insights.appinsights.name

  enabled    = var.env == "prod"
  alert_name = "Send_Letter_high_volume_-_BSP"
  alert_desc = "Triggers when send letter service receives large number of letters during a day."
  common_tags = var.common_tags

  app_insights_query = "traces | where message startswith 'Uploaded letter id'"

  frequency_in_minutes       = 60
  time_window_in_minutes     = 1440
  severity_level             = "1"
  action_group_name          = "${module.alert-action-group.action_group_name}"
  custom_email_subject       = "Send Letter Service high volume"
  trigger_threshold_operator = "GreaterThan"
  trigger_threshold          = 100
  resourcegroup_name         = "${azurerm_resource_group.rg.name}"
}
