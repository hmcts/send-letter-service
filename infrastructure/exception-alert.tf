module "send-letter-service-exception-alert" {
  source            = "git@github.com:hmcts/cnp-module-metric-alert"
  location          = var.appinsights_location
  app_insights_name = module.application_insights.name

  enabled     = var.env == "prod"
  alert_name  = "Send_Letter_Service_exception_-_BSP"
  alert_desc  = "Triggers when send letter service receive at least one exception within a 15 minutes window timeframe."
  common_tags = var.common_tags

  app_insights_query = <<EOF
union exceptions, traces
| where severityLevel >= 3
EOF

  frequency_in_minutes       = "15"
  time_window_in_minutes     = "15"
  severity_level             = "1"
  action_group_name          = module.alert-action-group.action_group_name
  custom_email_subject       = "Send Letter Service Exception"
  trigger_threshold_operator = "GreaterThan"
  trigger_threshold          = "0"
  resourcegroup_name         = azurerm_resource_group.rg.name
}
