data "azurerm_key_vault_secret" "source_bsp_email_secret" {
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "send-letter-alert-email"
}

module "alert-action-group" {
  source   = "git@github.com:hmcts/cnp-module-action-group"
  location = "global"
  env      = var.env

  resourcegroup_name     = azurerm_resource_group.rg.name
  action_group_name      = "Bulk Print Alert (${var.env})"
  short_name             = "BSP_Alert"
  email_receiver_name    = "BSP Alerts And Monitoring"
  email_receiver_address = data.azurerm_key_vault_secret.source_bsp_email_secret.value
}

resource "azurerm_key_vault_secret" "alert_action_group_name" {
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "alert-action-group-name"
  value        = module.alert-action-group.action_group_name
}

output "action_group_name" {
  value = module.alert-action-group.action_group_name
}
