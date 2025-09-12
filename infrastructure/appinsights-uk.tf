module "application_insights_uk" {
  source = "git@github.com:hmcts/terraform-module-application-insights?ref=4.x"

  env                 = var.env
  product             = var.product
  override_name       = var.component
  location            = var.location
  resource_group_name = azurerm_resource_group.rg.name

  common_tags = var.common_tags
}

resource "azurerm_key_vault_secret" "app_insights_connection_string_uk" {
  name         = "app-insights-connection-string-uk"
  value        = module.application_insights_uk.connection_string
  key_vault_id = module.send-letter-key-vault.key_vault_id
}
