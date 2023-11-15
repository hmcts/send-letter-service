

# Make sure the resource group exists
resource "azurerm_resource_group" "rg" {
  name     = "${var.product}-${var.component}-${var.env}"
  location = var.location_app
  tags     = var.common_tags
}

locals {
  ftp_private_key = data.azurerm_key_vault_secret.ftp_private_key.value
  ftp_public_key  = data.azurerm_key_vault_secret.ftp_public_key.value
  ftp_user        = data.azurerm_key_vault_secret.ftp_user.value

  encryption_public_key = data.azurerm_key_vault_secret.encryption_public_key.value

  product = "bulk-print"
  tags    = var.common_tags
}

# region Azure Key Vault
module "send-letter-key-vault" {
  source              = "git@github.com:hmcts/cnp-module-key-vault?ref=master"
  name                = "${var.product}-send-letter-${var.env}"
  product             = var.product
  env                 = var.env
  tenant_id           = var.tenant_id
  object_id           = var.jenkins_AAD_objectId
  resource_group_name = azurerm_resource_group.rg.name

  # dcd_cc-dev group object ID
  product_group_object_id              = "38f9dea6-e861-4a50-9e73-21e64f563537"
  common_tags                          = var.common_tags
  create_managed_identity              = true
  additional_managed_identities_access = ["rpe-shared"]
}

data "azurerm_key_vault" "s2s_key_vault" {
  name                = "s2s-${var.env}"
  resource_group_name = "rpe-service-auth-provider-${var.env}"
}

resource "azurerm_key_vault_secret" "APP-INSTRUMENTATION-KEY" {
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "app-insights-instrumentation-key"
  value        = azurerm_application_insights.appinsights.instrumentation_key
}

data "azurerm_key_vault_secret" "smtp_username" {
  name         = "reports-email-username"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "smtp_password" {
  name         = "reports-email-password"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "upload_summary_recipients" {
  name         = "upload-summary-report-recipients"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "ftp_user" {
  name         = "ftp-user"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "ftp_private_key" {
  name         = "ftp-private-key"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "ftp_public_key" {
  name         = "ftp-public-key"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "encryption_public_key" {
  name         = "encryption-public-key"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "launch_darkly_sdk_key" {
  name         = "launch-darkly-sdk-key"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "launch_darkly_offline_mode" {
  name         = "launch-darkly-offline-mode"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

# endregion
