provider "azurerm" {
  alias                      = "mgmt"
  subscription_id            = var.mgmt_subscription_id
  skip_provider_registration = true
  features {}
}

locals {
  container_names = [
    "new-cmc",
    "new-divorce",
    "new-finrem",
    "new-fpl",
    "new-nfdivorce",
    "new-probate",
    "new-sscs",
    "backup",
    "processed",
    "zipped",
    "encrypted",
    "new-bulkprint",
    "new-preview",
    "backup-preview",
    "processed-preview",
    "zipped-preview",
    "encrypted-preview"
  ]

  standard_subnets = [
    data.azurerm_subnet.jenkins_subnet.id,
    data.azurerm_subnet.jenkins_aks_00.id,
    data.azurerm_subnet.jenkins_aks_01.id,
    data.azurerm_subnet.cft_aks_00_subnet.id,
    data.azurerm_subnet.cft_aks_01_subnet.id
  ]

  preview_subnets   = var.env == "aat" ? [data.azurerm_subnet.preview_aks_00_subnet.id, data.azurerm_subnet.preview_aks_01_subnet.id] : []
  perftest_subnets  = var.env == "perftest" ? [data.azurerm_subnet.perftest_mgmt_subnet.id] : []
  all_valid_subnets = concat(local.standard_subnets, local.preview_subnets, local.perftest_subnets)

  short_component = replace(var.component, "-service", "")

  keda_mi_object_id = data.azurerm_user_assigned_identity.keda_mi.principal_id
}

module "storage_account" {
  source                     = "git@github.com:hmcts/cnp-module-storage-account?ref=master"
  env                        = var.env
  storage_account_name       = replace("${var.product}${local.short_component}${var.env}", "-", "")
  resource_group_name        = azurerm_resource_group.rg.name
  location                   = azurerm_resource_group.rg.location
  account_kind               = "StorageV2"
  account_tier               = "Standard"
  account_replication_type   = var.storage_account_repl_type
  sa_subnets                 = local.all_valid_subnets
  managed_identity_object_id = local.keda_mi_object_id
  role_assignments = [
    "Storage Blob Data Reader"
  ]
  pim_roles = {}

  common_tags = local.tags
}

resource "azurerm_storage_container" "service_containers" {
  for_each             = toset(local.container_names)
  name                 = each.value
  storage_account_name = module.storage_account.storageaccount_name
}

resource "azurerm_key_vault_secret" "storage_account_name" {
  name         = "storage-account-name"
  value        = module.storage_account.storageaccount_name
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

resource "azurerm_key_vault_secret" "storage_account_primary_key" {
  name         = "storage-account-primary-key"
  value        = module.storage_account.storageaccount_primary_access_key
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

# this secret is used by blob-router-service for uploading blobs
resource "azurerm_key_vault_secret" "storage_account_connection_string" {
  name         = "storage-account-connection-string"
  value        = module.storage_account.storageaccount_primary_connection_string
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

output "storage_account_name" {
  value = module.storage_account.storageaccount_name
}

output "storage_account_primary_key" {
  sensitive = true
  value     = module.storage_account.storageaccount_primary_access_key
}
