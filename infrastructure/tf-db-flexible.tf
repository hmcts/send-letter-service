# Postgres 15 flexible servers
locals {
  db_host_name = "${var.component}-flexible-postgres-db-v15"
  db_name      = "send_letter"
}

# Secrets for schema perms
data "azurerm_key_vault_secret" "POSTGRES-USER" {
  name         = "send-letter-service-POSTGRES-USER"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

data "azurerm_key_vault_secret" "POSTGRES-PASS" {
  name         = "send-letter-service-POSTGRES-PASS"
  key_vault_id = module.send-letter-key-vault.key_vault_id
}

module "postgresql" {
  providers = {
    azurerm.postgres_network = azurerm.postgres_network
  }

  source               = "git@github.com:hmcts/terraform-module-postgresql-flexible?ref=master"
  name                 = local.db_host_name
  product              = "send-letter"
  component            = var.component
  location             = var.location_db
  env                  = var.env
  pgsql_admin_username = local.db_name
  pgsql_databases = [
    {
      name : local.db_name
    }
  ]
  common_tags   = var.common_tags
  business_area = "cft"
  pgsql_version = "15"
  subnet_suffix = "expanded"

  admin_user_object_id = var.jenkins_AAD_objectId
  kv_name = module.send-letter-key-vault.key_vault_name
  user_secret_name = azurerm_key_vault_secret.POSTGRES-USER.name
  pass_secret_name = azurerm_key_vault_secret.POSTGRES-PASS.name

  # Force user permissions
  force_user_permissions_trigger = "1"

  # Database storage
  pgsql_storage_mb = var.pgsql_storage_mb
  pgsql_sku        = var.pgsql_sku
  
}

module "postgresql-staging" {
  count                = var.env == "aat" ? 1 : 0
  providers = {
    azurerm.postgres_network = azurerm.postgres_network
  }
  source               = "git@github.com:hmcts/terraform-module-postgresql-flexible?ref=master"
  name                 = "${var.component}-stg-db-v15"
  product              = var.product
  component            = var.component
  location             = var.location_db
  env                  = "aat"
  pgsql_admin_username = local.db_name
  pgsql_databases = [
    {
      name : local.db_name
    }
  ]
  common_tags   = var.common_tags
  business_area = "cft"
  pgsql_version = "15"
  subnet_suffix = "expanded"

  admin_user_object_id = var.jenkins_AAD_objectId
}
