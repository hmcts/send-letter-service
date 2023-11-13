# Postgres 15 flexible server store secrets in key vault
locals {
  flexible_secret_prefix         = "${var.component}-POSTGRES-FLEXIBLE"
  flexible_secret_prefix_staging = "${var.component}-staging-db-flexible"

  flexible_secrets = [
    {
      name_suffix = "PASS"
      value       = module.postgresql.password
    },
    {
      name_suffix = "HOST"
      value       = module.postgresql.fqdn
    },
    {
      name_suffix = "USER"
      value       = module.postgresql.username
    },
    {
      name_suffix = "PORT"
      value       = "5432"
    },
    {
      name_suffix = "DATABASE"
      value       = local.db_name
    }
  ]

resource "azurerm_key_vault_secret" "flexible_staging_db_user" {
  count        = var.num_staging_dbs
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-user"
  value        = try(module.postgresql-staging[0].username, "null")
}

resource "azurerm_key_vault_secret" "flexible_staging_db_password" {
  count        = var.num_staging_dbs
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-password"
  value        = try(module.postgresql-staging[0].password, "null")
}

resource "azurerm_key_vault_secret" "flexible_staging_db_host" {
  count        = var.num_staging_dbs
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-host"
  value        = try(module.postgresql-staging[0].fqdn, "null")
}

resource "azurerm_key_vault_secret" "flexible_staging_db_port" {
  count        = var.num_staging_dbs
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-port"
  value        = "5432"
}

resource "azurerm_key_vault_secret" "flexible_staging_db_name" {
  count        = var.num_staging_dbs
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-name"
  value        = try(module.postgresql-staging[0].db_name, "null")
}
