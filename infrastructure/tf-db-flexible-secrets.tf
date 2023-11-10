# Postgres 15 flexible server store secrets in key vault
locals {
  flexible_secret_prefix         = "${var.component}-POSTGRES-FLEXIBLE"
  flexible_secret_prefix_staging = "${var.component}-POSTGRES-FLEXIBLE-STG"

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

  flexible_secrets_staging = [
    {
      name_suffix = "PASS"
      value       = module.postgresql-staging.password
    },
    {
      name_suffix = "HOST"
      value       = module.postgresql-staging.fqdn
    },
    {
      name_suffix = "USER"
      value       = module.postgresql-staging.username
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
}

resource "azurerm_key_vault_secret" "flexible_secret" {
  for_each     = { for secret in local.flexible_secrets : secret.name_suffix => secret }
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix}-${each.value.name_suffix}"
  value        = each.value.value
  tags = merge(var.common_tags, {
    "source" : "${var.component} PostgreSQL"
  })
  content_type    = ""
  expiration_date = timeadd(timestamp(), "17520h")
}

resource "azurerm_key_vault_secret" "flexible_secret_staging" {
  count        = var.num_staging_dbs
  for_each     = { for secret in local.flexible_secrets_staging : secret.name_suffix => secret }
  key_vault_id = module.send-letter-key-vault.key_vault_id
  name         = "${local.flexible_secret_prefix_staging}-${each.value.name_suffix}"
  value        = each.value.value
  tags = merge(var.common_tags, {
    "source" : "${var.component} PostgreSQL"
  })
  content_type    = ""
  expiration_date = timeadd(timestamp(), "17520h")
}
