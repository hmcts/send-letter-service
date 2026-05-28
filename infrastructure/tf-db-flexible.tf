# Postgres 15 flexible servers
locals {
  db_host_name = "${var.component}-flexible-postgres-db-v15"
  db_name      = "send_letter"
}

data "azurerm_user_assigned_identity" "jenkins_cftptl_intsvc" {
  provider            = azurerm.cftptl_intsvc
  name                = "jenkins-cftptl-intsvc-mi"
  resource_group_name = "managed-identities-cftptl-intsvc-rg"
}

module "postgresql" {
  providers = {
    azurerm.postgres_network = azurerm.postgres_network
  }

  source               = "git@github.com:hmcts/terraform-module-postgresql-flexible?ref=DTSPO-30107-additional-postgres-admins"
  name                 = local.db_host_name
  product              = "send-letter"
  component            = var.component
  location             = var.location
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

  admin_user_object_id          = var.jenkins_AAD_objectId
  existing_admin_user_object_id = data.azurerm_user_assigned_identity.jenkins_cftptl_intsvc.principal_id

  # Force user permissions
  force_user_permissions_trigger = "1"

  # Database storage
  pgsql_storage_mb = var.pgsql_storage_mb
  pgsql_sku        = var.pgsql_sku

}
