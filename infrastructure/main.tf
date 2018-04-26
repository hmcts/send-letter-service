provider "azurerm" {}

provider "vault" {
  address = "https://vault.reform.hmcts.net:6200"
}

locals {
  db_connection_options = "?ssl=true"
}

# Make sure the resource group exists
resource "azurerm_resource_group" "rg" {
  name     = "${var.product}-${var.component}-${var.env}"
  location = "${var.location_app}"
}

# read the microservice key for tests from Vault
data "vault_generic_secret" "tests_s2s_secret" {
  path = "secret/${var.vault_section}/ccidam/service-auth-provider/api/microservice-keys/send-letter-tests"
}

data "vault_generic_secret" "ftp_user" {
  path = "secret/${var.vault_section}/cc/send-letter-consumer/ftp-user"
}

data "vault_generic_secret" "ftp_private_key" {
  path = "secret/${var.vault_section}/cc/send-letter-consumer/ftp-private-key"
}

data "vault_generic_secret" "ftp_public_key" {
  path = "secret/${var.vault_section}/cc/send-letter-consumer/ftp-public-key"
}

data "vault_generic_secret" "encryption_public_key" {
  path = "secret/${var.vault_section}/cc/send-letter/encryption-key"
}

locals {
  ase_name               = "${data.terraform_remote_state.core_apps_compute.ase_name[0]}"
  ftp_private_key        = "${replace(data.vault_generic_secret.ftp_private_key.data["value"], "\\n", "\n")}"
  ftp_public_key         = "${replace(data.vault_generic_secret.ftp_public_key.data["value"], "\\n", "\n")}"
  ftp_user               = "${data.vault_generic_secret.ftp_user.data["value"]}"
  encryption_public_key  = "${replace(data.vault_generic_secret.encryption_public_key.data["value"], "\\n", "\n")}"
  s2s_url                = "http://rpe-service-auth-provider-${var.env}.service.${local.ase_name}.internal"
}

module "db" {
  source              = "git@github.com:hmcts/moj-module-postgres.git?ref=master"
  product             = "${var.product}-db"
  location            = "${var.location_db}"
  env                 = "${var.env}"
  postgresql_database = "postgres"
  postgresql_user     = "letter_tracking"
}

module "send-letter-service" {
  source              = "git@github.com:hmcts/moj-module-webapp?ref=master"
  product             = "${var.product}-${var.component}"
  location            = "${var.location_app}"
  env                 = "${var.env}"
  ilbIp               = "${var.ilbIp}"
  resource_group_name = "${azurerm_resource_group.rg.name}"
  subscription        = "${var.subscription}"

  app_settings = {
    S2S_URL                         = "${local.s2s_url}"
    LETTER_TRACKING_DB_HOST         = "${module.db.host_name}"
    LETTER_TRACKING_DB_PORT         = "${module.db.postgresql_listen_port}"
    LETTER_TRACKING_DB_USER_NAME    = "${module.db.user_name}"
    LETTER_TRACKING_DB_PASSWORD     = "${module.db.postgresql_password}"
    LETTER_TRACKING_DB_NAME         = "${module.db.postgresql_database}"
    LETTER_TRACKING_DB_CONN_OPTIONS = "${local.db_connection_options}"
    FLYWAY_URL                      = "jdbc:postgresql://${module.db.host_name}:${module.db.postgresql_listen_port}/${module.db.postgresql_database}${local.db_connection_options}"
    FLYWAY_USER                     = "${module.db.user_name}"
    FLYWAY_PASSWORD                 = "${module.db.postgresql_password}"
    ENCRYPTION_ENABLED              = "${var.encyption_enabled}"
    SCHEDULING_ENABLED              = "${var.scheduling_enabled}"
    // ftp
    FTP_HOSTNAME                    = "${var.ftp_hostname}"
    FTP_PORT                        = "${var.ftp_port}"
    FTP_FINGERPRINT                 = "${var.ftp_fingerprint}"
    FTP_TARGET_FOLDER               = "${var.ftp_target_folder}"
    FTP_SMOKE_TEST_TARGET_FOLDER    = "${var.ftp_smoke_test_target_folder}"
    FTP_REPORTS_FOLDER              = "${var.ftp_reports_folder}"
    FTP_REPORTS_CRON                = "${var.ftp_reports_cron}"
    FTP_USER                        = "${local.ftp_user}"
    FTP_PRIVATE_KEY                 = "${local.ftp_private_key}"
    FTP_PUBLIC_KEY                  = "${local.ftp_public_key}"
    ENCRYPTION_PUBLIC_KEY           = "${local.encryption_public_key}"
    // Azure feature
    WEBSITE_LOCAL_CACHE_OPTION      = "${var.local_cache}"
  }
}

# region save DB details to Azure Key Vault
module "key-vault" {
  source              = "git@github.com:hmcts/moj-module-key-vault?ref=master"
  product             = "${var.product}"
  env                 = "${var.env}"
  tenant_id           = "${var.tenant_id}"
  object_id           = "${var.jenkins_AAD_objectId}"
  resource_group_name = "${azurerm_resource_group.rg.name}"
  # dcd_cc-dev group object ID
  product_group_object_id = "38f9dea6-e861-4a50-9e73-21e64f563537"
}

resource "azurerm_key_vault_secret" "POSTGRES-USER" {
  name      = "${var.component}-POSTGRES-USER"
  value     = "${module.db.user_name}"
  vault_uri = "${module.key-vault.key_vault_uri}"
}

resource "azurerm_key_vault_secret" "POSTGRES-PASS" {
  name      = "${var.component}-POSTGRES-PASS"
  value     = "${module.db.postgresql_password}"
  vault_uri = "${module.key-vault.key_vault_uri}"
}

resource "azurerm_key_vault_secret" "POSTGRES_HOST" {
  name      = "${var.component}-POSTGRES-HOST"
  value     = "${module.db.host_name}"
  vault_uri = "${module.key-vault.key_vault_uri}"
}

resource "azurerm_key_vault_secret" "POSTGRES_PORT" {
  name      = "${var.component}-POSTGRES-PORT"
  value     = "${module.db.postgresql_listen_port}"
  vault_uri = "${module.key-vault.key_vault_uri}"
}

resource "azurerm_key_vault_secret" "POSTGRES_DATABASE" {
  name      = "${var.component}-POSTGRES-DATABASE"
  value     = "${module.db.postgresql_database}"
  vault_uri = "${module.key-vault.key_vault_uri}"
}
# endregion
