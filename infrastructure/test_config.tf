# copy S2S secret from S2S's vault to app's vault, so that it can be passed to tests by Jenkins
data "azurerm_key_vault_secret" "source_test_s2s_secret" {
  name      = "microservicekey-send-letter-tests"
  vault_uri = "${local.s2s_vault_url}"
}

resource "azurerm_key_vault_secret" "test_s2s_secret" {
  name      = "test-s2s-secret"
  value     = "${data.azurerm_key_vault_secret.source_test_s2s_secret.value}"
  vault_uri = "${module.send-letter-key-vault.key_vault_uri}"
}

data "azurerm_key_vault_secret" "source_test_ftp_user" {
  name      = "test-ftp-user"
  key_vault_id = "${module.send-letter-key-vault.key_vault_id}"
}

resource "azurerm_key_vault_secret" "test_ftp_user" {
  name      = "test-ftp-user"
  value     = "${data.azurerm_key_vault_secret.source_test_ftp_user.value}"
  vault_uri = "${module.send-letter-key-vault.key_vault_uri}"
}

data "azurerm_key_vault_secret" "source_test_ftp_private_key" {
  name      = "test-ftp-private-key"
  key_vault_id = "${module.send-letter-key-vault.key_vault_id}"
}

resource "azurerm_key_vault_secret" "test_ftp_private_key" {
  name      = "test-ftp-private-key"
  value     = "${data.azurerm_key_vault_secret.source_test_ftp_private_key.value}"
  vault_uri = "${module.send-letter-key-vault.key_vault_uri}"
}

data "azurerm_key_vault_secret" "source_test_ftp_public_key" {
  name      = "test-ftp-public-key"
  key_vault_id = "${module.send-letter-key-vault.key_vault_id}"
}

resource "azurerm_key_vault_secret" "test_ftp_public_key" {
  name      = "test-ftp-public-key"
  value     = "${data.azurerm_key_vault_secret.source_test_ftp_public_key.value}"
  vault_uri = "${module.send-letter-key-vault.key_vault_uri}"
}
