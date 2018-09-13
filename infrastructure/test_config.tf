# copy S2S secret from S2S's vault to app's vault, so that it can be passed to tests by Jenknins
data "azurerm_key_vault_secret" "test_s2s_secret" {
  name      = "microservicekey-send-letter-tests"
  vault_uri = "${local.s2s_vault_url}"
}

resource "azurerm_key_vault_secret" "test-s2s-secret" {
  name      = "test-s2s-secret"
  value     = "${data.azurerm_key_vault_secret.test_s2s_secret.value}"
  vault_uri = "${module.send-letter-key-vault.key_vault_uri}"
}
