locals {
  demo_imports = [
    PASS = {
      id = "https://rpe-send-letter-demo.vault.azure.net/secrets/send-letter-service-POSTGRES-PASS/8a819aad99654173a543079d1974bcff"
      to = azurerm_key_vault_secret.flexible_secret_standard_format["PASS"]
    },
    HOST = {
      id = "https://rpe-send-letter-demo.vault.azure.net/secrets/send-letter-service-POSTGRES-HOST/da007f02beff44b9a15599b84fd6f669"
      to = azurerm_key_vault_secret.flexible_secret_standard_format["HOST"]
    },
    USER = {
      id = "https://rpe-send-letter-demo.vault.azure.net/secrets/send-letter-service-POSTGRES-USER/9f2fcc0e82314326a283b7b44b3af161"
      to = azurerm_key_vault_secret.flexible_secret_standard_format["USER"]
    },
    PORT = {
      id = "https://rpe-send-letter-demo.vault.azure.net/secrets/send-letter-service-POSTGRES-PORT/a58c542b268c4c89939c52d7914908a1"
      to = azurerm_key_vault_secret.flexible_secret_standard_format["PORT"]
    },
    DATABASE = {
      id = "https://rpe-send-letter-demo.vault.azure.net/secrets/send-letter-service-POSTGRES-DATABASE/ee565b76f0614b81936ca93c8ebc9285"
      to = azurerm_key_vault_secret.flexible_secret_standard_format["DATABASE"]
    },
  ]
}

import {
  for_each = { for k, v in local.demo_imports: k => v if var.env == "demo" }
  id = each.key
  to = azurerm_key_vault_secret.flexible_secret_standard_format[each.key]
}