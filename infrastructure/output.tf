output "vaultUri" {
  value = "${module.send-letter-key-vault.key_vault_uri}"
}

output "vaultName" {
  value = "${module.send-letter-key-vault.key_vault_name}"
}

output "microserviceName" {
  value = "${var.component}"
}
