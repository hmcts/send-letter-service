output "vaultUri" {
  value = "${module.send-letter-key-vault.key_vault_uri}"
}

output "vault_name" {
  value = "${module.send-letter-key-vault.key_vault_name}"
}

output "microserviceName" {
  value = "${var.component}"
}

output "resource_group_name" {
  value = "${azurerm_resource_group.rg.name}"
}

output "key_vault_id" {
  value = "${module.send-letter-key-vault.key_vault_id}"
}
#region Data for tests

output "test_s2s_url" {
  value = "${local.s2s_url}"
}

output "test_s2s_name" {
  value = "send_letter_tests"
}

output "test_ftp_hostname" {
  value = "${var.ftp_hostname}"
}

output "test_ftp_port" {
  value = "${var.ftp_port}"
}

output "test_ftp_fingerprint" {
  value = "${var.ftp_fingerprint}"
}

output "test_ftp_target_folder" {
  value = "${var.ftp_smoke_test_target_folder}"
}

output "ftp_target_folder" {
  value = "${var.ftp_target_folder}"
}

output "test_encryption_enabled" {
  value = "${var.encyption_enabled}"
}

#endregion
