variable "product" {}

variable "component" {}

variable "location" {
  default = "UK South"
}

variable "alert_location" {
  description = "Target Azure location to deploy the alert"
  type        = string
  default     = "global"
}

variable "appinsights_location" {
  default     = "West Europe"
  description = "Location for Application Insights"
}

variable "env" {}

variable "tenant_id" {}

variable "subscription" {}

variable "jenkins_AAD_objectId" {
  description = "(Required) The Azure AD object ID of a user, service principal or security group in the Azure Active Directory tenant for the vault. The object ID must be unique for the list of access policies."
}

variable "common_tags" {
  type = map(string)
}

variable "aks_subscription_id" {
  default = ""
}

variable "pgsql_storage_mb" {
  description = "Max storage allowed for the PGSql Flexibile instance"
  type        = number
  default     = 65536
}

variable "pgsql_sku" {
  description = "The PGSql flexible server instance sku"
  default     = "GP_Standard_D2s_v3"
}
