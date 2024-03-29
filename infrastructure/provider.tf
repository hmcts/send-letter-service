provider "azurerm" {
  alias           = "cft-mgmt"
  subscription_id = "ed302caf-ec27-4c64-a05e-85731c3ce90e"
  features {}
}

provider "azurerm" {
  features {}
  alias                      = "aks"
  subscription_id            = var.aks_subscription_id
  skip_provider_registration = true
}

provider "azurerm" {
  features {}
  alias           = "aks_preview"
  subscription_id = "8b6ea922-0862-443e-af15-6056e1c9b9a4"
}

provider "azurerm" {
  features {}
  alias           = "aks_perftest_mgmt"
  subscription_id = "7a4e3bd5-ae3a-4d0c-b441-2188fee3ff1c"
}

provider "azurerm" {
  features {}
  skip_provider_registration = true
  alias                      = "postgres_network"
  subscription_id            = var.aks_subscription_id
}
