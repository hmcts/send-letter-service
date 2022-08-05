terraform {
  backend "azurerm" {}

  required_providers {
    azuread = {
      source  = "hashicorp/azuread"
      version = "2.26.1"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.16.0"
    }
    random = {
      source = "hashicorp/random"
    }
  }
}

provider "azurerm" {
  features {}
}
