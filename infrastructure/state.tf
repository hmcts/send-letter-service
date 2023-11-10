terraform {
  backend "azurerm" {}

  required_providers {
    azuread = {
      source  = "hashicorp/azuread"
      version = "2.39.0"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.63.0"
    }
    random = {
      source = "hashicorp/random"
    }
    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = ">=1.17.1"
    }
  }
}

provider "azurerm" {
  features {}
}
