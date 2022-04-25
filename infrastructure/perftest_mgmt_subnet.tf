locals {
  perftest_vnet_name           = "core-infra-vnet-perftest"
  perftest_subnet_name         = "core-infra-subnet-mgmtperftest"
  perftest_vnet_resource_group = "core-infra-perftest"
}

data "azurerm_subnet" "perftest_mgmt_subnet" {
  provider             = azurerm.aks_perftest_mgmt
  name                 = local.perftest_subnet_name
  virtual_network_name = local.perftest_vnet_name
  resource_group_name  = local.perftest_vnet_resource_group
}
