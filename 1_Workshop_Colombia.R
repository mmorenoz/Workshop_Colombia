rm(list = ls())

# load libraries
list.packages = c("sf", "tidyverse", "pROC", "mapview", "terra", "mgcv", "data.table")
vapply(list.packages, library, logical(1), character.only = TRUE, logical.return = TRUE, quietly = TRUE)
remove(list.packages)

# load inventories and outline
outline = sf::st_read("/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/mapping_units/colombia.shp") %>%
  dplyr::filter(ADM1_ES == "Antioquia") %>% dplyr::select(ADM1_ES)

sf::st_write(basin_1000, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/basin_1000.gpkg")
sf::st_write(basin_5000, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/basin_5000.gpkg")
sf::st_write(basin_10000, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/basin_10000.gpkg")
sf::st_write(basin_25000, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/basin_25000.gpkg")
sf::st_write(basin_50000, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/basin_50000.gpkg")
sf::st_write(DESINV, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/desinventar.gpkg")
sf::st_write(SIMMA_catalog, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/simma_catalog.gpkg")
sf::st_write(SIMMA_inventory, "/home/mmorenozapata@eurac.edu/OneDrive/COLOMBIA/Torrential_Flows/0_Workshop/simma_inventory.gpkg")


# plots
mapview(basin_1000, color = "black", alpha.regions=0) +
  mapview(basin_5000, color = "black", alpha.regions=0) +
  mapview(basin_10000, color = "black", alpha.regions=0) +
  mapview(basin_25000, color = "black", alpha.regions=0) +
  mapview(basin_50000, color = "black", alpha.regions=0) +
  mapview(SIMMA_catalog, col.regions = "blue") +
  mapview(SIMMA_inventory, col.regions = "blue") +
  mapview(DESINV, col.regions = "red")

# histograms and boxplots
boxplot(basin_5000$slope_u ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Average slope (°)", xlab="")
boxplot(basin_5000$melton_index ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Melton index", xlab="")
boxplot(basin_5000$relief ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Relief (m)", xlab="")
boxplot(basin_5000$twi_u ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Average TWI", xlab="")
boxplot(basin_5000$granite ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Proportion of Granite", xlab="")
boxplot(basin_5000$heterogeneous_agricultural ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Proportion of heterogeneous agricultural areas (%)", xlab="")
boxplot(basin_5000$rainfall_daily_max_max~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), ylab = "Maximum daily rainfall (mm)", xlab="")


# model fit  #
formula_5000 = bin ~ s(slope_u, bs="tp") +
  s(area, bs="tp") +
  s(profile_curv_u) +
  s(forest_plantation, bs="tp") +
  s(rainfall_daily_max_u, bs="tp") +
  s(heterogeneous_agricultural, bs="tp") +
  s(artificial_land) +
  s(granite, bs="tp") 

mod_5000 = mgcv::gam(formula_5000, family = binomial, method="REML", data = basin_5000)
summary(mod_5000)
gam.check(mod_5000, rep=500)

plot(mod_5000, select=6, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_5000)[1], shade=T, shade.col="#74bee8", ylab="")

basin_5000$prob = as.numeric(predict(mod_5000, type="response", newdata=basin_5000, newdata.guaranteed=TRUE))
myroc_5000 = roc(response=basin_5000$bin, predictor=basin_5000$prob, auc=T)
myroc_5000$auc
plot(myroc_5000, main = round(myroc_5000$auc, 5))

mapview(basin_5000, zcol="prob", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))
mapview(basin_5000, zcol="DESINVENTAR")
