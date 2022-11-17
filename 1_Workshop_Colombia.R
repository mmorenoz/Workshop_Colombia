rm(list = ls())

# INITIAL SETTINGS --------------------------------------------------------

# installing packages
list.packages = c("sf", "tidyverse", "pROC", "mapview", "terra", "mgcv")
new.packages = list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# loading packages
lapply(list.packages, require, character.only=T)
remove(list.packages, new.packages)

# setting up directory
path = getwd()
setwd(path)
remove(path)

# LOADING DATA ------------------------------------------------------------
# load inventories and Antioquia boundary
SIMMA_catalog = sf::st_read("./data/simma_catalog.gpkg")
SIMMA_inventory = sf::st_read("./data/simma_inventory.gpkg")
DESINVENTAR = sf::st_read("./data/desinventar.gpkg")
antioquia = sf::st_read("./data/antioquia.gpkg")

# load mapping units
basin_1000 = sf::st_read("./data/basin_1000.gpkg")
basin_5000 = sf::st_read("./data/basin_5000.gpkg")
basin_10000 = sf::st_read("./data/basin_10000.gpkg")
basin_25000 = sf::st_read("./data/basin_25000.gpkg")
basin_50000 = sf::st_read("./data/basin_50000.gpkg")

# visualize data
mapview(basin_1000, color = "black", alpha.regions=0) +
  mapview(basin_5000, color = "black", alpha.regions=0) +
  mapview(basin_10000, color = "black", alpha.regions=0) +
  mapview(basin_25000, color = "black", alpha.regions=0) +
  mapview(basin_50000, color = "black", alpha.regions=0) +
  mapview(SIMMA_catalog, col.regions = "green") +
  mapview(SIMMA_inventory, col.regions = "blue") +
  mapview(DESINVENTAR, col.regions = "red")


# EXPLORATORY DATA ANALYSIS -----------------------------------------------
##### SIMMA inventory ####
SIMMA_inventory = SIMMA_inventory %>%
  dplyr::mutate(across(type:municipality, factor)) %>%
  dplyr::mutate(across(year:doy, as.numeric))

# histograms and barplots
# subtype
summary(SIMMA_inventory$subtype)
barplot(table(SIMMA_inventory$subtype), main = "Bar chart of subtype", col = "dodgerblue1")
pie(table(SIMMA_inventory$subtype), main="Pie chart of subtype")

# dates
table(SIMMA_inventory$month)
barplot(table(SIMMA_inventory$month), main = "Bar chart of month", col = "dodgerblue1")
table(SIMMA_inventory$year)
barplot(table(SIMMA_inventory$year), main = "Bar chart of year", col = "dodgerblue1")

#### DESINVENTAR inventory ####
DESINVENTAR = DESINVENTAR %>%
  dplyr::mutate(across(cause:municipality, factor)) %>%
  dplyr::mutate(across(year:doy, as.numeric)) %>%
  dplyr::mutate(across(people_dead:people_missing, as.numeric))

# histograms and barplots
table(DESINVENTAR$month)
barplot(table(DESINVENTAR$month), main = "Bar chart of month", col = "firebrick1")

table(DESINVENTAR$year)
barplot(table(DESINVENTAR$month), main = "Bar chart of year", col = "firebrick1")

# for specific municipalities
barplot(table(DESINVENTAR$year[DESINVENTAR$municipality=="Medellín"]), col = "firebrick1")

#### MAPPING UNITS ####
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
