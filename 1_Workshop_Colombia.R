rm(list = ls())

# INITIAL SETTINGS --------------------------------------------------------

# installing packages
list.packages = c("sf", "tidyverse", "pROC", "mapview", "terra", "mgcv", "sperrorest")
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
boxplot(basin_5000$slope_u ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Average slope (°)", xlab="", ylab="")
boxplot(basin_5000$melton_index ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Melton index", xlab="", ylab="")
boxplot(basin_5000$relief ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Relief (m)", xlab="", ylab="")
boxplot(basin_5000$twi_u ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Average TWI", xlab="", ylab="")
boxplot(basin_5000$granite ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Proportion of Granite", xlab="", ylab="")
boxplot(basin_5000$heterogeneous_agricultural ~ basin_5000$bin, col = c("dodgerblue1", "firebrick1"), main = "Proportion of heterogeneous agricultural areas (%)", xlab="", ylab="")



# MODELING ----------------------------------------------------------------
#### model fit #####
formula_1000 = bin ~
  s(slope_u) +
  s(circularity_ratio) +
  s(rainfall_daily_max_max, k=4) +
  s(heterogeneous_agricultural) 
  # s(granite, bs="tp") 

mod_1000 = mgcv::gam(formula_1000, family = binomial, method="REML", data = basin_1000)
summary(mod_1000)

plot(mod_1000, select=1, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_1000)[1], shade=T, shade.col="#74bee8", ylab="")
plot(mod_1000, select=2, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_1000)[2], shade=T, shade.col="#74bee8", ylab="")
plot(mod_1000, select=3, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_1000)[3], shade=T, shade.col="#74bee8", ylab="")
plot(mod_1000, select=4, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_1000)[4], shade=T, shade.col="#74bee8", ylab="")
plot(mod_1000, select=5, residuals=F, rug=T, all.terms=T, trans=plogis, scale=-1, seWithMean=T, shift=coef(mod_1000)[5], shade=T, shade.col="#74bee8", ylab="")

plot(mod_1000, pages = 1)
plot(mod_1000, select=1, residuals=F, rug=T)

basin_1000$prob = as.numeric(predict(mod_1000, type="response", newdata=basin_1000))
myroc_1000 = roc(response=basin_1000$bin, predictor=basin_1000$prob, auc=T)
myroc_1000$auc
plot(myroc_1000, main = round(myroc_1000$auc, 5))

mapview(basin_1000, zcol="prob", col.regions=paletteer::paletteer_d("RColorBrewer::RdYlGn", direction=-1))
mapview(basin_1000, zcol="DESINVENTAR")

#### validation #####
resamp = partition_cv(basin_1000, nfold = 10, repetition = 1, seed1= 1, coords = c("X", "Y")) # example with 3 folds for SUs with landslides
plot(resamp, SU, coords = c("X", "Y"), cex = 0.01, pch = 19)

centroid = st_centroid(d_rgw)
centroid = as.data.frame(st_coordinates(centroid))
d_rgw = as.data.frame(cbind(d_rgw, X=centroid$X, Y=centroid$Y))

# performing non-spatial cross-validation, 10 rep and 10 folds. This step takes a while
pred_cv = sperrorest(data = SU_whole, formula = Formula_truesum,
                     model_fun = mgcv::gam,
                     coords = c("X", "Y"),
                     model_args = list(family = gaussian),
                     pred_fun = predict,
                     progress = 2,
                     err_fun = my.error,
                     smp_fun = partition_cv,
                     smp_args = list(repetition = 1:10, nfold = 10, seed1= 1))