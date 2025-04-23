
## ============= set up workflow and data ===================

# project root, dependencies, plot themes
PATH = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(PATH)

# inla install testing version and centos 7
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
# inla.binary.install()

library(dplyr); library(raster); library(rgdal); library(sf)
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep)
source("00_plot_themes.R")

# INLA modelling functions, priors
source("00_inla_setup_functions_r4.R")


# build dataframe calls 21_build_model_df.R, specifying 4 variables 
# projname: name of save directory for outputs
# region: either NA, north, south or central; whether to subset to specific region
# n_clim_bins: n bins for grouping climatic predictors for nonlinear effects
# plot_graph: visualise neighbourhood matrix?
projname = "stempOOS_nb2"
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")

region = "all"
region2 = NA
n_clim_bins = 40
plot_graph = FALSE
province_case_threshold = NA

source("00_build_model_df_clim.R")

# ================= customise dataframe for this modelling task ====================


dd = read.csv("./output/data_process/ModelData_sfts_ALL_withLags.csv",fileEncoding = "GB18030")

# create dataframe for modelling, response and family
ddf = dd


# select only variables required for models and scale/log transform
ddf <- ddf %>%
  dplyr::select(
    city, 
    cityid,
    countyid, 
    county,
    year,
    month,
    date,
    total_cases,
    logpop,
    polyid,
    polyid_city,
    yearx,
    cityx, 
    areaidx,
    urban,
    transportation,
    Cropland,
    Forest,
    Grassland,
    Water,
    Barren,
    Shrub,
    elevation,
    in_migration,
    in_migration_norm,
    out_migration,
    out_migration_norm,
    out_migration_norm2,
    Tmean,
    Precipitation,
    Rh,
    Sun,
    Tmean_g,
    Precipitation_02m,
    Rh_06m,
    Rh_3m_g,
    Rh_6m_g,
    Sun_01m
  ) %>%
  dplyr::mutate(
    Tmean_g = inla.group(Tmean, n=n_clim_bins),
    Sun_01m_g = inla.group(Sun_01m, n=n_clim_bins),
    Precipitation_02m_log = log(Precipitation_02m + 1),
    Precipitation_log = log(Precipitation + 1),
    Rh_g = inla.group(Rh, n=n_clim_bins),
    in_migration_g = inla.group(in_migration_norm, n=n_clim_bins),
    urban_log = log(urban+1),
    transportation_log = log(transportation+1),
    elevation_g = inla.group(elevation, n=n_clim_bins),
    Cropland_g = inla.group(Cropland, n=n_clim_bins),
    Shrub_g = inla.group(Shrub, n=n_clim_bins),
    Grassland_g = inla.group(Grassland, n=n_clim_bins),
    Forest_g = inla.group(Forest, n=n_clim_bins),
    Cropland_log = log(Cropland+1),
    logyear = log(yearx+1)
    
  ) 




# subset to required variables
ddf = ddf %>%
  dplyr::select(
    city, 
    cityid,
    countyid, 
    county,
    year,
    month,
    date,
    total_cases, 
    logpop,
    polyid,
    yearx,
    logyear,
    cityx, 
    areaidx,
    Sun,
    Rh,
    Rh_g,
    Tmean_g,
    Tmean_1m_g,Rh_3m_g,Sun_01m_g,
    Precipitation_log,
    Precipitation_02m_log,
    in_migration_g,
    urban_log,
    transportation_log,
    elevation_g,
    Cropland_g,
    Cropland_log,
    Cropland,
    Shrub_g,
    Grassland_g,
    Forest_g
  )




# =================== create dataframe of models to fit and compare =====================

# with a province level fixed effect to account for confounding effects of space
form_base = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear"),
  collapse = " + "
)



# full model 
effect_names = 
  c("Precipitation_02m_log",
    "urban_log",
    "transportation_log",
    "Cropland_log",
    "f(Tmean_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE) ",
    "f(Rh_3m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
    "f(Sun_01m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
    "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
    "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
    "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
    "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"
  )

effect_names

m1 = paste(effect_names, collapse=" + "); 
name1 = "full"

m2 = paste(effect_names[c(1,2,3,5,6,7,8)], collapse=" + "); 
name2 = "drop_geo"

m3 = paste(effect_names[c(2,3,4,8,9,10,11)], collapse=" + "); 
name3 = "drop_climate"

m4 = paste(effect_names[c(1,4,5,6,7,9,10,11)], collapse=" + "); 
name4 = "drop_social"

m5 = paste(effect_names[c(4,9,10,11)], collapse=" + "); 
name5 = "geo"

m6 = paste(effect_names[c(1,5,6,7)], collapse=" + "); 
name6 = "climate"

m7 = paste(effect_names[c(2,3,8)], collapse=" + "); 
name7 = "social"

fx = c(m1,m2,m3,m4,m5,m6,m7)


# create data frame
fx = data.frame(modid = 1:length(fx),
                fx = fx,
                candidate = c(name1,name2,name3,name4,name5,name6,name7),
                formula = paste(form_base, fx, sep=" + "))
bs = data.frame(modid = "baseline", fx = "baseline", candidate="baseline", formula=form_base)
fx = rbind(fx, bs)

# model name
fx$model_filename = paste("fullmod_nb_proportion_model_", fx$modid, ".R", sep="")


# ================== fit models in an iterative loop ======================

# run model selection loop
for(i in 1:nrow(fx)){
  
  # formula
  fx_i = fx[ i, ]
  form_i = formula(as.vector(fx_i$formula))
  form_i
  # fit INLA model nested in tryCatch: time out after 2 hours (7200 secs)
  e = simpleError("error fitting")
  
  # data
  dd_i <- ddf %>% dplyr::mutate(y = total_cases)
  
  # fit model
  print("")
  print("==========================================")
  print(Sys.time())
  print("==========================================")
  print("")
  
  # set config to TRUE for posterior samples
  mod_i = tryCatch(
    fitINLAModel(form_i, dd_i, family="nbinomial", verbose=TRUE, config=TRUE, return.marginals=FALSE),
    error = function(e) return(e)
  )
  #poisson
  #nbinomial
  ## improve hyperpar estimates
  # mod_j = tryCatch(
  #   INLA::inla.hyperpar(mod_i, verbose=TRUE),
  #   error = function(e) return(e)
  #   )
  # if(class(mod_j)[1] != "simpleError"){
  #   mod_i = mod_j
  # }
  
  # write timeout to result
  if(class(mod_i)[1] == "simpleError"){
    
    ex = fx_i; ex$result = "error in fitting"
    ex_file_name = paste("fullmod_nb_err_", fx_i$modid, ".csv", sep="")
    write.csv(ex, paste(save_dir, "errors/", ex_file_name, sep=""), row.names=FALSE)
    
    # otherwise calculate and save fit metrics and model
  } else{
    
    fm = fitMetricsINLA(mod_i, data=dd_i, modname=fx_i$fx, inla.mode="experimental")
    res_i = cbind(fx_i, fm)
    fm_file_name = paste("fullmod_nb_fitmetrics_", fx_i$modid, ".csv", sep="")
    write.csv(res_i, paste(save_dir, "fitmetrics/", fm_file_name, sep=""), row.names=FALSE)
    
    # save model
    save(mod_i, file=paste(save_dir, "models/", fx_i$model_filename, sep=""))
  }
  
} # end of model fitting loop

#?control.inla
#inla.doc("rw1")
#inla.doc("bym")


load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_1.R")
m1 <- mod_i
summary(m1)

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_2.R")
m2 <- mod_i
summary(m2)

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_3.R")
m3 <- mod_i

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_4.R")
m4 <- mod_i

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_5.R")
m5 <- mod_i

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_6.R")
m6 <- mod_i

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_7.R")
m7 <- mod_i

load("./output/model_outputs/stempOOS_nb2/models/fullmod_nb_proportion_model_baseline.R")
baseline <- mod_i


# 函数计算贝叶斯 Pseudo R^2
calculate_pseudo_r2 <- function(model, data, response) {
  fitted_values <- model$summary.fitted.values$mean
  var_fitted <- var(fitted_values)
  residuals <- data[[response]] - fitted_values
  var_residuals <- var(residuals)
  pseudo_r2 <- var_fitted / (var_fitted + var_residuals)
  return(pseudo_r2)
}

pseudo_r2_full <- calculate_pseudo_r2(m1, ddf, "total_cases")

pseudo_r2_drop_geo <- calculate_pseudo_r2(m2, ddf, "total_cases")
pseudo_r2_geo <- calculate_pseudo_r2(m5, ddf, "total_cases")
pseudo_r2_full-pseudo_r2_drop_geo

pseudo_r2_drop_climate <- calculate_pseudo_r2(m3, ddf, "total_cases")
pseudo_r2_climate <- calculate_pseudo_r2(m6, ddf, "total_cases")
pseudo_r2_full-pseudo_r2_drop_climate

pseudo_r2_drop_social <- calculate_pseudo_r2(m4, ddf, "total_cases")
pseudo_r2_social <- calculate_pseudo_r2(m7, ddf, "total_cases")
pseudo_r2_full-pseudo_r2_drop_social

pseudo_r2_baseline <- calculate_pseudo_r2(baseline, ddf, "total_cases")
pseudo_r2_full-pseudo_r2_baseline


# 计算每类变量解释的变异百分比
percent_variance_geo <- ((pseudo_r2_full - pseudo_r2_drop_geo) / pseudo_r2_full) * 100
percent_variance_social <- ((pseudo_r2_full - pseudo_r2_drop_social) / pseudo_r2_full) * 100
percent_variance_climate <- ((pseudo_r2_full - pseudo_r2_drop_climate) / pseudo_r2_full) * 100
percent_variance_unexplained <- 100-percent_variance_geo-percent_variance_social-percent_variance_climate-percent_variance_random

# 打印结果
print(paste("The proportion of variance explained by geographical factors:", percent_variance_geo))
print(paste("The proportion of variance explained by social factors:", percent_variance_social))
print(paste("The proportion of variance explained by climate factors:", percent_variance_climate))
print(paste("The proportion of variance explained by random effects:", percent_variance_random))
print(paste("The proportion of unexplainable variance:", percent_variance_unexplained))



# # ================ plots =================

#######################################################################
percent_variance_explained = 100 - percent_variance_unexplained
data <- data.frame(
  Category = c("Explained", "Unexplained"),
  Percentage = c(percent_variance_explained, percent_variance_unexplained)
)

# 设置颜色
col_explained ="#9E9AC9"

# 扇区顺序
data$Category <- factor(data$Category, levels = c("Explained", "Unexplained"))

p1 <- ggplot(data, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(width = 1, stat = "identity", alpha = 0.65, color = c("#9E9AC9","grey"), size = 1.5) +  # 设置全局描边颜色和粗细
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Explained" = col_explained, 
                               "Unexplained" = "grey50")) +
  # 百分比标签，放在扇区中心
  geom_text(aes(label = sprintf("%.1f%%", Percentage/sum(Percentage)*100)),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  # 类别标签，放在扇区边缘，稍微向下和向外移动
  geom_text(aes(label = Category), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5,
            # 微调文本位置，避免与百分比标签重叠
            vjust = 2, hjust = 0.5) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Proportion of Explained and Residual Variance") +
  guides(fill = guide_legend(title = "Total Variance",override.aes = list(size = 5, alpha = 0.7)))  # 设置图例的透明度和大小

p1
##############################################################

data <- data.frame(
  Category = c("Geographical", "Social", "Climate", "Spatial Random-effects"),
  Percentage = c(percent_variance_geo, percent_variance_social, percent_variance_climate, percent_variance_random)
)

# 设置颜色
col_clim = viridis::viridis(200)[40]
col_socio= viridis::viridis(200)[105]
col_geo = viridis::magma(200)[150]

# 扇区顺序
data$Category <- factor(data$Category, levels = c("Geographical","Climate", "Social","Spatial Random-effects"))

# 创建饼图
p2 <- ggplot(data, aes(x = "", y = Percentage, fill = Category)) +
  #geom_bar(width = 1, stat = "identity", alpha = 0.65) +  # 设置透明度
  geom_bar(width = 1, stat = "identity", alpha = 0.65, color = c("#9E9AC9","#9E9AC9","#9E9AC9","#9E9AC9"), size = 1.5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Geographical" = col_geo, 
                               "Social" = col_socio, 
                               "Climate" = col_clim, 
                               "Spatial Random-effects" = col_random
                               )) +
  # 百分比标签，放在扇区中心
  geom_text(aes(label = sprintf("%.1f%%", Percentage/sum(Percentage)*100)),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  # 类别标签，放在扇区边缘，稍微向下和向外移动
  geom_text(aes(label = Category), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5,
            # 微调文本位置，避免与百分比标签重叠
            vjust = 2, hjust = 0.5) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Proportion of Variance Explained by Each type of Covariates") +
  guides(fill = guide_legend(title = "Explained Variance",override.aes = list(size = 5, alpha = 0.7)))  # 设置图例的透明度和大小

p2
###############################################################################


data <- data.frame(
  Category = c("Geographical", "Social", "Climate", "Spatial Random-effects", "Unexplained"),
  Percentage = c(percent_variance_geo, percent_variance_social, percent_variance_climate, percent_variance_random, percent_variance_unexplained)
)

# 设置颜色
col_clim = viridis::viridis(200)[40]
col_socio= viridis::viridis(200)[105]
col_geo = viridis::magma(200)[150]
col_random = viridis::magma(200)[195]

# 扇区顺序
data$Category <- factor(data$Category, levels = c("Geographical","Climate", "Social","Spatial Random-effects", "Unexplained"))

# 创建饼图
p3 <- ggplot(data, aes(x = "", y = Percentage, fill = Category)) +
  #geom_bar(width = 1, stat = "identity", alpha = 0.65) +  # 设置透明度
  geom_bar(width = 1, stat = "identity", alpha = 0.65, color = c("#9E9AC9","#9E9AC9","#9E9AC9","#9E9AC9","grey"), size = 1.5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Geographical" = col_geo, 
                               "Social" = col_socio, 
                               "Climate" = col_clim, 
                               "Spatial Random-effects" = col_random, 
                               "Unexplained" = "grey50")) +
  # 百分比标签，放在扇区中心
  geom_text(aes(label = sprintf("%.1f%%", Percentage/sum(Percentage)*100)),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  # 类别标签，放在扇区边缘，稍微向下和向外移动
  geom_text(aes(label = Category), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5,
            # 微调文本位置，避免与百分比标签重叠
            vjust = 2, hjust = 0.5) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Partition of Total Variance") +
  guides(fill = guide_legend(title = "Total Variance",override.aes = list(size = 5, alpha = 0.7)))  # 设置图例的透明度和大小

p3


p = gridExtra::grid.arrange(p1,p2,p3,nrow=1,widths=c("1","1.15","1.15") )
#ggsave(p, file="./output/figures/Figure5_Proportion_all.pdf", device="pdf", units="in", width=12, height=7)
#ggsave(p1, file="./output/figures/Figure5_Proportion1.pdf", device="pdf", units="in", width=8, height=6)
#ggsave(p2, file="./output/figures/Figure5_Proportion2.pdf", device="pdf", units="in", width=8, height=6)
#ggsave(p3, file="./output/figures/Figure5_Proportion3.pdf", device="pdf", units="in", width=8, height=6)
