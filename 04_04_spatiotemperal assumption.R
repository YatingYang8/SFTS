
# ==========================================================================================#
#                                                                                           #
#        EXAMPLE PIPELINE 3: FITS BASELINE AND FULL MODELS FOR SUBSET OF PROVINCES          #
#                        (USING THE STANDARD MODELLING SCRIPT)                              #
#              OUTPUTS SAVED TO "OUTPUT/MODEL_OUTPUTS/FULLMODEL_EXAMPLE"                    #
#                                                                                           #
# ==========================================================================================#

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
projname  =  "Bayesian nb model"
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")

region = "all"
region2 = NA
n_clim_bins = 40
plot_graph = FALSE
province_case_threshold = NA

source("00_build_model_df_clim.R")


# ================= customise dataframe for this modelling task ====================

dd = read.csv("./output/data_process/ModelData_sfts_All_withLags.csv",fileEncoding = "GB18030")

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
    Tmean_1m_g,
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

# ddf$region3x = as.integer(as.factor(ddf$region3))






# =================== create dataframe of models to fit and compare =====================
# bym2 model
# probability of SD of theta1 > 1 = 0.01
hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.5)))

hyper.bym3 = list(theta1 = list(prior="pc.prec", param=c(0.2, 0.05)),
                  theta2 = list(prior="pc", param=c(0.2, 0.8)))
hyper.bym4 = list(
  theta1 = list(prior="loggamma", param=c(1, 0.1)),
  theta2 = list(prior="loggamma", param=c(1, 0.5))
)

# iid model 
hyper.iid = list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# ar1 model
hyper.ar1 = list(theta1 = list(prior='pc.prec', param=c(0.5, 0.01)),
                 rho = list(prior='pc.cor0', param = c(0.5, 0.75)))

# bym model
hyper.bym = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                 theta2 = list(prior="pc.prec", param=c(1, 0.01)))


# rw1/rw2 model: three levels of constraint on precision parameter 
# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.05))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior="loggamma", param=c(1, 0.01)))



# =================== create dataframe of models to fit and compare =====================

# with a province level fixed effect to account for confounding effects of space
names(inla.models()$latent)
#inla.doc("seasonal")


form_base1 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name, replicate=yearx, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base2 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)"),
  collapse = " + "
)

form_base3 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(polyid, model='bym2', graph=nbmatrix_name, replicate=yearx, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base4 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(month, model='rw1', hyper=hyper1.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name, replicate=yearx, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym3)"),
  collapse = " + "
)



form_base5 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(month, model='rw1', hyper=hyper1.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym3)"),
  collapse = " + "
)

form_base6 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper2.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)


form_base7 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper2.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)


form_base8 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper2.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)"),
  collapse = " + "
)

form_base9 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper2.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)"),
  collapse = " + "
)

form_base = c(form_base1,form_base2,form_base3,form_base4,form_base5,form_base6,form_base7,form_base8,form_base9)

## parameter of model 7

form_base7 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper2.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base10 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper1.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base11 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(yearx, model='rw1', hyper=hyper3.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)


form_base12 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "yearx",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base13 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base14 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear"
  ),
  collapse = " + "
)


form_base=c(form_base1,form_base2,form_base3,form_base4,
            form_base5,form_base6,form_base7,form_base8,
            form_base9,form_base10,form_base11,form_base12,
            form_base13,form_base14)


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



m1 = paste(effect_names, collapse=" + "); 
name1 = "full"
fx = m1

##################
# create data frame
fx1 = data.frame(modid = 1:length(form_base),
                 fx = fx,
                 candidate = c("full"),
                 formula = paste(form_base, fx, sep=" + "))

bs1 = data.frame(modid = paste0("b", seq(1, length(form_base))), 
                 fx = "baseline", 
                 candidate="baseline", 
                 formula=form_base)

fx = rbind(fx1,bs1)

# model name
fx$model_filename = paste("fullmod_nb_model_", fx$modid, ".R", sep="")


write.csv(fx, file = paste(save_dir, "models/fx4.csv", sep=""))


# ================== fit models in an iterative loop ======================

# run model selection loop
for(i in i:nrow(fx)){
  #for(i in c(4,8)){
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
?inla
#?control.inla
inla.doc("rw1")
inla.doc("rw2")
inla.doc("bym")
inla.doc("bym2")
inla.doc("pc.prec")
inla.doc("iid")



########################## Fitmetrics ################################################
# full model and baseline model
rm(mod_i)
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_1.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_2.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_3.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_4.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_5.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_6.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_7.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_8.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_9.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_10.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_11.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_12.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_13.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_14.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b1.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b2.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b3.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b4.R")
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b5.R")
load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_1.R")
load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_13.R")
load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_14.R")

ddf$fitted.values <- mod_i$summary.fitted.values[, "mean"]
ddf$fitted.values_lower <- mod_i$summary.fitted.values[, "0.025quant"]
ddf$fitted.values_upper <- mod_i$summary.fitted.values[, "0.975quant"]


predicted_values = ddf$fitted.values
observed_values = ddf$total_cases
residuals = observed_values - predicted_values
plot(predicted_values, residuals, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)


plot(observed_values, predicted_values, 
     xlab = "Observed Values", ylab = "Predicted Values", 
     main = "Observed vs. Predicted")
abline(a = 0, b = 1, col = "blue", lty = 2)




pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 

pop_yr <- pop %>%
  group_by(year) %>%
  summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 

dd_yr <- ddf %>%
  #filter(year >2023) %>%
  group_by(year)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year))

dd_yr <- left_join(dd_yr,pop_yr)


dd_yr <- dd_yr %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year))



p <-ggplot(dd_yr, aes(x = year)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.values_lower, ymax = fitted.values_upper, fill = "Predicted Cases-SSP585"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = total_cases, color = "Actual Cases"), size = 1.5) +
  geom_point(aes(y = total_cases, color = "Actual Cases"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.values, color = "Predicted Cases"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.values, color = "Predicted Cases"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual Cases" = "red", 
    "Predicted Cases" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted Cases" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y = "Number of Cases",
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p


p2 <-ggplot(dd_yr, aes(x = year)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases-SSP585"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = incidence, color = "Actual incidence"), size = 1.5) +
  geom_point(aes(y = incidence, color = "Actual incidence"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.incidence, color = "Predicted incidence"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.incidence, color = "Predicted incidence"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual incidence" = "red", 
    "Predicted incidence" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted incidence" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y= expression(paste("Incidence of SFTS cases",(1/10^7))),
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p2




dd_yearmonth <- ddf %>%
  #filter(year >2023) %>%
  group_by(year,month, date)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))

dd_yearmonth <- left_join(dd_yearmonth, pop_yr)


dd_yearmonth <- dd_yearmonth %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))



p3 <-ggplot(dd_yearmonth, aes(x = date)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = incidence, color = "Actual incidence"), size = 1.5) +
  geom_point(aes(y = incidence, color = "Actual incidence"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.incidence, color = "Predicted incidence"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.incidence, color = "Predicted incidence"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual incidence" = "red", 
    "Predicted incidence" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted incidence" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y = "Incidence of SFTS cases (1/10^7)",
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p3




fit = data.frame(modname = "14",
                 dic = mod_i$dic$dic, 
                 waic = mod_i$waic$waic,
                 waic_neffp = mod_i$waic$p.eff,
                 #mae = mean(dx$abs_err, na.rm=TRUE),
                 lpml = mean(log(mod_i$cpo$cpo), na.rm = TRUE),
                 cpo_fail = sum(mod_i$cpo$failure == 1 & !is.na(mod_i$cpo$failure)))

#fit_all <- fit
fit_all <- rbind(fit_all,fit)

#write.csv(fit_all, file = paste(save_dir, "fitmetrics/fit_metrics.csv", sep=""))

pp1 <- p2
pp2 <- p2
pp3 <- p2




########################### Effect ###################################################

# full model and baseline model
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_14.R")
#summary(mod_i)
mx = mod_i
rm(mod_i)

load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_b14.R")
mb = mod_i
rm(mod_i)

### extractRandomINLA: extract random effect and rename columns

# if effect is grouped/replicated by a factor, automatically assign each subgroup to its grouping factor (labelled 1:n) 
# if BYM model, further partition into u and v components

#' @param summary_random points to model$summary.random$effect_of_interest
#' @param effect_name name to assign to fitted effect in dataframe (can be anything)
#' @param model_is_bym boolean; to specify if model is joint Besag-York-Mollie
#' @param transform specify whether to exponentiate coefficients (i.e. back transform to relative risk)
#' 
extractRandomINLA = function(summary_random, effect_name, model_is_bym=FALSE, transform=FALSE){
  
  # extract model effect
  rf = summary_random %>%
    dplyr::rename("value"=1, "lower"=4, "median"=5, "upper"=6)
  
  # label by grouping factor (if not replicated, group is 1 for all observations)
  rf$group = rep(1:as.vector(table(rf$value)[1]), each=n_distinct(rf$value))
  
  # partition BYM into u and v components
  if(model_is_bym){
    rf$component = rep(c("uv_joint", "u_besag"), each=n_distinct(rf$value)/2)
    rf$value = rep(1:(n_distinct(rf$value)/2), n_distinct(rf$group)*2)
  }
  
  # back transform if specified
  if(transform == TRUE){
    rf[ , 2:7 ] = exp(rf[ , 2:7])
  }
  
  # name and return
  rf$effect = effect_name
  return(rf)
}


### extractFixedINLA: extract fixed effects and rename columns

extractFixedINLA = function(model, model_name="mod", transform=FALSE){
  ff = model$summary.fixed
  ff$param = row.names(ff)
  ff$param[ ff$param == "(Intercept)" ] = "Intercept"
  names(ff)[3:5] = c("lower", "median", "upper")
  if(transform == TRUE){
    ff[ 1:5 ] = exp(ff[ 1:5 ])
  }
  ff
}




# ====================== Extract and save fitted climate effects and parameters ========================

# ranefs
ranefs = mx$summary.random
rf = lapply(ranefs[3:length(ranefs)], extractRandomINLA, effect_name = "x", transform=FALSE)
for(i in 1:length(rf)){
  rf[[i]]$effect = names(ranefs[ 3:length(ranefs)][i] )
}
rf = do.call(rbind.data.frame, rf)
row.names(rf) = c()
write.csv(rf, "./output/model_outputs/Bayesian nb model/fitted_params/fullmodel_fittedclimatefunctions_rw2.csv", row.names=FALSE)

# fixed effects
fixefs = extractFixedINLA(mx, model_name = "full_model") %>%
  dplyr::mutate(description = "slope parameter for scaled covariate")
row.names(fixefs) = c()
write.csv(fixefs, "./output/model_outputs/Bayesian nb model/fitted_params/fullmodel_fixedeffects.csv", row.names=FALSE)


# ======================== Visualise full results ===========================

# colours for different variable types
library(viridis)
col_clim = viridis::viridis(200)[40]
col_socio = viridis::viridis(200)[105]
col_geo = viridis::magma(200)[150]


# ranefs
ranefs = mx$summary.random
rf = lapply(ranefs[1:length(ranefs)], extractRandomINLA, effect_name = "x", transform=TRUE)
for(i in 1:length(rf)){
  rf[[i]]$effect = names(ranefs[ 1:length(ranefs)][i] )
}
rf = do.call(rbind.data.frame, rf)

##delete
# numerical_vars <- rf %>% dplyr::select_if(is.numeric) %>% names()
# rf <- rf %>%
#   mutate(across(all_of(numerical_vars[4:6]), ~ifelse(effect == "Rh_3m_g", .x + 0.1, .x))) %>%
#   mutate(across(all_of(numerical_vars[4:6]), ~ifelse(effect == "out_migration_g", .x + 0.10, .x)))

# extract ranefs and create standardised plots
effs = unique(rf$effect)
effs
plots = vector("list", length(effs))
plotRF = function(x){
  
  rr=rf[ rf$effect == effs[x], ]
  
  #distinguish climate and social
  if(effs[x] %in% c("Tmean_1m_g", "Precipitation_2m_g", "Wind_2m_g","Rh_3m_g","Sun_01m_g")){
    colx = col_clim
  } else if(effs[x] %in% c("out_migration_g")){
    colx = col_socio
  }else {
    colx = col_geo
  }
  
  # if(effs[x] == "tmean_1m_g"){
  #   # rr$upper[ rr$value < 14 ] = NA
  #   # rr$lower[ rr$value < 14 ] = NA
  #   rr = rr %>% dplyr::filter(value >= quantile(ddf$tmean_1m, 0.001))
  # }
  
  rr = left_join(
    rr,
    data.frame(
      effect = c("month","polyid","Tmean_1m_g", "Precipitation_2m_g", "Rh_3m_g","Sun_01m_g","out_migration_g", "Grassland_g","Barren_g","Shrub_g"),
      effname = c("month","polyid","Mean temperature (1−month lag)", "Precipitation (2−month lag)",  "Relative humidity (6−month accumulative)", "Sun duration (1−month accumulative)","Migration index","Grassland coverage","Barren coverage","Shrub coverage"),
      type = c("month","polyid","Climate", "Climate", "Climate", "Climate", "Mobility", "Geography","Geography","Geography"),
      units = c("month","polyid","Mean Temperature (°C)", "Precipitation (mm)", "Rh (%)", "Sun (hours)", "Migration index","Grassland (km2)","Barren (km2)","Shrub (km2)" )
    )
  )
  
  # visualise plot
  px = ggplot(rr) + 
    geom_line(aes(value, median)) +
    geom_ribbon(aes(value, ymin=lower, ymax=upper), alpha=0.2, fill=colx) +
    geom_hline(yintercept=1, lty=2) +
    #facet_wrap(~effname, scales="free") +
    theme_classic() +
    ggtitle(rr$effname[1]) + 
    ylab("Relative risk") + xlab(rr$units[1]) +
    theme(plot.title = element_text(size=14.25, hjust=0.5),
          axis.text = element_text(size=12.25),
          axis.title = element_text(size=13))
  
  
  #add density of climate vars
  if(effs[x] == "Tmean_1m_g"){
    densx = dd %>%
      dplyr::filter(Tmean >= min(rr$value) & Tmean <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Tmean), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Precipitation_2m_g"){
    densx = dd %>%
      dplyr::filter(Precipitation >= min(rr$value) & Precipitation <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Precipitation), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Wind_2m_g"){
    densx = dd %>%
      dplyr::filter(Wind >= min(rr$value) & Wind <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Precipitation), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Rh_3m_g"){
    densx = dd %>%
      dplyr::filter(Rh >= min(rr$value) & Rh <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Rh), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Sun_01m_g"){
    densx = dd %>%
      dplyr::filter(Sun >= min(rr$value) & Sun <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Sun), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "out_migration_g"){
    densx = dd %>%
      dplyr::filter(out_migration_norm2 >= min(rr$value) & out_migration_norm2 <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(out_migration_norm2), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Grassland_g"){
    densx = dd %>%
      dplyr::filter(Grassland >= min(rr$value) & Grassland <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.25, col="grey70", alpha=0.4) +
      geom_density(aes(Grassland), fill=colx, alpha=0.2, size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=12.25, hjust=0.5),
            axis.text.y = element_text(size=10.25),
            axis.text.x = element_text(size=10.25),
            axis.title = element_text(size=11),
            axis.title.y = element_text(size=10, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Barren_g"){
    densx = dd %>%
      dplyr::filter(Barren >= min(rr$value) & Barren <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Barren), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Shrub_g"){
    densx = dd %>%
      dplyr::filter(Shrub >= min(rr$value) & Shrub <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Shrub), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  # return
  plots[[x]] = px
}


prf = lapply(1:length(effs), plotRF)




effs = unique(rf$effect)
effs
rr=rf[ rf$effect == effs[2], ]

rr = left_join(
  rr,
  data.frame(
    effect = c("month","polyid","Tmean_1m_g", "Precipitation_2m_g", "Rh_3m_g","Sun_01m_g","out_migration_g", "Grassland_g","Barren_g","Shrub_g"),
    effname = c("month","polyid","Mean temperature (1−month lag)", "Precipitation (2−month lag)",  "Relative humidity (6−month accumulative)", "Sun duration (1−month accumulative)","Migration index","Grassland coverage","Barren coverage","Shrub coverage"),
    type = c("month","polyid","Climate", "Climate", "Climate", "Climate", "Mobility", "Geography","Geography","Geography"),
    units = c("month","polyid","Mean Temperature (°C)", "Precipitation (mm)", "Rh (%)", "Sun (hours)", "Migration index","Grassland (km2)","Barren (km2)","Shrub (km2)" )
  )
)

rr = left_join(
  rr,
  data.frame(
    effect = c("yearx","month","polyid","Tmean_1m_g", "Precipitation_2m_g", "Rh_3m_g","Sun_01m_g","out_migration_g", "Grassland_g","Barren_g","Shrub_g"),
    effname = c("yearx","month","polyid","Mean temperature (1−month lag)", "Precipitation (2−month lag)",  "Relative humidity (6−month accumulative)", "Sun duration (1−month accumulative)","Migration index","Grassland coverage","Barren coverage","Shrub coverage"),
    type = c("yearx","month","polyid","Climate", "Climate", "Climate", "Climate", "Mobility", "Geography","Geography","Geography"),
    units = c("yearx","month","polyid","Mean Temperature (°C)", "Precipitation (mm)", "Rh (%)", "Sun (hours)", "Migration index","Grassland (km2)","Barren (km2)","Shrub (km2)" )
  )
)

# visualise plot
px = ggplot(rr) + 
  geom_line(aes(value, median)) +
  geom_ribbon(aes(value, ymin=lower, ymax=upper), alpha=0.2, fill="grey") +
  geom_hline(yintercept=1, lty=2) +
  #facet_wrap(~effname, scales="free") +
  theme_classic() +
  ggtitle(rr$effname[1]) + 
  ylab("Relative risk") + xlab(rr$units[1]) +
  theme(plot.title = element_text(size=14.25, hjust=0.5),
        axis.text = element_text(size=12.25),
        axis.title = element_text(size=13))

px

library(ggh4x)

# fixed effects plot
pfx = extractFixedINLA(mx, model_name="mod", transform=TRUE)
pfx$param

pfx = extractFixedINLA(mx, model_name="mod", transform=TRUE) %>%
  dplyr::filter(param != "Intercept") %>%
  dplyr::left_join(
    data.frame(
      param=c("urban_log", "transportation_log", "Cropland_log",  "elevation_log", "Water_log"),
      paramname = c("Urban\nexpansion\n(log)", "Transportation\n(log km)", "Cropland\n(log km^2)","Elevation\n(log km)","Water\n(log km^2)"),
      type = c("Social", "Social", "Geographical", "Geographical", "Geographical"),
      facet = c("Slope (risk ratio) ", "Slope (risk ratio)", "Slope (risk ratio)", "Slope (risk ratio)", "Slope (risk ratio)")
    )
  ) %>%
  dplyr::mutate(paramname = factor(paramname, levels=c("Urban\nexpansion\n(log)", "Transportation\n(log km)", "Cropland\n(log km^2)", "Elevation\n(log km)", "Water\n(log km^2)"), ordered=TRUE)
  ) %>%
  ggplot() +
  geom_point(aes(paramname, mean, col=type), size=2) +
  geom_linerange(aes(paramname, ymin=lower, ymax=upper, col=type), show.legend=FALSE) +
  geom_hline(yintercept=1, lty=2) +
  theme_classic() +
  ylab("Slope (risk ratio)") + 
  xlab("") + 
  theme(plot.title = element_text(size=14.25, hjust=0.5),
        axis.text = element_text(size=12.25),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)) +
  #scale_y_continuous(limits=c(0.7, 1.8), breaks=c(0.75, 1, 1.25, 1.5, 1.75), labels=c(0.75, 1, 1.25, 1.5, 1.75)) +
  #scale_color_viridis_d(begin=0.2, end=0.6) + 
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_text(angle=0, size=11),
  #       axis.title.y = element_blank(),
  #       #axis.title.y = element_text(size=11),
  #       axis.text.y = element_text(size=11)) +
  #ggtitle("Waffle\nwaffles") +
  theme(#strip.text = element_blank(),
    strip.text = element_text(size=11),
    #plot.title = element_text(size=12.15, color="white"),
    strip.background = element_blank(),
    strip.placement="outside",
    legend.text = element_text(size=12), legend.title = element_blank(), #legend.background=element_rect(fill=NA, color="grey70"),
    legend.position=c(0.2, 0.9)) + guides(colour = guide_legend(override.aes = list(size=5, alpha=0.8)))


pfx


# row 1: fixed + socioenv
pc1 = gridExtra::grid.arrange(pfx, 
                              prf[[1]], 
                              prf[[2]],
                              prf[[3]], 
                              prf[[4]],
                              prf[[5]],
                              prf[[6]],
                              prf[[7]],
                              prf[[8]],
                              #prf[[9]],
                              nrow=4, widths=c(1.1, 1.05, 1.05, 1.05))
#ggsave(pc1, file="./output/figures/Figure3_effect_true3.pdf", device="pdf", width=15.5, height=9, units="in")








#################################################################################
########################## Plots for validation results #########################
#################################################################################

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
projname  =  "Bayesian nb model"
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")

region = "all"
region2 = NA
n_clim_bins = 40
plot_graph = FALSE
province_case_threshold = NA

source("00_build_model_df_clim.R")

#county level:nbmatrix_name; city level:nbmatrix_name2; baidu migration: nbmatrix_name1
nbmatrix_name = nbmatrix_name1
nbmatrix_name

# ================= customise dataframe for this modelling task ====================

dd = read.csv("./output/data_process/ModelData_sfts_All_withLags.csv",fileEncoding = "GB18030")

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
    Tmean_1m_g,
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

# ddf$region3x = as.integer(as.factor(ddf$region3))


load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_1.R")


ddf$fitted.values <- mod_i$summary.fitted.values[, "mean"]
ddf$fitted.values_lower <- mod_i$summary.fitted.values[, "0.025quant"]
ddf$fitted.values_upper <- mod_i$summary.fitted.values[, "0.975quant"]


predicted_values = ddf$fitted.values
observed_values = ddf$total_cases
residuals = observed_values - predicted_values
plot(predicted_values, residuals, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)


plot(observed_values, predicted_values, 
     xlab = "Observed Values", ylab = "Predicted Values", 
     main = "Observed vs. Predicted")
abline(a = 0, b = 1, col = "blue", lty = 2)




pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 

pop_yr <- pop %>%
  group_by(year) %>%
  summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 

dd_yr <- ddf %>%
  #filter(year >2023) %>%
  group_by(year)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year))

dd_yr <- left_join(dd_yr,pop_yr)


dd_yr <- dd_yr %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year))


p2 <-ggplot(dd_yr, aes(x = year)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases-SSP585"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = incidence, color = "Actual incidence"), size = 1.5) +
  geom_point(aes(y = incidence, color = "Actual incidence"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.incidence, color = "Predicted incidence"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.incidence, color = "Predicted incidence"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual incidence" = "red", 
    "Predicted incidence" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted incidence" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y= expression(paste("Incidence of SFTS cases",(1/10^7))),
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p2


fit = data.frame(modname = "1",
                 dic = mod_i$dic$dic, 
                 waic = mod_i$waic$waic,
                 waic_neffp = mod_i$waic$p.eff,
                 #mae = mean(dx$abs_err, na.rm=TRUE),
                 lpml = mean(log(mod_i$cpo$cpo), na.rm = TRUE),
                 cpo_fail = sum(mod_i$cpo$failure == 1 & !is.na(mod_i$cpo$failure)))

fit_all <- fit

#write.csv(fit_all, file = paste(save_dir, "fitmetrics/fit_metrics.csv", sep=""))

aa1 <- p2




load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_13.R")

ddf$fitted.values <- mod_i$summary.fitted.values[, "mean"]
ddf$fitted.values_lower <- mod_i$summary.fitted.values[, "0.025quant"]
ddf$fitted.values_upper <- mod_i$summary.fitted.values[, "0.975quant"]


predicted_values = ddf$fitted.values
observed_values = ddf$total_cases
residuals = observed_values - predicted_values
plot(predicted_values, residuals, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)


plot(observed_values, predicted_values, 
     xlab = "Observed Values", ylab = "Predicted Values", 
     main = "Observed vs. Predicted")
abline(a = 0, b = 1, col = "blue", lty = 2)




pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 

pop_yr <- pop %>%
  group_by(year) %>%
  summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 

dd_yr <- ddf %>%
  #filter(year >2023) %>%
  group_by(year)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year))

dd_yr <- left_join(dd_yr,pop_yr)


dd_yr <- dd_yr %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year))


p2 <-ggplot(dd_yr, aes(x = year)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases-SSP585"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = incidence, color = "Actual incidence"), size = 1.5) +
  geom_point(aes(y = incidence, color = "Actual incidence"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.incidence, color = "Predicted incidence"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.incidence, color = "Predicted incidence"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual incidence" = "red", 
    "Predicted incidence" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted incidence" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y= expression(paste("Incidence of SFTS cases",(1/10^7))),
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p2


fit = data.frame(modname = "13",
                 dic = mod_i$dic$dic, 
                 waic = mod_i$waic$waic,
                 waic_neffp = mod_i$waic$p.eff,
                 #mae = mean(dx$abs_err, na.rm=TRUE),
                 lpml = mean(log(mod_i$cpo$cpo), na.rm = TRUE),
                 cpo_fail = sum(mod_i$cpo$failure == 1 & !is.na(mod_i$cpo$failure)))

#fit_all <- fit
fit_all <- rbind(fit_all,fit)
aa2 <- p2




load("./output/model_outputs/Bayesian nb model/models/新建文件夹/fullmod_nb_model_14.R")


ddf$fitted.values <- mod_i$summary.fitted.values[, "mean"]
ddf$fitted.values_lower <- mod_i$summary.fitted.values[, "0.025quant"]
ddf$fitted.values_upper <- mod_i$summary.fitted.values[, "0.975quant"]


predicted_values = ddf$fitted.values
observed_values = ddf$total_cases
residuals = observed_values - predicted_values
plot(predicted_values, residuals, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)


plot(observed_values, predicted_values, 
     xlab = "Observed Values", ylab = "Predicted Values", 
     main = "Observed vs. Predicted")
abline(a = 0, b = 1, col = "blue", lty = 2)




pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 

pop_yr <- pop %>%
  group_by(year) %>%
  summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 

dd_yr <- ddf %>%
  #filter(year >2023) %>%
  group_by(year)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year))

dd_yr <- left_join(dd_yr,pop_yr)


dd_yr <- dd_yr %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year))


p2 <-ggplot(dd_yr, aes(x = year)) +
  # 置信区间条带
  geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases-SSP585"), alpha = 0.1) +
  
  # 历史数据的实线和圆圈
  geom_line(aes(y = incidence, color = "Actual incidence"), size = 1.5) +
  geom_point(aes(y = incidence, color = "Actual incidence"), size = 3, shape = 16) +
  geom_line(aes(y = fitted.incidence, color = "Predicted incidence"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = fitted.incidence, color = "Predicted incidence"), size = 2.8, shape = 16) +
  
  scale_color_manual(values = c(
    "Actual incidence" = "red", 
    "Predicted incidence" = "dodgerblue")) +
  scale_fill_manual(values = c(
    "Predicted incidence" = "dodgerblue")) +
  
  # 图例标签
  labs(title = "",
       x = "Year",
       y= expression(paste("Incidence of SFTS cases",(1/10^7))),
       color = "Data Type",
       fill = "Data Type") +
  # 主题
  theme_classic() +
  theme(
    legend.position = c(0.2,0.6),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列

p2


fit = data.frame(modname = "14",
                 dic = mod_i$dic$dic, 
                 waic = mod_i$waic$waic,
                 waic_neffp = mod_i$waic$p.eff,
                 #mae = mean(dx$abs_err, na.rm=TRUE),
                 lpml = mean(log(mod_i$cpo$cpo), na.rm = TRUE),
                 cpo_fail = sum(mod_i$cpo$failure == 1 & !is.na(mod_i$cpo$failure)))

#fit_all <- fit
fit_all <- rbind(fit_all,fit)
aa3 <- p2

ggsave(aa1, file="./output/figures/FigureS6_a1.tif", device="tiff", units="in", width=6, height=4.5, dpi=300)
ggsave(aa2, file="./output/figures/FigureS6_a2.tif", device="tiff", units="in", width=6, height=4.5, dpi=300)
ggsave(aa3, file="./output/figures/FigureS6_a3.tif", device="tiff", units="in", width=6, height=4.5, dpi=300)



print(fit_all)













############################ cross validation ##################################

oos_results <- read.csv(".\\output\\model_outputs\\stempOOS_nb1\\models\\1_output_1_969167_stempOOS_nb_model_1_.csv")  #log year+f(region)

oos_results <- read.csv(".\\output\\model_outputs\\stempOOS_nb2\\models\\1_output_1_969167_stempOOS_nb_model_1_.csv")  #log year

oos_results <- read.csv(".\\output\\model_outputs\\spOOs_previous\\1_output_66_656850_spOOS_nb_model_1_.csv") #f(year)+f(month)+f(region)
oos_results <- read.csv(".\\output\\model_outputs\\spOOs_previous\\1_output_79_727708_spOOS_nb_model_1_.csv")
oos_results <- read.csv(".\\output\\model_outputs\\spOOs_previous\\1_output_92_756296_spOOS_nb_model_1_.csv")

oos_results$predicted = exp(oos_results$logpop + oos_results$mean)
oos_results$resid = oos_results$predicted - oos_results$total_cases

head(oos_results)

sum(oos_results$total_cases)
sum(oos_results$predicted)
# calculate summary stats
mae_oos = mean(abs(oos_results$resid), na.rm=TRUE)
mae_oos
rmse_oos = sqrt(mean(oos_results$resid^2, na.rm=TRUE))
rmse_oos
correlation <- cor(oos_results$total_cases, oos_results$predicted)
correlation
cor.test(oos_results$total_cases, oos_results$predicted)









################# prediction with varing training set ###########################
PATH = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(PATH)

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
projname  =  "Validation"
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")

region = "all"
region2 = NA
n_clim_bins = 40
plot_graph = FALSE
province_case_threshold = NA

source("00_build_model_df_clim.R")

#county level:nbmatrix_name; city level:nbmatrix_name2; baidu migration: nbmatrix_name1
#nbmatrix_name = nbmatrix_name1
nbmatrix_name=nbmatrix_name1


####################### SSP 245 #################################################
#############  Using data from 2011-2023 to predict for 2024-2028 ###############


predic_func <- function(form_base,form_name, trainyear1,trainyear2,valyear1,valyear2){
  
  load(file = "output/data_process/proj_df_final4.RData")
  
  names(proj_df_final4)
  
  #choose the climate change scenarios
  proj_df_final4 <- proj_df_final4 %>%
    mutate(Tmean_1m_g = ifelse(year <= 2023, Tmean_1m_g, Tmean_ssp245))
  
  # 数据处理
  dd = proj_df_final4 %>%
    dplyr::mutate(
      Tmean_1m_g = inla.group(Tmean_1m_g, n=30),
      Rh_3m_g =inla.group(Rh_3m_g, n=30),
      Sun_01m_g  =inla.group(Sun_01m_g, n=30),
      in_migration_g = inla.group(in_migration_g, n=30),
      Grassland_g = inla.group(Grassland_g, n=30),
      Shrub_g = inla.group(Shrub_g, n=30),
      elevation_g = inla.group(elevation_g, n=30),
      yearx = as.integer(as.factor(year)) 
    )%>%  #每个连续变量都要记得inla.group否则会出错
    filter(year %in% c(trainyear1:valyear2)) %>%
    mutate( logyear = log(yearx))
  
  dd$polyid <- dd$polyid_city 
  
  # 定义训练和验证数据集
  train_data = dd %>% filter(year %in% c(trainyear1:trainyear2))
  val_data = dd %>% filter(year %in% c(valyear1:valyear2))
  
  # 创建用于建模的数据框，响应变量和家庭
  train_data <- train_data %>% mutate(y = total_cases)
  val_data <- val_data %>% mutate(y = NA)
  
  # 
  
  form_base1 = paste(
    c("y ~ 1",
      "offset(logpop)",
      "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
      "f(polyid, model='bym2', graph=nbmatrix_name, replicate=yearx, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
    collapse = " + "
  )
  
  
  form_base2 = paste(
    c("y ~ 1",
      "offset(logpop)",
      "logyear",
      "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
    collapse = " + "
  )
  
  form_base3 = paste(
    c("y ~ 1",
      "offset(logpop)",
      "logyear"
    ),
    collapse = " + "
  )
  
  
  # 完整模型的效果名称
  effect_names = 
    c("Precipitation_02m_log",
      "urban_log",
      "transportation_log",
      "Cropland_log",
      "f(Tmean_1m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE) ",
      "f(Rh_3m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
      "f(Sun_01m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
      "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
      "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
      "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
      "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"
    )
  
  #form_base=form_base1
  
  # 公式
  form_full = paste(form_base, paste(effect_names, collapse=" + "), sep=" + ")
  
  
  
  
  # 创建 INLA stack 对象
  stack_train = inla.stack(data = list(y = train_data$y),
                           A = list(1),
                           effects = list(train_data[, !names(train_data) %in% "y"]), ##前面已经有y，所以这个数据集里不能出现y否则重名会出错
                           tag = "train")
  
  stack_val = inla.stack(data = list(y = val_data$y),
                         A = list(1),
                         effects = list(val_data[,!names(val_data) %in% "y"]),
                         tag = "val")
  
  # 合并 stack 对象
  stack = inla.stack(stack_train, stack_val)
  
  # 模型拟合
  mod_i = inla(
    formula = as.formula(form_full),
    data = inla.stack.data(stack),
    family = "nbinomial",
    control.fixed = list(mean.intercept=0, 
                         prec.intercept=0.3, # precision 1
                         mean=0, 
                         prec=0.3), # weakly regularising on fixed effects (sd of 1)
    control.predictor = list(A = inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE, return.marginals = FALSE),
    verbose = TRUE
  )
  
  
  save(mod_i, file=paste(save_dir, "models/", form_name,"_",trainyear2,"_",valyear2,".R", sep=""))
  
  # 获取预测值和置信区间
  ####### calculate the 95%CI for fitted cases ###################################
  
  load(file=paste(save_dir, "models/", form_name,"_",trainyear2,"_",valyear2,".R", sep=""))
  
  index_val = inla.stack.index(stack, "val")$data
  
  predictions = mod_i$summary.fitted.values[index_val, "mean"]
  val_data$fitted.values <- predictions
  
  predictions_95lower = mod_i$summary.fitted.values[index_val, "0.025quant"]
  val_data$fitted.values_lower <- predictions_95lower
  
  
  predictions_95upper = mod_i$summary.fitted.values[index_val, "0.975quant"]
  val_data$fitted.values_upper <- predictions_95upper 
  
  
  
  
  index_val = inla.stack.index(stack, "train")$data
  
  predictions = mod_i$summary.fitted.values[index_val, "mean"]
  train_data$fitted.values <- predictions
  
  predictions_95lower = mod_i$summary.fitted.values[index_val, "0.025quant"]
  train_data$fitted.values_lower <- predictions_95lower
  
  predictions_95upper = mod_i$summary.fitted.values[index_val, "0.975quant"]
  train_data$fitted.values_upper <- predictions_95upper 
  
  Temp_proj_fitted_result <- rbind(train_data,val_data)
  
  save(Temp_proj_fitted_result, file=paste(save_dir, form_name,"_","train",trainyear2,"_val",valyear2,".RData", sep=""))
  
}




form_base1 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "f(month, model='rw1', hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=cityx)",
    "f(polyid, model='bym2', graph=nbmatrix_name, replicate=yearx, scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)


form_base2 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear",
    "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  collapse = " + "
)

form_base3 = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear"
  ),
  collapse = " + "
)

predic_func(form_base1,form_name="form1",2011,2023,2024,2028)
predic_func(form_base1,form_name="form1",2011,2022,2023,2027)
predic_func(form_base1,form_name="form1",2011,2021,2022,2026)
predic_func(form_base1,form_name="form1",2011,2020,2021,2025)

predic_func(form_base2,form_name="form2",2011,2023,2024,2028)
predic_func(form_base2,form_name="form2",2011,2022,2023,2027)
predic_func(form_base2,form_name="form2",2011,2021,2022,2026)
predic_func(form_base2,form_name="form2",2011,2020,2021,2025)

predic_func(form_base3,form_name="form3",2011,2023,2024,2028)
predic_func(form_base3,form_name="form3",2011,2022,2023,2027)
predic_func(form_base3,form_name="form3",2011,2021,2022,2026)
predic_func(form_base3,form_name="form3",2011,2020,2021,2025)


############plots#######

filepaths1 <- c("./output/model_outputs/Validation/form1_train2023_val2028.RData",
                "./output/model_outputs/Validation/form1_train2022_val2027.RData",
                "./output/model_outputs/Validation/form1_train2021_val2026.RData",
                "./output/model_outputs/Validation/form1_train2020_val2025.RData")

filepaths2 <- c("./output/model_outputs/Validation/form2_train2023_val2028.RData",
                "./output/model_outputs/Validation/form2_train2022_val2027.RData",
                "./output/model_outputs/Validation/form2_train2021_val2026.RData",
                "./output/model_outputs/Validation/form2_train2020_val2025.RData")

filepaths3 <- c("./output/model_outputs/Validation/form3_train2023_val2028.RData",
                "./output/model_outputs/Validation/form3_train2022_val2027.RData",
                "./output/model_outputs/Validation/form3_train2021_val2026.RData",
                "./output/model_outputs/Validation/form3_train2020_val2025.RData")



plot_trainingsets_func <- function(filepaths){
  
  load(filepaths[1])
  
  dd_yr <- Temp_proj_fitted_result %>%
    #filter(year >2023) %>%
    group_by(year)%>%
    summarise(total_cases = sum(total_cases, na.rm = TRUE),
              fitted.values = sum(fitted.values, na.rm = TRUE),
              fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
              fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
    ) %>%
    mutate(year = as.integer(year))
  
  
  pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 
  
  pop_yr <- pop %>%
    group_by(year) %>%
    summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 
  
  
  dd_yr <- left_join(dd_yr,pop_yr)
  
  dd_yr <- dd_yr %>%
    mutate(population = ifelse(year <= 2023, population, 6627))
  
  
  dd_yr <- dd_yr %>%
    mutate(incidence = (total_cases/population) *1000, 
           fitted.incidence = (fitted.values/population) *1000, 
           fitted.incidence_lower = (fitted.values_lower/population) *1000, 
           fitted.incidence_upper = (fitted.values_upper/population) *1000
    ) %>%
    mutate(year = as.integer(year))
  
  
  dd_yr$incidence[dd_yr$incidence == 0] <- NA
  
  p1 <-ggplot(dd_yr, aes(x = year)) +
    # 置信区间条带
    geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases"), alpha = 0.1) +
    
    # 历史数据的实线和圆圈
    geom_line(aes(y = incidence, color = "Actual Cases"), size = 1.5) +
    geom_point(aes(y = incidence, color = "Actual Cases"), size = 3, shape = 16) +
    geom_line(aes(y = fitted.incidence, color = "Predicted Cases"), linetype = "dashed", size = 1.2) +
    geom_point(aes(y = fitted.incidence, color = "Predicted Cases"), size = 2.8, shape = 16) +
    geom_vline(xintercept = 2023, linetype = "dashed", color = "red") +
    
    scale_color_manual(values = c(
      "Actual Cases" = "#9654e5", 
      "Predicted Cases" = "#e7298a")) +
    scale_fill_manual(values = c(
      "Predicted Cases" = "#e7298a")) +
    
    # 图例标签
    labs(title = "Using data from 2011-2023 to predict for 2024-2028",
         x = "Year",
         y = expression(paste("Incidence of SFTS cases (", 1/10^7, ")")),
         color = "Data Type",
         fill = "Data Type") +
    # 主题
    theme_classic() +
    theme(
      legend.position = c(0.2,0.6),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
    guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列
  
  p1
  
  
  
  ######
  
  
  load(filepaths[2])
  
  dd_yr <- Temp_proj_fitted_result %>%
    #filter(year >2023) %>%
    group_by(year)%>%
    summarise(total_cases = sum(total_cases, na.rm = TRUE),
              fitted.values = sum(fitted.values, na.rm = TRUE),
              fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
              fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
    ) %>%
    mutate(year = as.integer(year))
  
  
  pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 
  
  pop_yr <- pop %>%
    group_by(year) %>%
    summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 
  
  
  dd_yr <- left_join(dd_yr,pop_yr)
  
  dd_yr <- dd_yr %>%
    mutate(population = ifelse(year <= 2023, population, 6627))
  
  
  dd_yr <- dd_yr %>%
    mutate(incidence = (total_cases/population) *1000, 
           fitted.incidence = (fitted.values/population) *1000, 
           fitted.incidence_lower = (fitted.values_lower/population) *1000, 
           fitted.incidence_upper = (fitted.values_upper/population) *1000
    ) %>%
    mutate(year = as.integer(year))
  
  
  dd_yr$incidence[dd_yr$incidence == 0] <- NA
  
  p2 <-ggplot(dd_yr, aes(x = year)) +
    # 置信区间条带
    geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases"), alpha = 0.1) +
    
    # 历史数据的实线和圆圈
    geom_line(aes(y = incidence, color = "Actual Cases"), size = 1.5) +
    geom_point(aes(y = incidence, color = "Actual Cases"), size = 3, shape = 16) +
    geom_line(aes(y = fitted.incidence, color = "Predicted Cases"), linetype = "dashed", size = 1.2) +
    geom_point(aes(y = fitted.incidence, color = "Predicted Cases"), size = 2.8, shape = 16) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "red") +
    
    scale_color_manual(values = c(
      "Actual Cases" = "#9654e5", 
      "Predicted Cases" = "#e7298a")) +
    scale_fill_manual(values = c(
      "Predicted Cases" = "#e7298a")) +
    
    # 图例标签
    labs(title = "Using data from 2011-2022 to predict for 2023-2027",
         x = "Year",
         y = expression(paste("Incidence of SFTS cases (", 1/10^7, ")")),
         color = "Data Type",
         fill = "Data Type") +
    # 主题
    theme_classic() +
    theme(
      legend.position = c(0.2,0.6),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
    guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列
  
  p2
  
  
  
  
  ######
  
  
  load(filepaths[3])
  
  dd_yr <- Temp_proj_fitted_result %>%
    #filter(year >2023) %>%
    group_by(year)%>%
    summarise(total_cases = sum(total_cases, na.rm = TRUE),
              fitted.values = sum(fitted.values, na.rm = TRUE),
              fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
              fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
    ) %>%
    mutate(year = as.integer(year))
  
  
  pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 
  
  pop_yr <- pop %>%
    group_by(year) %>%
    summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 
  
  
  dd_yr <- left_join(dd_yr,pop_yr)
  
  dd_yr <- dd_yr %>%
    mutate(population = ifelse(year <= 2023, population, 6627))
  
  
  dd_yr <- dd_yr %>%
    mutate(incidence = (total_cases/population) *1000, 
           fitted.incidence = (fitted.values/population) *1000, 
           fitted.incidence_lower = (fitted.values_lower/population) *1000, 
           fitted.incidence_upper = (fitted.values_upper/population) *1000
    ) %>%
    mutate(year = as.integer(year))
  
  
  dd_yr$incidence[dd_yr$incidence == 0] <- NA
  
  p3 <-ggplot(dd_yr, aes(x = year)) +
    # 置信区间条带
    geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases"), alpha = 0.1) +
    
    # 历史数据的实线和圆圈
    geom_line(aes(y = incidence, color = "Actual Cases"), size = 1.5) +
    geom_point(aes(y = incidence, color = "Actual Cases"), size = 3, shape = 16) +
    geom_line(aes(y = fitted.incidence, color = "Predicted Cases"), linetype = "dashed", size = 1.2) +
    geom_point(aes(y = fitted.incidence, color = "Predicted Cases"), size = 2.8, shape = 16) +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "red") +
    
    scale_color_manual(values = c(
      "Actual Cases" = "#9654e5", 
      "Predicted Cases" = "#e7298a")) +
    scale_fill_manual(values = c(
      "Predicted Cases" = "#e7298a")) +
    
    # 图例标签
    labs(title = "Using data from 2011-2021 to predict for 2022-2026",
         x = "Year",
         y = expression(paste("Incidence of SFTS cases (", 1/10^7, ")")),
         color = "Data Type",
         fill = "Data Type") +
    # 主题
    theme_classic() +
    theme(
      legend.position = c(0.2,0.6),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
    guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列
  
  p3
  
  
  
  
  ######
  
  
  load(filepaths[4])
  
  dd_yr <- Temp_proj_fitted_result %>%
    #filter(year >2023) %>%
    group_by(year)%>%
    summarise(total_cases = sum(total_cases, na.rm = TRUE),
              fitted.values = sum(fitted.values, na.rm = TRUE),
              fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
              fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
    ) %>%
    mutate(year = as.integer(year))
  
  
  pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 
  
  pop_yr <- pop %>%
    group_by(year) %>%
    summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 
  
  
  dd_yr <- left_join(dd_yr,pop_yr)
  
  dd_yr <- dd_yr %>%
    mutate(population = ifelse(year <= 2023, population, 6627))
  
  
  dd_yr <- dd_yr %>%
    mutate(incidence = (total_cases/population) *1000, 
           fitted.incidence = (fitted.values/population) *1000, 
           fitted.incidence_lower = (fitted.values_lower/population) *1000, 
           fitted.incidence_upper = (fitted.values_upper/population) *1000
    ) %>%
    mutate(year = as.integer(year))
  
  
  dd_yr$incidence[dd_yr$incidence == 0] <- NA
  
  
  p4 <-ggplot(dd_yr, aes(x = year)) +
    # 置信区间条带
    geom_ribbon(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, fill = "Predicted Cases"), alpha = 0.1) +
    
    # 历史数据的实线和圆圈
    geom_line(aes(y = incidence, color = "Actual Cases"), size = 1.5) +
    geom_point(aes(y = incidence, color = "Actual Cases"), size = 3, shape = 16) +
    geom_line(aes(y = fitted.incidence, color = "Predicted Cases"), linetype = "dashed", size = 1.2) +
    geom_point(aes(y = fitted.incidence, color = "Predicted Cases"), size = 2.8, shape = 16) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
    
    scale_color_manual(values = c(
      "Actual Cases" = "#9654e5", 
      "Predicted Cases" = "#e7298a")) +
    scale_fill_manual(values = c(
      "Predicted Cases" = "#e7298a")) +
    
    # 图例标签
    labs(title = "Using data from 2011-2020 to predict for 2021-2025",
         x = "Year",
         y = expression(paste("Incidence of SFTS cases (", 1/10^7, ")")),
         color = "Data Type",
         fill = "Data Type") +
    # 主题
    theme_classic() +
    theme(
      legend.position = c(0.2,0.6),
      legend.title = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_continuous(breaks = seq(min(dd_yr$year), max(dd_yr$year), by = 1))+
    guides(color = guide_legend(ncol = 1, bycol = TRUE))  # 图例水平排列
  
  p4
  
  # return
  plots = vector("list", 4)
  plots[[1]] = p1
  plots[[2]] = p2
  plots[[3]] = p3
  plots[[4]] = p4
  
  return(plots)
}


library(gridExtra)

plots1 <- plot_trainingsets_func(filepaths1)
bb1 <- grid.arrange(plots1[[1]], plots1[[2]], plots1[[3]], plots1[[4]], ncol = 2, nrow = 2)

plots2 <- plot_trainingsets_func(filepaths2)
bb2 <- grid.arrange(plots2[[1]], plots2[[2]], plots2[[3]], plots2[[4]], ncol = 2, nrow = 2)

plots3 <- plot_trainingsets_func(filepaths3)
bb3 <- grid.arrange(plots3[[1]], plots3[[2]], plots3[[3]], plots3[[4]], ncol = 2, nrow = 2)



ggsave(bb1, file="./output/figures/FigureS6_b1.tif", device="tiff", units="in", width=12, height=9, dpi=300)
ggsave(bb2, file="./output/figures/FigureS6_b2.tif", device="tiff", units="in", width=12, height=9, dpi=300)
ggsave(bb3, file="./output/figures/FigureS6_b3.tif", device="tiff", units="in", width=12, height=9, dpi=300)



