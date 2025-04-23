
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


nbmatrix_name = nbmatrix_name1
nbmatrix_name

ddf$polyid <- ddf$polyid_city

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


form_base = paste(
  c("y ~ 1",
    "offset(logpop)",
    "logyear"),
  collapse = " + "
)


form_base

# form_base = paste(
#   c("y ~ 1",
#     "offset(logpop)",
#     "f(yearx, model='rw1', hyper=hyper3.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
#     "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
#   collapse = " + "
# )

# form_base = paste(
#   c("y ~ 1",
#     "offset(logpop)",
#     "logyear",
#     "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
#   collapse = " + "
# )



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


# create data frame
fx = data.frame(modid = 1:length(fx),
                fx = fx,
                candidate = c(name1),
                formula = paste(form_base, fx, sep=" + "))
bs = data.frame(modid = "baseline", fx = "baseline", candidate="baseline", formula=form_base)
fx = rbind(fx, bs)

# model name
fx$model_filename = paste("fullmod_nb_model_", fx$modid, ".R", sep="")





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
?inla
#?control.inla
#inla.doc("rw1")
#inla.doc("bym")


