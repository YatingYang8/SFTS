

# ====================================================================================================
# ===================== VARIABLES: BASELINE + UNIVARIATE COVARIATES ==================
# ====================================================================================================

## Modelling script 1:
## Fits baseline model and baseline + single covariate ('univariate') models
PATH = dirname(rstudioapi::getSourceEditorContext()$path)  
setwd(PATH)

## ============= set up workflow and data ===================

# project root, dependencies, plot themes

library(dplyr); library(raster); library(rgdal); library(sf)
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep)
source("./00_plot_themes.R")

# INLA modelling functions, priors and pardiso license
source("./00_inla_setup_functions_r4.R")

# build dataframe calls 21_build_model_df.R, specifying 4 variables 
# projname: name of save directory for outputs
# region: either NA, north, south or central; whether to subset to specific region
# n_clim_bins: n bins for grouping climatic predictors for nonlinear effects
# plot_graph: visualise neighbourhood matrix?
projname = "variableselect"

# create folder structure for saving outputs
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")
if(!dir.exists(save_dir)){ 
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste(save_dir, "model_output/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "errors/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "models/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "fitmetrics/", sep=""), recursive = TRUE, showWarnings = FALSE) 
}


region = "all"
n_clim_bins = 40
plot_graph = FALSE

source("00_build_model_df_clim.R")

dd = read.csv("./output/data_process/ModelData_sfts_All_withLags.csv",fileEncoding = "GB18030")


# ================= customise dataframe for this modelling task ====================

# create dataframe for modelling, response and family
ddf = dd

# defined the polyid, polyid_city for manually added neibour matrix
### if you use nbmatrix_name1 or nbmatrix_name2, choose polyid_city (city level) 
### if you use nbmatrix_name, choose polyid (county level) 
ddf$polyid <- ddf$polyid_city

#names(ddf)

# select only variables required for models and scale/log transform
ddf <- ddf %>%
  dplyr::select(
    city, 
    cityid,
    countyid, 
    county,
    year,
    month,
    total_cases, 
    logpop,
    polyid,
    yearx,
    cityx, 
    areaidx,
    urban,
    transportation,
    Cropland,
    Forest,
    Grassland,
    elevation,
    Shrub,
    in_migration,
    out_migration,
    Tmean,
    Tmin,
    Tmax,
    Precipitation,
    Wind,
    Rh,
    Sun
  ) %>%
  dplyr::mutate(
    urban_s = scale(urban),
    urban_log = log(urban + 1),
    transportation_s = scale(transportation),
    transportation_log = log(transportation + 1),
    Cropland_s = scale(Cropland),
    Cropland_log = log(Cropland + 1),
    Forest_s = scale(Forest),
    Forest_log = log(Forest + 1),
    Grassland_s = scale(Grassland),
    Grassland_log = log(Grassland + 1),
    Shrub_s = scale(Shrub),
    Shrub_log = log(Shrub+1),
    elevation_s = scale(elevation),
    elevation_log = log(elevation + 1),
    in_migration_s = scale(in_migration),
    in_migration_log = log(in_migration + 1),
    out_migration_s = scale(out_migration),
    out_migration_log = log(out_migration + 1),
    migration_mean = mean(in_migration,out_migration, trim = 0),
    migration_sum = sum(in_migration,out_migration),
    Tmean_s = scale(Tmean),
    Tmean_log = log(Tmean),
    Precipitation_s = scale(Precipitation),
    Precipitation_log = log(Precipitation),
    Rh_s = scale(Rh),
    Rh_log = log(Rh),
    Sun_s = scale(Sun),
    Sun_log = log(Sun)
    
  ) 

# # static mean variables for gravity/urban/pop/mobility


# grouped variables
ddf = ddf %>%
  dplyr::mutate(urban_g = inla.group(urban, n=n_clim_bins),
                transportation_g = inla.group(transportation, n=n_clim_bins),
                Cropland_g = inla.group(Cropland, n=n_clim_bins),
                Forest_g = inla.group(Forest, n=n_clim_bins),
                Grassland_g = inla.group(Grassland, n=n_clim_bins),
                Shrub_g = inla.group(Shrub, n=n_clim_bins),
                elevation_g = inla.group(elevation, n=n_clim_bins),
                in_migration_g = inla.group(in_migration, n=n_clim_bins),
                out_migration_g = inla.group(out_migration, n=n_clim_bins),
                Tmean_g = inla.group(Tmean, n=n_clim_bins),
                Precipitation_g = inla.group(Precipitation, n=n_clim_bins),
                Sun_g = inla.group(Sun, n=n_clim_bins),
                Rh_g = inla.group(Rh, n=n_clim_bins)
  )

# extra region flags
#ddf$regiony1 = ddf$regiony

# other flags
ddf$areaidx = as.integer(as.factor(ddf$countyid))
ddf$polyidx = ddf$polyid



# # =================== create dataframe of models to fit and compare =====================

# separate bym effects for each year
form_base = paste(
  c("y ~ 1",
    "offset(logpop)"),
  collapse = " + "
)
#names(ddf)
# univariate fx
fx = c("urban", "urban_s", "urban_log",
       "transportation", "transportation_s", "transportation_log",
       "Cropland", "Cropland_s", "Cropland_log",
       "Forest", "Forest_s", "Forest_log",
       "Grassland",  "Grassland_s", "Grassland_log",
       "Shrub","Shrub_s","Shrub_log",
       "elevation", "elevation_s", "elevation_log",
       "in_migration",  "in_migration_s", "in_migration_log",
       "out_migration", "out_migration_s", "out_migration_log",
       "migration_mean", 
       "migration_sum", 
       "Tmean", "Tmean_s", "Tmean_log",
       "Precipitation", "Precipitation_s", "Precipitation_log",
       "Rh", "Rh_s", "Rh_log",
       "Sun", "Sun_s", "Sun_log",
       "f(urban_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(transportation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Cropland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Forest_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(out_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Tmean_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Precipitation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Rh_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
       "f(Sun_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"
       
)

# create data frame
fx = data.frame(modid = 1:length(fx),
                fx = fx,
                effect_type = c(rep("linear", 41), rep("rw2", 13)),
                formula = paste(form_base, fx, sep=" + "))
bs = data.frame(modid = "baseline", fx = "baseline", effect_type = "baseline", formula=form_base)
fx = rbind(fx, bs)
#head(fx)
# model name
fx$model_filename = paste("univariate_nb_model_", fx$modid, ".R", sep="")



# ================== fit models in an interative loop ======================

# run model selection loop
for(i in 1:nrow(fx)){
  
  # formula
  fx_i = fx[ i, ]
  form_i = formula(as.vector(fx_i$formula))
  
  # fit INLA model nested in tryCatch
  e = simpleError("error fitting")
  
  # save storage by cutting all unnecessary variables and only keep specified variable
  if(fx_i$effect_type %in% c("rw2" , "baseline")){ 
    
    dd_i <- ddf %>%
      dplyr::select(
        city, 
        cityid,
        countyid, 
        county,
        year,
        month,
        total_cases, 
        logpop,
        polyid,
        polyidx,
        yearx,
        cityx, 
        areaidx, 
        urban_g,
        transportation_g,
        Cropland_g,
        Forest_g,
        Grassland_g,
        Shrub_g,
        elevation_g,
        in_migration_g,
        out_migration_g,
        Tmean_g,
        Precipitation_g,
        Rh_g,
        Sun_g) %>%
      dplyr::mutate(y = total_cases)
    
  } else{
    
    dd_i <- ddf %>%
      dplyr::select(
        city, 
        cityid,
        countyid, 
        county,
        year,
        month,
        total_cases, 
        logpop,
        polyid,
        polyidx,
        yearx,
        cityx, 
        areaidx,
        urban,
        urban_s,
        urban_log,
        transportation,
        transportation_s,
        transportation_log,
        Cropland,
        Cropland_s,
        Cropland_log,
        Forest,
        Forest_s,
        Forest_log,
        Grassland,
        Grassland_s,
        Grassland_log,
        Shrub,
        Shrub_s,
        Shrub_log,
        elevation,
        elevation_s,
        elevation_log,
        in_migration,
        in_migration_s,
        in_migration_log,
        out_migration,
        out_migration_s,
        out_migration_log,
        migration_mean,
        migration_sum,
        Tmean,
        Tmean_s,
        Tmean_log,
        Precipitation,
        Precipitation_s,
        Precipitation_log,
        Rh,
        Rh_s,
        Rh_log,
        Sun,
        Sun_s,
        Sun_log) %>%
      dplyr::mutate(y = total_cases)
    dd_i <- cbind(dd_i, ddf[ , which(names(ddf) %in% unlist(strsplit(as.vector(fx_i$fx), split="[ + ]"))), drop=FALSE])
    
  }
  
  # fit model
  mod_i = tryCatch(
    fitINLAModel(form_i, dd_i, family="nbinomial", verbose=TRUE),
    error = function(e) return(e)
  )
  
  # write timeout to result
  if(class(mod_i)[1] == "simpleError"){
    
    ex = fx_i; ex$result = "error in fitting"
    ex_file_name = paste("sociouni_nb_err_", fx_i$modid, ".csv", sep="")
    write.csv(ex, paste(save_dir, "errors/", ex_file_name, sep=""), row.names=FALSE)
    
    # otherwise calculate and save fit metrics and model
  } else{
    
    fm = fitMetricsINLA(mod_i, data=dd_i, modname=fx_i$fx, inla.mode="experimental")
    res_i = cbind(fx_i, fm)
    fm_file_name = paste("sociouni_nb_fitmetrics_", fx_i$modid, ".csv", sep="")
    write.csv(res_i, paste(save_dir, "fitmetrics/", fm_file_name, sep=""), row.names=FALSE)
    
    # save model
    save(mod_i, file=paste(save_dir, "models/", fx_i$model_filename, sep=""))
  }
  
} # end of model fitting loop










##################################################################################

# ============ view goodness of fit (DIC) metrics for all univariate models ===============

# functions for reading in
countvars = function(x){ sapply(strsplit(as.vector(x), "[+]"), length) }
readfile = function(x){
  foo = read.csv(x)
  if(!"covar" %in% names(foo)){ foo$covar = "" }
  if(!"model_sub" %in% names(foo)){ foo$model_sub = "" }
  if(!"model_filename" %in% names(foo)){ foo$model_filename = "" }
  foo
}


# socio-environmental covariate models
ff = list.files("./output/model_outputs/variableselect/fitmetrics/", full.names=TRUE, pattern=".csv")

fx1 = do.call(rbind.data.frame, lapply(ff, readfile)) %>%
  dplyr::mutate(num_predictors = countvars(modname),
                covar = modname) %>%
  dplyr::arrange(waic) %>%
  dplyr::select(-formula)

fx1$deltawaic = fx1$waic - fx1$waic[ fx1$modname == "baseline"]
fx1$deltadic = fx1$dic - fx1$dic[ fx1$modname == "baseline"]
fx1$deltaLS = fx1$logscore - fx1$logscore[ fx1$modname == "baseline"]

fx1$covar_grp = NA
fx1$covar_grp[ grep("migration|transportation", fx1$modname) ] = "Mobility"
fx1$covar_grp[ grep("urban", fx1$modname) ] = "Urbanisation"
fx1$covar_grp[ grep("Cropland|Forest|Grassland|Shrub|elevation", fx1$modname) ] = "Terrain"
fx1$covar_grp[ grep("Tmean|Tmin|Tmax|Precip|Wind|Rh|Sun", fx1$modname) ] = "Temperature"

write.csv(fx1,file = "./output/model_outputs/Bayesian nb model/model_output/univariate_analysis_excluZS.csv")


# climate models
ff = list.files("./output/model_outputs/variableselect/fitmetrics/", full.names=TRUE, pattern=".csv")

fx2 = do.call(rbind.data.frame, lapply(ff, readfile)) %>%
  dplyr::mutate(num_predictors = countvars(modname), effect_type = "rw2") %>%
  dplyr::arrange(dic) %>%
  dplyr::select(-formula)

fx2$deltawaic = fx2$waic - fx2$waic[ fx2$modname == "baseline"]
fx2$deltadic = fx2$dic - fx2$dic[ fx2$modname == "baseline"]
fx2$deltaLS = fx2$logscore - fx2$logscore[ fx2$modname == "baseline"]

fx2$modname[ fx2$modname == "baseline"] = "climate_baseline"
fx2$covar_grp = "Temperature"

fx2 <- fx2 %>%
  dplyr::select(-group)

# keep only the best lag for each
fx3 = fx2 %>% 
  dplyr::mutate(xx = unlist(lapply(strsplit(covar, "_"), "[", 1))) %>%
  dplyr::group_by(xx) %>%
  dplyr::filter(deltawaic == min(deltawaic)) %>%
  #dplyr::filter(deltadic == min(deltadic)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-xx)


# combine
fx = rbind(fx1, fx2)
#write.csv(fx,file = "./output/model_outputs/Bayesian poisson model/model_output/univariate_analysis_excluZS.csv")

fx = rbind(fx1, fx3)
#write.csv(fx,file = "./output/model_outputs/Bayesian poisson model/model_output/univariate_analysis_excluZSfinal.csv")


# Geography covariate models
ff = list.files("./output/model_outputs/geography_excluZS/fitmetrics/", full.names=TRUE, pattern=".csv")

fx4 = do.call(rbind.data.frame, lapply(ff, readfile)) %>%
  dplyr::mutate(num_predictors = countvars(modname),
                covar = modname) %>%
  dplyr::arrange(waic) %>%
  dplyr::select(-formula)

fx4$deltawaic = fx4$waic - fx4$waic[ fx4$modname == "baseline"]
fx4$deltadic = fx4$dic - fx4$dic[ fx4$modname == "baseline"]
fx4$deltaLS = fx4$logscore - fx4$logscore[ fx4$modname == "baseline"]

fx4$covar_grp = NA
fx4$covar_grp[ grep("Forest", fx4$modname) ] = "Forest"
fx4$covar_grp[ grep("Cropland", fx4$modname) ] = "Cropland"
fx4$covar_grp[ grep("Grassland", fx4$modname) ] = "Grassland"
fx4$covar_grp[ grep("Water", fx4$modname) ] = "Water"
fx4$covar_grp[ grep("Barren", fx4$modname) ] = "Barren"
fx4$covar_grp[ grep("Impervious", fx4$modname) ] = "Impervious"
fx4$covar_grp[ grep("Shrub", fx4$modname) ] = "Shrub"


#write.csv(fx4,file = "./output/model_outputs/Bayesian poisson model/model_output/univariate_analysis_geography.csv")






##################  Plots change in DIC  ################################################
fx <- read.csv("./output/model_outputs/Bayesian nb model/model_output/univariate_analysis_excluZSplots.csv")
names(fx)

fx$group_num <- as.factor(fx$group_num)

fx$covar

# plot change in dic for different vars in univariate
dat_subset = fx %>%
  #dplyr::filter(type == "uni") %>%
  dplyr::filter(!modname %in% c("baseline")) %>%
  dplyr::arrange(group_num, desc(deltadic)) %>%
  dplyr::mutate(covar_name = factor(covar, levels=covar, ordered=TRUE)) 


dic_plot = dat_subset %>%
  #dplyr::filter(covar_name != "Multivariate") %>%
  dplyr::arrange(desc(group_num), desc(deltadic)) %>%
  #dplyr::filter(!grepl("total distance", covar)) %>%
  dplyr::mutate(covar_name = factor(covar_name, levels=covar_name, ordered=TRUE)) %>%
  ggplot() +
  geom_point(aes(covar_name, deltadic, fill=group_num), size=6, pch=21) +
  geom_hline(yintercept=0, lty=2, size=1) +
  coord_flip() +
  theme_minimal() +
  ylab(expression(paste(Delta, "DIC"))) + xlab("") +
  theme(axis.text.y = element_text(size=13),
        panel.border = element_rect(color="grey20", fill=NA),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=14),
        plot.title = element_text(size=15, hjust=0.5),
        legend.title=element_blank(),
        legend.text = element_text(size=13),
        legend.position="none") 

print(dic_plot)


ggsave(dic_plot, file="./output/figures/FigureS3_Variableselection2.pdf", device="pdf", units="in", width=12, height=9)

