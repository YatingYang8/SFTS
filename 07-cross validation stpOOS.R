# ====================================================================================================
# ============================ CROSS VALIDATION MODEL SCRIPT =========================================
# ====================================================================================================


## ============= set up workflow and data ===================

# working directory
PATH = dirname(rstudioapi::getSourceEditorContext()$path)  #加一个dirname可以返回再上一级目录
setwd(PATH)

library(dplyr); library(raster); library(rgdal); library(sf)
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep)
source("00_plot_themes.R")
source("00_inla_setup_functions_r4.R")



# INLA modelling functions, priors and pardiso license
#inla.setOption(pardiso.license="pardiso.lic.txt")

# build dataframe calls 21_build_model_df.R, specifying 4 variables 
# projname: name of save directory for outputs
# region: either NA, north, south or central; whether to subset to specific region
# n_clim_bins: n bins for grouping climatic predictors for nonlinear effects
# plot_graph: visualise neighbourhood matrix?
projname = "stempOOS_nb2"
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")
if(!dir.exists(save_dir)){ 
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste(save_dir, "model_output/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "errors/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "models/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "fitmetrics/", sep=""), recursive = TRUE, showWarnings = FALSE) 
}

region = "all"
region2 = NA
n_clim_bins = 40
plot_graph = FALSE
province_case_threshold = NA

source("./00_build_model_df_clim.R")

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





ddf$areaid <- ddf$countyid



# ================= initialises 5fold exclusion =================

# n.b. the below only runs once, the first time this script is run for a specified save_dir
# creates candidate models list, tracker and completed files to track and store model runs

# parameters for setup

n_reps = 10 
k_folds = 5 
#type = "spOOS" 
type = "stempOOS"
#type = "tempOOS"

# create initialise files have not all created already
ll = list.files(save_dir)
if(!"models_list_all.csv" %in% ll){
  set.seed(123)
  # unique IDs vec
  kfold_ids = c()
  
  # ----- create n_reps k-fold sets ------
  
  for(nn in 1:n_reps){
    
    unique_id_n = sample(1:10^6, 1)
    mod_names_n = paste(unique_id_n, type, sep="_")
    kfold_ids = c(kfold_ids, unique_id_n)
    
    # partition dataset
    if(type == "stempOOS"){
      folds = ddf %>% dplyr::select(areaid, year) %>% distinct()
      folds$kfold = kfold_func(folds, k = k_folds)
      write.csv(folds, paste(save_dir, unique_id_n, "_folds.csv", sep=""), row.names=FALSE)
      #ddf = left_join(ddf, folds)
    }
    
    if(type == "spOOS"){
      folds = ddf %>% dplyr::select(areaid) %>% distinct() #（distinct）返回数据框中唯一的行，它将返回一个只包含不同areaid值的数据框
      folds$kfold = kfold_func(folds, k = k_folds)
      write.csv(folds, paste(save_dir, unique_id_n, "_folds.csv", sep=""), row.names=FALSE)
      #ddf = left_join(ddf, folds)
    }
    
    # by quarters
    if(type == "tempOOS"){
      
      # 3-month groupings
      my = ddf %>%
        dplyr::select(date, month) %>%
        distinct() %>%
        dplyr::arrange(date) %>%
        dplyr::left_join(
          data.frame(month = 1:12, subyear=rep(1:4, each=3))
        ) %>%
        dplyr::select(-month)
      ddf = left_join(ddf, my)
      
      # 5-fold quarterly groupings per district
      # kfd = ddf %>%
      #   dplyr::select(areaid, year, subyear) %>%
      #   distinct() %>%
      #   dplyr::group_by(areaid) %>%
      #   dplyr::mutate(kfold = kfold_func(subyear, k=5))
      
      # 5-fold quarterly groupings across the board
      kfd = ddf %>%
        dplyr::select(year, subyear) %>%
        distinct() %>%
        dplyr::mutate(kfold = kfold_func(subyear, k=k_folds))
      
      ddf = left_join(ddf, kfd)
      
      write.csv(
        ddf %>% dplyr::select(areaid, date, kfold),
        paste(save_dir, unique_id_n, "_folds.csv", sep=""),
        row.names=FALSE
      )
      
      ddf = ddf %>% dplyr::select(-kfold)
    }
    
  } # end partitioning loop
  
  
  # ----- create models dataframe -----
  
  
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
  
  
  # model list
  fx = vector("list", length=length(effect_names)+4)
  
  # full models
  m1 = paste(effect_names, collapse=" + "); 
  name1 = "full"
  
  # add full models
  fx[1] = list(m1)
  
  # individual holdouts
  for(i in 1:length(effect_names)){
    fx[[ i + 1 ]] = paste(effect_names[ -i ], collapse=" + ")
  }
  
  # models of exclude each type of variate
  climate = paste(c("urban_log",
                    "transportation_log",
                    "Cropland_log",
                    "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                    "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                    "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                    "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"),collapse = "+")
  fx[13] = list(climate)
  
  social = paste(c("Precipitation_02m_log",
                   "Cropland_log",
                   "f(Tmean_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE) ",
                   "f(Rh_3m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                   "f(Sun_01m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                   "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                   "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                   "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"
  ),collapse = "+")
  fx[14] = list(social)
  
  geographical = paste(c("Precipitation_02m_log",
                         "urban_log",
                         "transportation_log",
                         "f(Tmean_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE) ",
                         "f(Rh_3m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                         "f(Sun_01m_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
                         "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"
  ),collapse = "+")
  fx[15] = list(geographical)
  
  # unlist and add into df
  fx = unlist(fx)
  
  # create data frame including formulae
  fx = data.frame(modid = 1:length(fx),
                  fx = fx,
                  candidate = c(name1,"Precipitation","urban", "transportation", "Cropland",  
                                "Tmean",  "Rh", "Sun", "Migration","elevation","Grassland","Shrub",
                                "Climate factors","Social factors","Geographical factors"),
                  formula = paste(form_base, fx, sep=" + "))
  
  bs = data.frame(modid = "baseline", fx = "baseline", candidate="baseline", formula=form_base)
  fx = rbind(fx, bs)
  
  # repeat for each n_reps ID to create full set of models to fit, cross referenced to unique holdout set
  fx_mods = data.frame()
  for(ii in kfold_ids){
    fx_mods = rbind(fx_mods, 
                    fx %>% dplyr::mutate(unique_id = ii,
                                         model_filename = paste(ii, type, "nb_model", modid, sep="_")))
  }
  fx_mods$model_identifier = 1:nrow(fx_mods)
  
  # ------- save initialisation objects ---------
  
  # list of models to fit
  write.csv(fx_mods, paste(save_dir, "models_list_all.csv", sep=""), row.names=FALSE)
  
  # tracker (which models are currently running)
  tracker = data.frame(unique_id = "dummy", model_filename = "dummy", model_identifier = "dummy")
  write.csv(tracker, paste(save_dir, "models_tracker.csv", sep=""), row.names=FALSE)
  
  # completed
  completed = data.frame(unique_id = "dummy", model_filename = "dummy", model_identifier = "dummy", candidate = "dummy",
                         n_models_fitted = "dummy", mae_oos = "dummy", rmse_oos = "dummy")
  write.csv(completed, paste(save_dir, "models_completed.csv", sep=""), row.names=FALSE)
  
} # end init block




# ===================== chooses and fits model under k-fold CV ============================
sim <- function(kkk){
  
  # check currently running and completed models
  tracker = read.csv(paste(save_dir, "models_tracker.csv", sep=""))
  completed = read.csv(paste(save_dir, "models_completed.csv", sep=""))
  
  # list of models to fit
  fx = read.csv(paste(save_dir, "models_list_all.csv", sep="")) %>%
    dplyr::filter(!model_identifier %in% c(tracker$model_identifier, completed$model_identifier)) 
  
  # select model and fit if > 0 models left in list
  if(nrow(fx) > 0){
    
    # select 1 model
    #fx = fx %>% sample_n(1)
    fx = fx[1,]
    
    # append selected model to tracker (removed after model completed)
    to_append = fx[ , c("unique_id", "model_filename", "model_identifier")]
    write.table(to_append, file=paste(save_dir, "models_tracker.csv", sep=""), 
                append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
    
    # add correct folds information to dd
    folds = read.csv(paste(save_dir, fx$unique_id, "_folds.csv", sep=""))
    ddf = left_join(ddf, folds)
    
    # fit each of the models in dataframe fx (by default 1)
    for(i in 1:nrow(fx)){
      
      # formula
      fx_i = fx[ i, ]
      form_i = formula(as.vector(fx_i$formula))
      
      # fit INLA model nested in tryCatch
      e = simpleError("error fitting")
      
      # the folds to fit
      folds_seq = unique(ddf$kfold)[ order(unique(ddf$kfold)) ]
      
      # oos_results data frame
      oos_results = data.frame()
      
      # for each fold
      for(k in folds_seq){
        
        # data and set group k to NA
        dd_i <- ddf %>% dplyr::mutate(y = total_cases) %>%
          dplyr::mutate(y = replace(y, kfold == k, NA))
        
        print("")
        print("==========================================")
        print(Sys.time())
        print("==========================================")
        print("")
        
        # fit model
        mod_i = tryCatch(
          fitINLAModel(form_i, dd_i, family="nbinomial", verbose=TRUE),
          error = function(e) return(e)
        )
        #poisson
        #nbinomial
        # try again if failed
        if(class(mod_i)[1] == "simpleError"){
          mod_i = tryCatch(
            fitINLAModel(form_i, dd_i, family="nbinomial", verbose=TRUE),
            error = function(e) return(e)
          )
        }
        
        # if failed again write timeout to result
        if(class(mod_i)[1] == "simpleError"){
          
          ex = fx_i; ex$result = "error in fitting"
          ex_file_name = paste(k, "_", type, "_nb_err_", fx_i$model_identifier, ".csv", sep="")
          write.csv(ex, paste(save_dir, "errors/", ex_file_name, sep=""), row.names=FALSE)
          
          # otherwise calculate and save fit metrics and model
        } else{
          
          fm = fitMetricsINLA(mod_i, data=dd_i, modname=fx_i$fx, inla.mode="experimental")
          res_i = cbind(fx_i, fm)
          fm_file_name = paste(k, "_", type, "_nb_fitmetrics_", fx_i$model_identifier, ".csv", sep="")
          write.csv(res_i, paste(save_dir, "fitmetrics/", fm_file_name, sep=""), row.names=FALSE)
          
          # save model
          #save(mod_i, file=paste(save_dir, "models/", paste(k, fx_i$model_filename, sep="_"), sep=""))
          
          # extract observed and fitted and save
          dd_o = ddf %>%
            dplyr::select(areaid, date, logpop, total_cases, kfold) %>%
            dplyr::mutate(oos = ifelse(kfold == k, TRUE, FALSE),
                          model = fx_i$candidate,
                          holdout_id = fx_i$unique_id,
                          model_identifier = fx_i$model_identifier,
                          holdout_type = type,
                          mean = mod_i$summary.linear.predictor$mean,
                          lower = mod_i$summary.linear.predictor$`0.025quant`,
                          upper = mod_i$summary.linear.predictor$`0.975quant`) %>%
            dplyr::filter(oos == TRUE)
          write.csv(dd_o, paste(save_dir, "models/", paste(k, "output", fx_i$model_identifier, fx_i$model_filename, ".csv", sep="_"), sep=""), row.names=FALSE)
          
          # add to growing OOS results dataframe
          oos_results = rbind(oos_results, dd_o)
          
        }
        
      } # end of kfold loop
      
      
      # ============ final operations ==============
      
      print("Saving results")
      
      # create dataframe to add to "completed" csv
      completed_i = data.frame(unique_id = fx_i$unique_id, 
                               model_filename = fx_i$model_filename, 
                               model_identifier = fx_i$model_identifier, 
                               candidate = fx_i$candidate,
                               n_models_fitted = n_distinct(oos_results$kfold), 
                               mae_oos = NA, 
                               rmse_oos = NA)
      
      # calculate predicted and residual error (run through link function)
      oos_results$predicted = exp(oos_results$logpop + oos_results$mean)
      oos_results$resid = oos_results$predicted - oos_results$total_cases
      
      # calculate summary stats
      completed_i$mae_oos = mean(abs(oos_results$resid), na.rm=TRUE)
      completed_i$rmse_oos = sqrt(mean(oos_results$resid^2, na.rm=TRUE))
      
      # remove from 
      #if(completed_i$n_models_fitted == k_folds){
      
      # append to completed
      write.table(completed_i, file=paste(save_dir, "models_completed.csv", sep=""), 
                  append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
      
      #}
      
      # remove model from tracker
      read.csv(paste(save_dir, "models_tracker.csv", sep="")) %>%
        dplyr::filter(model_identifier != fx_i$model_identifier) %>%
        write.csv(paste(save_dir, "models_tracker.csv", sep=""), row.names=FALSE, col.names=FALSE)
      
    } # end of model fitting loop
    
    
  } # end of if statement
  
  
}


# ===================== ENDS =============================
for (kkk in 1:160){
  set.seed(kkk)
  sim(kkk)
}


