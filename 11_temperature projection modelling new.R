
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
projname  =  "Temperature Projection"
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

predic_func <- function(trainyear1,trainyear2,valyear1,valyear2){
  
  load(file = "output/data_process/proj_df_final4.RData")
  
  names(proj_df_final4)
  
  #choose the climate change scenarios
  proj_df_final4 <- proj_df_final4 %>%
    mutate(Tmean_g = ifelse(year <= 2023, Tmean_g, Tmean_ssp245))
  
  # 数据处理
  dd = proj_df_final4 %>%
    dplyr::mutate(
      Tmean_g = inla.group(Tmean_g, n=30),
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
  
  #dd$polyid <- dd$polyid_city 
  
  
  # epochs = data.frame(year = 2011:2028,
  #                     epoch=c(rep("1", length(2011:2013)), rep("2", length(2014:2016)), 
  #                             rep("3", length(2017:2019)), rep("4", length(2020:2023)),
  #                             rep("5", length(2024:2028))))
  # 
  # 
  # dd$year <- as.integer(dd$year)
  # 
  # dd <- left_join(dd, epochs, by = "year") %>%
  #   mutate( 
  #     epoch = as.integer(epoch),
  #     yearx2 = ifelse(year <= trainyear2, yearx, yearx[year == trainyear2])
  #   )
  
  
  
  # 定义训练和验证数据集
  train_data = dd %>% filter(year %in% c(trainyear1:trainyear2))
  val_data = dd %>% filter(year %in% c(valyear1:valyear2))
  
  # 创建用于建模的数据框，响应变量和家庭
  train_data <- train_data %>% mutate(y = total_cases)
  val_data <- val_data %>% mutate(y = NA)
  
  # 
  form_base = paste(
    c("y ~ 1",
      "offset(logpop)",
      "logyear"
    ),
    collapse = " + "
  )
  
  # form_base = paste(
  #   c("y ~ 1",
  #     "offset(logpop)",
  #     "f(yearx2, model='rw1', hyper=hyper3.rw,  scale.model=TRUE, constr=TRUE, replicate=cityx)",
  #     "f(polyid, model='bym2', graph=nbmatrix_name,  scale.model=TRUE, constr=TRUE, adjust.for.con.comp=TRUE, hyper=hyper.bym2)"),
  #   collapse = " + "
  # )
  
  # 完整模型的效果名称
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
  
  
  save(mod_i, file=paste(save_dir, "models/", "Temp_proj_model_train",trainyear2,"_",valyear2,".R", sep=""))
  
  # 获取预测值和置信区间
  ####### calculate the 95%CI for fitted cases ###################################
  
  load(file=paste(save_dir, "models/", "Temp_proj_model_train",trainyear2,"_",valyear2,".R", sep=""))
  
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
  
  save(Temp_proj_fitted_result, file=paste(save_dir, "Temp_proj_model_result_train",trainyear2,"_val",valyear2,".RData", sep=""))
  
}




predic_func(2011,2023,2024,2028)
predic_func(2011,2022,2023,2027)
predic_func(2011,2021,2022,2026)
predic_func(2011,2020,2021,2025)
predic_func(2011,2018,2019,2022)


predic_func(2011,2023,2024,2028)
predic_func(2011,2022,2023,2028)
predic_func(2011,2021,2022,2028)
predic_func(2011,2020,2021,2028)
