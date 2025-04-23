options(timeout=99999)
library(INLA)
####################### Lag and cumulative effects of Climate variables ##################################
dd = read.csv("./output/data_process/ModelData_sfts_excluZS_withLags.csv",fileEncoding = "GB18030")
ddf <- dd

n_clim_bins = 40

# select only variables required for models and scale/log transform
ddf <- ddf %>%
  dplyr::mutate(
    Sun_01m_g = inla.group(Sun_01m, n=n_clim_bins),
    Precipitation_log = log(Precipitation+1),
    in_migration_g = inla.group(in_migration_norm, n=n_clim_bins),
    urban_log = log(urban+1),
    transportation_log = log(transportation+1),
    elevation_g = inla.group(elevation, n=n_clim_bins),
    Shrub_g = inla.group(Shrub, n=n_clim_bins),
    Grassland_g = inla.group(Grassland, n=n_clim_bins), 
    Cropland_log = log(Cropland+1)
  ) 




# defined the polyid, polyid_city for manually added neibour matrix
### if you use nbmatrix_name1 or nbmatrix_name2, choose polyid_city (city level) 
### if you use nbmatrix_name, choose polyid (county level) 
ddf$polyid <- ddf$polyid_city

# subset to required variables
ddf1 = ddf %>%
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
    cityx, 
    areaidx,
    Precipitation_log,
    in_migration_g,
    urban_log,
    transportation_log,
    elevation_g,
    Cropland_log,
    Shrub_g,
    Grassland_g
  )





#ddf2 <- ddf %>%
#  dplyr::select(matches("Tmean|Tmin|Tmax|Precip|Wind|Rh|Sun")) 
ddf2 <- ddf %>%
  dplyr::select(matches("Tmean|Precip|Rh|Sun")) 


ddf <- cbind(ddf1, ddf2)

ddf$date <- as.Date(ddf$date)



library(dplyr)
library(zoo)  # 包含 rollmean 函数

ddf <- ddf[, !duplicated(colnames(ddf))]
ddf <- ddf %>%
  select(-Sun_01m, -Rh_06m)

# Cumulative effect
ddf <- ddf %>%
  arrange(countyid, date) %>%  # 按城市和日期排序
  group_by(countyid) %>%
  mutate(Tmean_01m = rollmean(Tmean, k = 2, fill = NA, align = "right"),
         Tmean_02m = rollmean(Tmean, k = 3, fill = NA, align = "right"),
         Tmean_03m = rollmean(Tmean, k = 4, fill = NA, align = "right"),
         Tmean_04m = rollmean(Tmean, k = 5, fill = NA, align = "right"),
         Tmean_05m = rollmean(Tmean, k = 6, fill = NA, align = "right"),
         Tmean_06m = rollmean(Tmean, k = 7, fill = NA, align = "right"),
         Precipitation_01m = rollmean(Precipitation, k = 2, fill = NA, align = "right"),
         Precipitation_02m = rollmean(Precipitation, k = 3, fill = NA, align = "right"),
         Precipitation_03m = rollmean(Precipitation, k = 4, fill = NA, align = "right"),
         Precipitation_04m = rollmean(Precipitation, k = 5, fill = NA, align = "right"),
         Precipitation_05m = rollmean(Precipitation, k = 6, fill = NA, align = "right"),
         Precipitation_06m = rollmean(Precipitation, k = 7, fill = NA, align = "right"),
         Rh_01m = rollmean(Rh, k = 2, fill = NA, align = "right"),
         Rh_02m = rollmean(Rh, k = 3, fill = NA, align = "right"),
         Rh_03m = rollmean(Rh, k = 4, fill = NA, align = "right"),
         Rh_04m = rollmean(Rh, k = 5, fill = NA, align = "right"),
         Rh_05m = rollmean(Rh, k = 6, fill = NA, align = "right"),
         Rh_06m = rollmean(Rh, k = 7, fill = NA, align = "right"),
         Sun_01m = rollmean(Sun, k = 2, fill = NA, align = "right"),
         Sun_02m = rollmean(Sun, k = 3, fill = NA, align = "right"),
         Sun_03m = rollmean(Sun, k = 4, fill = NA, align = "right"),
         Sun_04m = rollmean(Sun, k = 5, fill = NA, align = "right"),
         Sun_05m = rollmean(Sun, k = 6, fill = NA, align = "right"),
         Sun_06m = rollmean(Sun, k = 7, fill = NA, align = "right"),
  ) %>%
  ungroup()




projname = "climunivar_excluZS_lags2"
# create folder structure for saving outputs
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")
if(!dir.exists(save_dir)){ 
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste(save_dir, "model_output/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "errors/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "models/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "fitmetrics/", sep=""), recursive = TRUE, showWarnings = FALSE) 
}


form_base = paste(
  c("y ~ 1",
    "offset(logpop)"),
  collapse = " + "
)


# fx = paste(
#   c("urban_log",
#     "transportation_log",
#     "Cropland_log",
#     "f(in_migration_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
#     "f(elevation_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
#     "f(Grassland_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)",
#     "f(Shrub_g, model='rw2', hyper=hyper2.rw, scale.model=TRUE, constr=TRUE)"),
#   collapse = " + "
# )

lags = c("Tmean","Tmean_1m","Tmean_2m","Tmean_3m","Tmean_4m","Tmean_5m","Tmean_6m",
         "Tmean_01m","Tmean_02m","Tmean_03m","Tmean_04m","Tmean_05m","Tmean_06m",
         "Precipitation","Precipitation_1m" ,"Precipitation_2m","Precipitation_3m" ,"Precipitation_4m","Precipitation_5m","Precipitation_6m",
         "Precipitation_01m" ,"Precipitation_02m","Precipitation_03m" ,"Precipitation_04m","Precipitation_05m","Precipitation_06m",
         "Rh","Rh_1m","Rh_2m","Rh_3m","Rh_4m","Rh_5m","Rh_6m",
         "Rh_01m","Rh_02m","Rh_03m","Rh_04m","Rh_05m","Rh_06m",
         "Sun","Sun_1m","Sun_2m","Sun_3m","Sun_4m","Sun_5m","Sun_6m",
         "Sun_01m","Sun_02m","Sun_03m","Sun_04m","Sun_05m","Sun_06m")



# lags = c("Tmean","Tmean_1m","Tmean_2m","Tmean_3m","Tmean_4m","Tmean_5m","Tmean_6m",
#          "Precipitation","Precipitation_1m" ,"Precipitation_2m","Precipitation_3m" ,"Precipitation_4m","Precipitation_5m","Precipitation_6m",
#          "Rh","Rh_1m","Rh_2m","Rh_3m","Rh_4m","Rh_5m","Rh_6m",
#          "Sun","Sun_1m","Sun_2m","Sun_3m","Sun_4m","Sun_5m","Sun_6m")


# create data frame
fx = data.frame(modid = 1:length(lags),
                fx = lags,
                formula = paste(form_base, lags, sep=" + ")
                #formula = paste(form_base, fx,lags, sep=" + ")
)
bs = data.frame(modid = "baseline", fx = "baseline",  formula=form_base)
fx = rbind(fx, bs)


# model names
fx$model_filename = paste("climuni_nb_model_", fx$modid, ".R", sep="")



# ================== fit models in an iterative loop ======================

# run model selection loop
for(i in 1:nrow(fx)){
  
  # formula
  fx_i = fx[ i, ]
  form_i = formula(as.vector(fx_i$formula))
  
  # fit INLA model nested in tryCatch: time out after 2 hours (7200 secs)
  e = simpleError("error fitting")
  
  # save storage by cutting all unnecessary variables and only keep specified variable
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
      yearx,
      cityx, 
      areaidx,
      in_migration_g,
      urban_log,
      transportation_log,
      elevation_g,
      Cropland_log,
      Shrub_g,
      Grassland_g
    ) %>%
    dplyr::mutate(y = total_cases) %>%
    cbind(ddf[ , grep(as.vector(fx_i$fx), names(ddf)), drop=FALSE])
  
  # fit model
  mod_i = tryCatch(
    fitINLAModel(form_i, dd_i, family="nbinomial", verbose=TRUE),
    error = function(e) return(e)
  )
  
  # write timeout to result
  if(class(mod_i)[1] == "simpleError"){
    
    ex = fx_i; ex$result = "error in fitting"
    ex_file_name = paste("climuni_nb_err_", fx_i$modid, ".csv", sep="")
    write.csv(ex, paste(save_dir, "errors/", ex_file_name, sep=""), row.names=FALSE)
    
    # otherwise calculate and save fit metrics and model
  } else{
    
    ff = mod_i$summary.fixed
    ff$param = row.names(ff)
    ff <- ff %>% slice(n())  #只留下lag变量的信息，即最后一行
    names(ff)[3:5] = c("lower", "median", "upper")
    ff[ 1:5 ] = exp(ff[ 1:5 ])
    
    res_i = cbind(fx_i, ff)
    fm_file_name = paste("climuni_nb_fixmetrics_", fx_i$modid, ".csv", sep="")
    write.csv(res_i, paste(save_dir, "fitmetrics/", fm_file_name, sep=""), row.names=FALSE)
    
    # save model
    save(mod_i, file=paste(save_dir, "models/", fx_i$model_filename, sep=""))
  }
  
} # end of model fitting loop





# directory where model objects are stored (large files so external)
models_dir = "./output/model_outputs/climunivar_excluZS_lags2/models"


# ============ view goodness of fit (DIC) metrics for all univariate models ===============

# Define the helper functions
countvars = function(x){ sapply(strsplit(as.vector(x), "[+]"), length) }
readfile = function(x){
  foo = read.csv(x)
  if(!"covar" %in% names(foo)){ foo$covar = "" }
  if(!"model_sub" %in% names(foo)){ foo$model_sub = "" }
  if(!"model_filename" %in% names(foo)){ foo$model_filename = "" }
  foo
}

# Load data
ff = list.files("./output/model_outputs/climunivar_excluZS_lags2/fitmetrics/", full.names=TRUE, pattern=".csv")
fx1 = do.call(rbind.data.frame, lapply(ff, readfile)) %>%
  dplyr::select(-formula)

# Handle modid conversion and arrange
fx1 <- fx1 %>%
  mutate(modid = suppressWarnings(as.numeric(as.character(modid)))) %>%
  arrange(modid)

# Add covariate groups
fx1$covar_grp = NA
fx1$covar_grp[ grep("Tmean", fx1$param) ] = "Temperature"
fx1$covar_grp[ grep("Precip", fx1$param) ] = "Precipitation"
fx1$covar_grp[ grep("Rh", fx1$param) ] = "Rh"
fx1$covar_grp[ grep("Sun", fx1$param) ] = "Sun"

# Filter and adjust numeric variables
fx1 <- fx1[!is.na(fx1$covar_grp), ]
numerical_vars <- fx1 %>% dplyr::select_if(is.numeric) %>% names()

#delete
# fx1 <- fx1 %>%
#   mutate(across(all_of(numerical_vars), ~ifelse(covar_grp == "Rh", .x + 0.009, .x))) %>%
#   mutate(across(all_of(numerical_vars), ~ifelse(covar_grp == "Sun", .x + 0.002, .x))) %>%
#   mutate(across(all_of(numerical_vars), ~ifelse(covar_grp == "Precipitation", .x + 0.002, .x)))

# Calculate limits
limits <- fx1 %>%
  group_by(covar_grp) %>%
  summarise(
    ymin = min(lower, na.rm = TRUE),
    ymax = max(upper, na.rm = TRUE),
    xmin = min(as.numeric(factor(param)), na.rm = TRUE),
    xmax = max(as.numeric(factor(param)), na.rm = TRUE)
  )

# Create the plot
p <- ggplot(fx1, aes(x = param, y = mean)) +
  facet_wrap(~ covar_grp, scales = "free_x", drop = FALSE) +
  geom_point(size = 1, shape = 21, fill = "blue4", color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "blue4") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Lag effects of climate variables",
    x = "Lag months",
    y = "Mean Estimate"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"),
    panel.background = element_rect(fill = grey(0.95)),
    axis.line = element_line(color = "black")
  ) +
  scale_x_discrete(labels = c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6",
                              "lag01", "lag02", "lag03", "lag04", "lag05", "lag06"))

# Print the plot
print(p)

library(viridis)
library(gridExtra)


# Create a list of individual plots for each facet with custom limits
plots <- lapply(unique(fx1$covar_grp), function(group) {
  
  group_data <- fx1 %>% 
    filter(covar_grp == group) %>%
    mutate(fx = as.factor(fx))
  
  group_limits <- limits %>% filter(covar_grp == group)
  
  p <- ggplot(group_data, aes(x = param, y = mean)) +
    facet_wrap(~ covar_grp, scales = "free_x", drop = FALSE) +
    geom_point(size = 1, shape = 21, fill = "#FF6347", color = "#FF6347") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "#CD5C5C") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
    theme_classic() +
    labs(
      x = "Lag months",
      y = "Relative risks"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      strip.text = element_text(size = 14, face = "bold"),
      panel.spacing = unit(1, "lines"),
      #panel.background = element_rect(fill = grey(0.95)),
      axis.line = element_line(color = "black")
    ) +
    coord_cartesian(ylim = c(group_limits$ymin, group_limits$ymax), 
                    xlim = c(group_limits$xmin, group_limits$xmax)) +
    scale_x_discrete(limits = group_data$fx, labels = c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6",
                                                        "lag01", "lag02", "lag03", "lag04", "lag05", "lag06"))
  #ggtitle(paste("Lag effects of", group))
})

# Combine all individual plots into one
library(gridExtra)
plag <- do.call(grid.arrange, plots)
plag
ggsave(plag, file="./output/figures/FigureS2_Lag effect2.pdf", device="pdf", width=11, height=6.5, units="in")


