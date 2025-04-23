
################## build dataset including all counties ########################
shp <- st_read("./Data/shapefiles/县.shp") %>%
  filter(省代码 %in% c(330000)) %>%  
  dplyr::filter(!市代码 %in% offshore_areas) %>%
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码,
         county = 县,
         countyid = 县代码
  ) 

#names(shp)
shp <- shp[,c(1,2,3,4,5,6,13)] 



# SFTS cases data
dd = read_excel("./Data/SFTScases.xlsx") 

# 将areaid转换为字符型，以确保字符串操作的正确性
dd$areaid <- as.character(dd$areaid)
dd <- dd %>% mutate(
  provinceid = str_c(substring(areaid, 1, 2), "0000"),
  cityid      = str_c(substring(areaid, 1, 4), "00"),
  countyid    = substring(areaid, 1, 6)
)

dd$cases <- 1

dd <- dd %>%
  mutate(death_date = ifelse(death_date == ".", NA, death_date)) %>%
  mutate(death = ifelse(is.na(death_date), 0, 1)) 

dd <- dd %>%
  mutate(
    incidence_date = as.Date(incidence_date, format="%Y-%m-%d"),  
    year = format(incidence_date, "%Y"),
    month = format(incidence_date, "%m"),
    yearmonth = format(incidence_date, "%Y-%m"),
  )


dd <- dd %>%
  filter(year >= 2011 & year <= 2023)

# mean annual incidence across years
epochs = data.frame(year = 2011:2023,
                    epoch=c(rep("2011-2013", length(2011:2013)), rep("2014-2016", length(2014:2016)), 
                            rep("2017-2019", length(2017:2019)), rep("2020-2023", length(2020:2023))))


dd$year <- as.integer(dd$year)
dd <- left_join(dd, epochs, by = "year")

dd_monthly <- dd %>%
  group_by(epoch,year, month, yearmonth, countyid, cityid, provinceid)%>%
  summarise(total_cases = sum(cases, na.rm = TRUE), 
            total_deaths = sum(death, na.rm = TRUE)) 

dd_monthly$date <- as.Date(paste0(dd_monthly$yearmonth, "-01"))

regionslookup <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS/Data/shapefiles/regionslookup.csv",fileEncoding = "GB18030")

dd_monthly$countyid <- as.integer(dd_monthly$countyid)
dd_monthly <- left_join(dd_monthly,regionslookup[,c("countyid","county","city")],by=c("countyid"))

#sum(dd_monthly$total_cases)
#sum(dd_monthly$total_deaths)

#没有发生病例的月份也要体现在数据集中
# 创建一个包含所有月份和所有地区的数据框 regionslookup
date_range <- seq(as.Date("2011-01-01"), as.Date("2023-12-01"), by="month")

ddregion <- unique(shp$countyid)

total_index <- expand.grid(
  date = date_range,
  countyid = ddregion
)

dd1 <- left_join(total_index,dd_monthly[,c("date","countyid","total_cases","total_deaths")], by = c("date","countyid") )

dd1 <- dd1 %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d"),  
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m"),
  )

dd1$year <- as.integer(dd1$year)
dd1 <- left_join(dd1, epochs, by = "year")

regionslookup <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS/Data/shapefiles/regionslookup.csv",fileEncoding = "GB18030")

dd1$countyid <- as.integer(dd1$countyid)
dd1 <- left_join(dd1,regionslookup[,c("countyid","county","cityid","city")],by=c("countyid"))

dd1$total_cases[is.na(dd1$total_cases)] <- 0

dd1$total_deaths[is.na(dd1$total_deaths)] <- 0



##population
pop = read.csv("./Data/yearbook.csv",fileEncoding = "GB18030") 

pop_area <- pop %>%
  group_by(city) %>%
  summarise(population = mean(pop,na.rm = TRUE), .groups = 'keep') 

pop_yr <- pop %>%
  group_by(year) %>%
  summarise(population = sum(pop,na.rm = TRUE), .groups = 'keep') 


dd1 <- left_join(dd1,pop, by = c("city","year"))

dd1$incidence <- (dd1$total_cases / dd1$pop) * 1000

#save(dd1, file = "./output/data_process/sfts_caseALL.RData")
#write.csv(dd1, file = "./output/data_process/sfts_caseALL_new(all counties).csv",fileEncoding = "GB18030")

MeteorologydataLag <- read.csv("./output/data_process/Meteorologydata_Lags.csv",fileEncoding = "GB18030")%>%
  dplyr::mutate(date = as.Date(date))

landcover <- read.csv("./output/data_process/landcover.csv",fileEncoding = "GB18030")

DEM <- read.csv("./output/data_process/DEM.csv",fileEncoding = "GB18030")

migration <- read.csv("./output/data_process/migration.csv",fileEncoding = "GB18030")

dd <- read.csv("./output/data_process/sfts_caseALL_new(all counties).csv",fileEncoding = "GB18030") %>%
  dplyr::filter(!cityid %in% c(330900)) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::left_join(landcover, by=c("city","cityid","county","countyid","year")) 

dd <- left_join(dd,DEM, by=c("city","cityid","county","countyid"))

dd <- left_join(dd,migration[,c("city","year","month","in_migration","out_migration")], by=c("city","year","month"))

dd <- left_join(dd, MeteorologydataLag, by=c("date","city","cityid")) 

# subset shapefile to only locations within example dataset
shp = shp %>% dplyr::filter(countyid %in% dd$countyid)

sum(dd$total_cases)
sum(dd$total_deaths)
# =============== group predictors for fitting nonlinear effects ==================


# group predictors for fitting nonlinear effects 
# do this before subsetting to geographical subregions to ensure consistency for prediction/projection

print("Grouping climate predictors")

nbins = n_clim_bins
dx = dd[ , grep("Tmean|Tmin|Tmax|Precip|Wind|Rh|Sun", names(dd))]
names(dx) = paste(names(dx), "_g", sep="")

groupCols = function(x){
  x = dx[ , x, drop=FALSE ]
  x[ , 1] = inla.group(x[ , 1], n=nbins)
  x
}
dx = do.call(cbind.data.frame, lapply(1:ncol(dx), groupCols))
dd = cbind(dd, dx)






# ================ subset to specified region(s), if specified =================
# 生成region变量
dd$region1 <- ifelse(dd$cityid %in% c(330500, 330100, 330400), "1",
                     ifelse(dd$cityid %in% c(330800, 331100, 330300), "2",
                            ifelse(dd$cityid %in% c(330200, 330600, 331000, 330700), "3", NA)))

if(region != "all"){
  if(!region %in% unique(dd$region1)){
    print("Region not recognised: defaulting to 'all'")
  }
  else{
    dd = dd[ dd$region1 %in% c(region), ]
    shp = shp[ shp$countyid %in% dd$countyid, ]
    shp_city = shp_city[ shp_city$cityid %in% dd$cityid, ]
  }
}









# =============== setup covariates, transform, scale and set grouping factors ====================

# defining offset as log population (in hundreds of thousands)
#  model is estimating incidence per 10,000,000 inhabitants
dd$logpop = log(dd$pop/1000)

# replicate variables for grouping
areaidx = factor(dd$countyid, levels=unique(dd$countyid)[ order(unique(dd$countyid)) ], ordered=TRUE)
dd$areaidx = as.integer(areaidx)
dd$areaidy = as.integer(areaidx)
dd$yearx = as.integer(as.factor(dd$year))
dd$cityx = as.integer(factor(dd$city, levels=unique(dd$city)[ order(unique(dd$city)) ], ordered=TRUE))
dd$cityy = dd$cityx

#standardized and weighted the migration
dd <- dd %>%
  group_by(city) %>%  # 按city分组
  mutate(
    out_migration_norm = (out_migration - min(out_migration, na.rm = TRUE)) / (max(out_migration, na.rm = TRUE) - min(out_migration, na.rm = TRUE)),
    out_migration_norm = pmax(pmin(out_migration_norm, 1), 0)
  ) %>%
  ungroup()  # 取消分组

dd <- dd %>%
  group_by(city) %>%  # 按city分组
  mutate(
    out_migration_norm2 = (out_migration - min(out_migration, na.rm = TRUE)) / (max(out_migration, na.rm = TRUE) - min(out_migration, na.rm = TRUE)),
    out_migration_norm2 = pmax(pmin(out_migration_norm2, 1), 0),
    in_migration_norm = (in_migration - min(in_migration, na.rm = TRUE)) / (max(in_migration, na.rm = TRUE) - min(in_migration, na.rm = TRUE)),
    in_migration_norm = pmax(pmin(in_migration_norm, 1), 0)
  ) %>%
  ungroup()  # 取消分组

# different levels of region 


#dd$regionx = as.integer(as.factor(dd$region1))
#dd$regiony = as.integer(as.factor(dd$region2))
#dd$regionz = as.integer(as.factor(dd$region3))




################## build projection dataset ####################################

#historical dataset
# create dataframe for modelling, response and family
ddf = dd


library(zoo) #rollmean function
ddf <- ddf %>%
  arrange(countyid, date) %>%  # 按城市和日期排序
  group_by(countyid) %>%
  mutate(Precipitation_02m = rollmean(Precipitation, k = 3, fill = NA, align = "right"),
         Rh_06m = rollmean(Rh, k = 7, fill = NA, align = "right"),
         Sun_01m = rollmean(Sun, k = 2, fill = NA, align = "right")
  ) %>%
  ungroup()


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
    Rh_3m_g,
    Rh_6m_g,
    Precipitation_02m,
    Rh_06m,
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


names(ddf)

#save(ddf, file = "./output/data_process/ModelData_sfts_excluZS_new(allcounties).RData")

#################################################################################


# 生成2024-2030年的日期数据
years <- 2024:2030
months <- 1:12

date_df <- expand.grid(year = years, month = months)

# 城市信息

county_name <- ddf %>%
  filter(year == 2023) %>%
  mutate(cityid = as.character(cityid)) %>%
  select(city,cityid,countyid,county) %>% 
  distinct()

city_name <- data.frame(
  cityid = c("330100","330200","330300","330400","330500","330600","330700","330800",
             "330900","331000","331100"),
  city_name = c("Hangzhou","Ningbo","Wenzhou","Jiaxing","Huzhou","Shaoxing","Jinhua",
                "Quzhou","Zhoushan","Taizhou","Lishui"),
  city = c("杭州市", "宁波市", "温州市", "嘉兴市", "湖州市", "绍兴市",
           "金华市", "衢州市", "舟山市", "台州市", "丽水市")
) %>% mutate(cityid = as.character(cityid))

# 组合日期数据和城市信息
proj_dates <- expand.grid(year = years, month = months, countyid = county_name$countyid) %>%
  left_join(county_name, by = "countyid") %>%
  left_join(city_name, by = c("cityid","city"))

# 使用2023年的模板数据
proj_df <- ddf %>%
  filter(year == 2023) %>%
  mutate(cityid = as.character(cityid))

#某些变量2023年是缺失的用填补数据集填补
library(mice)
imputed_data <- mice(ddf, m = 5, method = 'rf', maxit = 5)
#imputed_data <- mice(ddf, m = 5, method = 'pmm', maxit = 5)
completed_data <- complete(imputed_data, 1)

ddf_imputed <- completed_data %>%
  filter(year == 2023) %>%
  select(countyid,cityid,month,Sun,Rh,transportation,transportation_log,Rh_3m_g,Sun_01m_g,
         Cropland,Shrub,Grassland,Forest,Cropland_log,Grassland_g,Shrub_g)%>%
  mutate(cityid = as.character(cityid))



# 生成预测数据集
proj_df_new <- proj_dates %>%
  left_join(proj_df %>% select(-year, -date, -total_cases, -Tmean_1m_g, -city, -county,
                               -Sun,-Rh,-transportation,-transportation_log,-Rh_3m_g,-Sun_01m_g,
                               -Cropland,-Shrub,-Grassland,-Forest,-Cropland_log,-Grassland_g,-Shrub_g), by = c("countyid","cityid","month")) %>%
  left_join(ddf_imputed, by = c("countyid","cityid","month")) %>%
  mutate(Tmean_1m_g = NA,  # 在此处替换为未来的预测气温数据
         total_cases = NA) # 将total_cases设为NA

proj_df_new$date <- as.Date(paste0(proj_df_new$year,"-",proj_df_new$month, "-01"))

names(proj_df_new)

# 将预测数据集转换为原始数据集的格式
proj_df_new <- proj_df_new %>%
  select(names(proj_df))

#### 整合预测气温变量
load(file = "./output/data_process/future_temp_df_MRI-ESM2-0_ssp585.RData")
future_temp_df_new1 <- future_temp_df %>%
  filter(year > 2023) %>%
  mutate(Scenarios = "SSP585",
         Tmean_ssp585 = Temp_correct) 

load(file = "./output/data_process/future_temp_df_MRI-ESM2-0_ssp245.RData")
future_temp_df_new2 <- future_temp_df %>%
  filter(year > 2023) %>%
  mutate(Scenarios = "SSP245",
         Tmean_ssp245 = Temp_correct) 

load(file = "./output/data_process/future_temp_df_MRI-ESM2-0_ssp119.RData")
future_temp_df_new3 <- future_temp_df %>%
  filter(year > 2023) %>%
  mutate(Scenarios = "SSP119",
         Tmean_ssp119 = Temp_correct) 


# 将 sf 对象转换为 data.frame
future_temp_df_new1_df <- st_drop_geometry(future_temp_df_new1)
future_temp_df_new2_df <- st_drop_geometry(future_temp_df_new2)
future_temp_df_new3_df <- st_drop_geometry(future_temp_df_new3)

# 使用 left_join 进行合并
future_temp_df_new <- future_temp_df_new1_df[,c("year","month","cityid","city","Tmean_ssp585")] %>%
  left_join(future_temp_df_new2_df[,c("year","month","cityid","city","Tmean_ssp245")], 
            by = c("year", "month", "cityid", "city")) %>%
  left_join(future_temp_df_new3_df[,c("year","month","cityid","city","Tmean_ssp119")], 
            by = c("year", "month", "cityid", "city")) %>%
  mutate(cityid = as.character(cityid),
         date = as.Date(paste0(year,"-",month, "-01")) 
  )

#write.csv(future_temp_df_new,file = "E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS/output/data_process/future_temp_df_MRI-ESM2-0_ssp119_245_585.csv",fileEncoding = "GB18030") 

future_temp_df_new$date = ymd(future_temp_df_new$date) %m+% months(1)  #lag 1 month

proj_df_new2 <- left_join(proj_df_new,future_temp_df_new[ ,c("date", "cityid","Tmean_ssp585","Tmean_ssp245","Tmean_ssp119")])

proj_df_new2 <- proj_df_new2 %>%
  select(names(proj_df),"Tmean_ssp585","Tmean_ssp245","Tmean_ssp119") %>%
  mutate(date=as.Date(date))


########### combined historical data and projection data ################################
names(proj_df_new2)
names(ddf)
ddf <- ddf %>%
  mutate(Tmean_ssp585=NA,Tmean_ssp245=NA,Tmean_ssp119=NA)

proj_df_final <- rbind(ddf,proj_df_new2) 
proj_df_final$date <- as.Date(paste0(proj_df_final$year,"-",proj_df_final$month, "-01"))

#save(proj_df_final,file = "output/data_process/proj_df_final.RData")

####### third version: used the imputed historical dataset ddf and linear imputed covariates########################

library(dplyr)
library(zoo)

# 确保数据按照时间排序
proj_df_final2 <- proj_df_final2 %>% arrange(city, county, year, month)

# 定义市级和县级变量
city_level_vars <- c("urban_log", "transportation_log", "in_migration_g")
county_level_vars <- c("Cropland_log", "Grassland_g", "Shrub_g", "elevation_g")

# 创建新的数据集，包含2011-2023的历史数据和2024-2030的预测数据
historical_data <- proj_df_final2 %>% filter(year <= 2023)
future_data <- proj_df_final2 %>% filter(year > 2023)

# 对市级变量进行线性插值
for (var in city_level_vars) {
  for (city_id in unique(historical_data$city)) {
    city_data <- historical_data %>% filter(city == city_id)
    interpolated_values <- na.approx(city_data[[var]], x = city_data$year, xout = future_data %>% filter(city == city_id) %>% pull(year), method = "linear", rule = 2)
    future_data <- future_data %>% mutate(!!var := ifelse(city == city_id, interpolated_values, !!sym(var)))
  }
}

# 对县级变量进行线性插值
for (var in county_level_vars) {
  for (county_id in unique(historical_data$county)) {
    county_data <- historical_data %>% filter(county == county_id)
    interpolated_values <- na.approx(county_data[[var]], x = county_data$year, xout = future_data %>% filter(county == county_id) %>% pull(year), method = "linear", rule = 2)
    future_data <- future_data %>% mutate(!!var := ifelse(county == county_id, interpolated_values, !!sym(var)))
  }
}

# 将历史数据和插值后的未来数据合并
proj_df_final4 <- bind_rows(historical_data, future_data)

save(proj_df_final4, file = "output/data_process/proj_df_final4_new(allcounties).RData")

##################################################################################
# visualize
library(tidyr)

proj_df_finalv <- proj_df_final4 %>%
  group_by(year,city) %>%
  summarise(Tmean_ssp585 = mean(Tmean_ssp585,na.rm = TRUE),
            Tmean_ssp245 = mean(Tmean_ssp245,na.rm = TRUE),
            Tmean_ssp119 = mean(Tmean_ssp119,na.rm = TRUE)) 

proj_df_finalv$year <- as.character(proj_df_finalv$year)

proj_df_long <- proj_df_finalv %>%
  filter(year>2023)%>%
  pivot_longer(cols = starts_with("Tmean_ssp"), 
               names_to = "Scenario", 
               values_to = "Temperature")

ggplot(proj_df_long, aes(x = year, y = Temperature, color = Scenario, group = Scenario)) +
  geom_line() +
  facet_wrap(~ city, scales = "free_y") +  # 按城市分面
  labs(title = "Temperature Change by Year and City under Different SSP Scenarios",
       x = "Year", 
       y = "Mean Temperature (°C)",
       color = "SSP Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################################################################################


############  predicting modeling    ###############################################


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


####################### SSP 245 #################################################
#############  Using data from 2011-2023 to predict for 2024-2028 ###############

predic_func <- function(trainyear1,trainyear2,valyear1,valyear2){
  
  load(file = "output/data_process/proj_df_final4_new(allcounties).RData")
  
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
  
  
  save(mod_i, file=paste(save_dir, "models/", "Temp_proj_model_train",trainyear2,"_",valyear2,"new(allcounties).R", sep=""))
  
  # 获取预测值和置信区间
  ####### calculate the 95%CI for fitted cases ###################################
  
  load(file=paste(save_dir, "models/", "Temp_proj_model_train",trainyear2,"_",valyear2,"new(allcounties).R", sep=""))
  
  index_val = inla.stack.index(stack, "val")$data
  
  predictions = mod_i$summary.fitted.values[index_val, "mean"]
  val_data$fitted.values <- predictions
  
  predictions_95lower = mod_i$summary.fitted.values[index_val, "0.025quant"]
  val_data$fitted.values_lower <- predictions_95lower
  
  
  predictions_95upper = mod_i$summary.fitted.values[index_val, "0.975quant"]
  val_data$fitted.values_upper <- predictions_95upper 
  
  val_data$sd <- mod_i$summary.fitted.values[index_val, "sd"]
  
  
  
  index_val = inla.stack.index(stack, "train")$data
  
  predictions = mod_i$summary.fitted.values[index_val, "mean"]
  train_data$fitted.values <- predictions
  
  predictions_95lower = mod_i$summary.fitted.values[index_val, "0.025quant"]
  train_data$fitted.values_lower <- predictions_95lower
  
  predictions_95upper = mod_i$summary.fitted.values[index_val, "0.975quant"]
  train_data$fitted.values_upper <- predictions_95upper 
  
  train_data$sd <- mod_i$summary.fitted.values[index_val, "sd"]
  
  Temp_proj_fitted_result <- rbind(train_data,val_data)
  
  save(Temp_proj_fitted_result, file=paste(save_dir, "Temp_proj_model_result_train",trainyear2,"_val",valyear2,"new(allcounties).RData", sep=""))
  
}




predic_func(2011,2023,2024,2028)


