
library(dplyr); library(sf);library(readxl);library(raster); library(rgdal);
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
library(RColorBrewer)

# working directory
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)

# ================= Build SFTS dataset =================

# shapefiles: district
shp <- st_read("./Data/shapefiles/县.shp") %>%
  filter(省代码 %in% c(330000)) %>%  ##筛选出浙江省的数据
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码,
         county = 县,
         countyid = 县代码
  ) 

#names(shp)
shp <- shp[,c(1,2,3,4,5,6,13)] 

shp_prov = st_read("./Data/shapefiles/省.shp") %>%
  filter(省代码 %in% c(330000)) 

shp_prov <- shp_prov[ ,c(1,2,4)] 

st_crs(shp_prov) = st_crs(shp)  #将shp_prov的坐标参考系统（CRS）设置为与shp相同的CRS,确保两个数据集具有相同的坐标系统
shp_prov = st_crop(shp_prov, shp) #对shp_prov进行裁剪，使其与shp相交的部分保留


shp_city = st_read("./Data/shapefiles/市.shp") %>%
  filter(省代码 %in% c(330000)) %>%
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码) 
shp_city <- shp_city[,c(1,2,3,4,7)]
st_crs(shp_city) = st_crs(shp)  
shp_city = st_crop(shp_city, shp)


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
# 创建一个包含所有月份和有过病例发生的地区的数据框
date_range <- seq(as.Date("2011-01-01"), as.Date("2023-12-01"), by="month")

ddregion <- unique(dd_monthly$countyid)

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

dd1 <- left_join(dd1,pop, by = c("city","year"))

dd1$incidence <- (dd1$total_cases / dd1$pop) * 1000

#save(dd1, file = "./output/data_process/sfts_caseALL.RData")
#write.csv(dd1, file = "./output/data_process/sfts_caseALL.csv",fileEncoding = "GB18030")



#################################################################################
library(readxl)

# working directory
PATH = dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
setwd(PATH)


########### mean Temperature data ############################################################
Tmean <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/1981-2023年逐月平均气温/1981-2023年逐月平均气温.csv",fileEncoding = "GB18030") %>%
  filter(省代码 %in% c(330000))

Tmean=melt(Tmean,
           id=c(names(Tmean)[c(1:5)]),
           measure.vars=c(names(Tmean)[-c(1:5)]))


Tmean <- Tmean %>%
  mutate(variable = sub("^X(\\d{6})", "\\1", variable))

Tmean = Tmean %>%
  subset(select=-c(1))

names(Tmean) <- c("province","provinceid","city","cityid","date","Tmean")

Tmean$date <-parse_date_time(Tmean$date,orders = "ym")

#colSums(is.na(Tmean))
Tmean <- na.omit(Tmean)

Tmean$date <- as.Date(Tmean$date)


########### min Temperature data  ############################################################
Tmin <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/1981-2023年逐月最低气温/1981-2023年逐月最低气温.csv",fileEncoding = "GB18030") %>%
  filter(省代码 %in% c(330000))

Tmin=melt(Tmin,
          id=c(names(Tmin)[c(1:5)]),
          measure.vars=c(names(Tmin)[-c(1:5)]))


Tmin <- Tmin %>%
  mutate(variable = sub("^X(\\d{6})", "\\1", variable))

Tmin = Tmin %>%
  subset(select=-c(1))

names(Tmin) <- c("province","provinceid","city","cityid","date","Tmin")

Tmin$date <-parse_date_time(Tmin$date,orders = "ym")

colSums(is.na(Tmin))
Tmin <- na.omit(Tmin)

Tmin$date <- as.Date(Tmin$date)


########### max Temperature data ############################################################
Tmax <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/1981-2023年逐月最高气温/1981-2023年逐月最高气温.csv",fileEncoding = "GB18030") %>%
  filter(省代码 %in% c(330000))

Tmax=melt(Tmax,
          id=c(names(Tmax)[c(1:5)]),
          measure.vars=c(names(Tmax)[-c(1:5)]))


Tmax <- Tmax %>%
  mutate(variable = sub("^X(\\d{6})", "\\1", variable))

Tmax = Tmax %>%
  subset(select=-c(1))

names(Tmax) <- c("province","provinceid","city","cityid","date","Tmax")

Tmax$date <-parse_date_time(Tmax$date,orders = "ym")

colSums(is.na(Tmax))
Tmax <- na.omit(Tmax)

Tmax$date <- as.Date(Tmax$date)



########### Precipitation data  ############################################################
Precipitation <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/1981-2023年逐月降水量/1981-2023年逐月降水量.csv",fileEncoding = "GB18030") %>%
  filter(省代码 %in% c(330000))

Precipitation=melt(Precipitation,
                   id=c(names(Precipitation)[c(1:5)]),
                   measure.vars=c(names(Precipitation)[-c(1:5)]))


Precipitation <- Precipitation %>%
  mutate(variable = sub("^X(\\d{6})", "\\1", variable))

Precipitation = Precipitation %>%
  subset(select=-c(1))

names(Precipitation) <- c("province","provinceid","city","cityid","date","Precipitation")

Precipitation$date <-parse_date_time(Precipitation$date,orders = "ym")

colSums(is.na(Precipitation))
Precipitation <- na.omit(Precipitation)

Precipitation$date <- as.Date(Precipitation$date)

######################################################################################
#omit similar codes for other climatic variables

######Combine data#################################################################################

Tmean$date <- as.Date(Tmean$date)
Tmax$date <- as.Date(Tmax$date)
Tmin$date <- as.Date(Tmin$date)
Precipitation$date <- as.Date(Precipitation$date)
Wind_monthly$date <- as.Date(Wind_monthly$date)
Rh_monthly$date <- as.Date(Rh_monthly$date)
#Sun_monthly <- meteorology_monthly[ ,c(1,2,3,8)]
Sun_monthly$date <- as.Date(Sun_monthly$date)

all <- left_join(Tmean, Tmin, by = c("date","cityid","city","province","provinceid") ) %>%
  left_join(Tmax, by = c("date","cityid","city","province","provinceid") ) %>%
  left_join(Precipitation, by = c("date","cityid","city","province","provinceid") ) %>%
  left_join(Wind_monthly, by = c("date","cityid","city") ) %>%
  left_join(Rh_monthly, by = c("date","cityid","city") ) %>%
  left_join(Sun_monthly, by = c("date","cityid","city") ) 


all <- all %>%
  filter(year(date) > 2010)


write.csv(all, file = "E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS/output/data_process/Meteorologydata.csv",fileEncoding = "GB18030")

save(all, file = "./R code/SFTS/Analysis/meteorology_monthly.RData")



# create lags
# subset to only env data, rename, add X months to date (to bring up to lag time) and then left_join to clim
Meteorologydata <- read.csv("./output/data_process/Meteorologydata.csv",fileEncoding = "GB18030")%>%
  dplyr::mutate(date = as.Date(date))

clim = Meteorologydata
c2 = clim
for(lag in 1:6){
  lag1 = c2 
  new_names = unlist(lapply(strsplit(names(lag1)[3:ncol(lag1)], "_"), "[", 1))
  names(lag1)[3:ncol(lag1)] = paste0( new_names, "_", lag, "m", sep="")
  lag1$date = ymd(lag1$date) %m+% months(lag)
  clim = left_join(clim, lag1)
}


library(dplyr)
library(zoo)  # 包含 rollmean 函数



# Cumulative effect
#ddf <- ddf %>%
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


write.csv(clim, file = "E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS/output/data_process/Meteorologydata_Lags.csv",fileEncoding = "GB18030")




#################################################################################


# ================ Combine dengue data and covariates from socioeconomic / earth observation into dataset for modelling ===============

# script combines all covariates produced using "process" scripts with dengue incidence data
# produces final dataset for use in statistical models
# scripts that produced each constituent dataset/covariate are named below

# project root and dependencies
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)
pacman::p_load("dplyr", "raster", "rgdal", "sf", "ecmwfr", "stringr", "ggplot2", "lubridate", "magrittr", "vroom")
library(terra)


################### land cover data, area of Cropland,Forest,Grassland ##################################
landcover_list <- list()

for (year in 2011:2022){
  file_path = paste(c("./Rawdata/1990-2022年我国省市县三级的各类土地覆盖面积/excel格式的数据/按区县统计的用地数据/CLCD_v01_", 
                      year, "_县.csv"), collapse="")
  
  landcover_list[[year]] <- read.csv(file_path)
}

landcover <- do.call(rbind, landcover_list) 

landcover <-landcover %>%
  filter(省代码 %in% c(330000)) %>%
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码,
         county = 县,
         countyid = 县代码,
         year = 年份)

#导入县面积，计算覆盖率
Countyarea <- read.csv("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/County area.csv",fileEncoding = "GB18030") %>%
  dplyr::select(1,2)

landcover <- left_join(landcover,Countyarea, by = "county")

colSums(is.na(landcover))

landcover <- landcover %>%
  mutate(Cropland_p = Cropland / countyarea,
         Forest_p = Forest / countyarea,
         Water_p = Water / countyarea,
         Grassland_p = Grassland / countyarea,
         Barren_p = Barren / countyarea,
         Impervious_p = Impervious / countyarea,
         Shrub_p = Shrub / countyarea )


landcover <- landcover %>%
  select(c("city","cityid","county","countyid","year","Cropland","Forest","Grassland",
           "Water","Barren","Impervious","Shrub","Cropland_p","Forest_p","Grassland_p",
           "Water_p","Barren_p","Impervious_p","Shrub_p","countyarea" ))


write.csv(landcover,file = "./output/data_process/landcover.csv",fileEncoding = "GB18030")

################################ DEM data ###############################################
# 读取栅格数据
dem_list <- list()

for (iii in c("杭州市", "湖州市", "嘉兴市", "金华市", "丽水市",
              "宁波市", "衢州市", "绍兴市", "台州市","温州市", "舟山市")){
  
  file_path = paste(c("E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/浙江省地形/分城市的数据", 
                      iii, "/", iii, "_COP30.tif" ), collapse="")
  
  terrainX <- raster(file_path)
  #plot(terrainX)
  #summary(values(terrainX))
  #terrain <- as.data.frame(terrain,xy=TRUE)
  #terrain <- na.omit(terrain)
  
  # 读取县级边界矢量数据（假设为Shapefile）
  shpX <- st_read("./Data/shapefiles/县.shp") %>%
    filter(市 %in% iii) %>%  ##筛选出浙江省的数据
    rename(province = 省,  
           provinceid = 省代码,
           city = 市,
           cityid = 市代码,
           county = 县,
           countyid = 县代码
    ) 
  
  shpX <- shpX[,c(1,2,3,4,5,6,13)] 
  
  # 确保栅格数据和矢量数据的CRS是一致的 
  #terrainX <- projectRaster(terrainX, crs = st_crs(shpX) , over = TRUE) ##不知道为什么跑不通，直接设置一下
  
  # 设置栅格数据的CRS为EPSG:4326
  #st_crs(terrainX)
  #st_crs(shp)
  target_crs <- CRS("+init=epsg:4326")
  terrainX <- projectRaster(terrainX, crs = target_crs, over = TRUE) 
  
  # 将栅格数据提取到县级边界
  aggregated_data <- raster::extract(terrainX, shpX, fun = mean, na.rm = TRUE)
  
  # 将结果转换为数据框
  aggregated_df <- as.data.frame(aggregated_data)
  
  county_summary <- cbind(shpX, aggregated_df) 
  
  county_summary = county_summary %>%
    rename(elevation = V1 )
  
  dem_list[[iii]] <-county_summary
  
  print(iii)
  
}

DEM <- do.call(rbind, dem_list)

rownames(DEM) <- c()

DEM <- DEM %>%
  select(c("city","cityid","county","countyid","elevation"))

DEM <- read.csv("./output/data_process/DEM.csv",fileEncoding = "GB18030")
DEM <- na.omit(DEM)

#write.csv(DEM,file = "./output/data_process/DEM.csv",fileEncoding = "GB18030")



##################### human mobility data #####################################
#In-Migration Scale Index
directory_path <- "E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/Baidu Migration Data/Zhejiang/in"
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
#csv_files
library(dplyr)
In_migration <- NULL

# 循环读取每个CSV文件，并使用bind_rows()函数合并数据
for (file in csv_files) {
  temp_data <- read.csv(file, fileEncoding = "GB18030")
  
  In_migration <- bind_rows(In_migration, temp_data)
}

names(In_migration) <- c("province","city","type","date","in_migration")

In_migration$date <- as.Date(In_migration$date)

In_migration <- In_migration %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m"),
         yearmonth = format(date, "%Y-%m")) %>%
  filter( year <= 2023 )

In_migration$in_migration <- as.numeric(In_migration$in_migration)

In_migration_month <- In_migration %>%
  group_by(city, year,month,yearmonth) %>%
  summarise( in_migration = mean(in_migration, na.rm = TRUE))


#Out-Migration Scale Index
directory_path <- "E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/Baidu Migration Data/Zhejiang/out"
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
#csv_files

Out_migration <- NULL

# 循环读取每个CSV文件，并使用bind_rows()函数合并数据
for (file in csv_files) {
  temp_data <- read.csv(file, fileEncoding = "GB18030")
  
  Out_migration <- bind_rows(Out_migration, temp_data)
}

names(Out_migration) <- c("province","city","type","date","out_migration")

Out_migration$date <- as.Date(Out_migration$date)

Out_migration <- Out_migration %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m"),
         yearmonth = format(date, "%Y-%m")) %>%
  filter( year <= 2023 )

Out_migration$out_migration <- as.numeric(Out_migration$out_migration)

Out_migration_month <- Out_migration %>%
  group_by(city, year,month,yearmonth) %>%
  summarise( out_migration = mean(out_migration, na.rm = TRUE))

migration <- full_join(In_migration_month, Out_migration_month, by=c("city","year","month","yearmonth"))

migration$city <- paste0(migration$city, "市")


#write.csv(migration, file = "./output/data_process/migration.csv",fileEncoding = "GB18030")





