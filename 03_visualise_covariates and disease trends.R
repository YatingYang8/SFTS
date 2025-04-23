
################## disease  distribute ######################################

library(dplyr); library(sf);library(readxl);library(raster); library(rgdal);
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
library(RColorBrewer)
library(viridis)

# working directory
PATH = "E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS"
setwd(PATH)

#Seasonal Trends of SFTS
#Fig3_sfts_season <- dd_season
#save(Fig3_sfts_season, file = "./codes for figures/Fig3/Fig3_sfts_season.RData")
load(file = "./codes for figures/Fig3/Fig3_sfts_season.RData")


city_groups <- list(
  group1 = c("Huzhou", "Jiaxing","Hangzhou"),  
  group2 = c("Jinhua", "Ningbo","Shaoxing","Taizhou"),  
  group3 = c("Quzhou", "Lishui", "Wenzhou" )
)


city_colors <- c(
  setNames(scales::alpha(RColorBrewer::brewer.pal(3, "Blues"), 0.8), city_groups$group1),
  setNames(scales::alpha(RColorBrewer::brewer.pal(4, "Reds"), 0.8), city_groups$group2),
  setNames(scales::alpha(RColorBrewer::brewer.pal(3, "Purples"), 0.8), city_groups$group3)
)

Fig3_sfts_season$month <- as.numeric(Fig3_sfts_season$month)

Fig3_sfts_season$city <- factor(Fig3_sfts_season$city, levels = c("Huzhou", "Jiaxing","Hangzhou", "Jinhua", "Ningbo","Shaoxing","Taizhou","Quzhou","Lishui", "Wenzhou"))

summary(Fig3_sfts_season$total_cases)

# 使用这些自定义颜色进行绘图
p1 <- ggplot(Fig3_sfts_season, aes(x = month, y = total_cases, group = city, color = city)) +
  geom_line(size = 1) +  
  scale_color_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Seasonal trends of SFTS", x = "Month", y = "Number of SFTS Cases") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    legend.position = "right",  # 隐藏图例
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(ncol = 1, byrow = TRUE))+
  scale_x_continuous(breaks = seq(2, 12, by = 2))  # 图例水平排列

# 打印图形
print(p1)

#ggsave(p1, file=paste0(path,"Fig2_sfts_season", ".pdf"), device="pdf", width=4, height=4.5, units="in")



################### Annual trends of climate variable ###########################

#Annual_data <- dd_year
#save(Annual_data, file = "./codes for figures/Fig3/Annual_data.RData")
load(file = "./codes for figures/Fig3/Annual_data.RData")

Annual_data$year <- as.numeric(Annual_data$year)
Annual_data$city <- factor(Annual_data$city, levels = c("Huzhou", "Jiaxing","Hangzhou", "Jinhua", "Ningbo","Shaoxing","Taizhou","Quzhou","Lishui", "Wenzhou"))

p6 <- ggplot(Annual_data, aes(x = year, y = Tmean, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Mean temperature", x = "Year", y = "Mean temperature (°C)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + # 图例水平排列
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

p6
#ggsave(p6, file=paste0(path,"Fig2_Tmean_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")

p7 <- ggplot(Annual_data, aes(x = year, y = Rh, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Relative humidity", x = "Year", y = "Rh (%)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + # 图例水平排列
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

p7
#ggsave(p7, file=paste0(path,"Fig2_Rh_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")


p8 <- ggplot(Annual_data, aes(x = year, y = Precipitation, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Monthly precipitation", x = "Year", y = "Precipitation (mm)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

print(p8)
#ggsave(p8, file=paste0(path,"Fig2_Precipitation_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")


p9 <- ggplot(Annual_data, aes(x = year, y = Sun, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Sun duration", x = "Year", y = "Sun duration (hours)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(ncol = 1, bycol = TRUE))  +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

print(p9)
#ggsave(p9, file=paste0(path,"Fig2_Sun_year", ".pdf"), device="pdf", width=5.5, height=4.5, units="in")




#####################################################################################
##################### Annual trend of disease, urban, transport, mobility ###################################

load(file = "./codes for figures/Fig3/Annual_data.RData")

Annual_data$year <- as.numeric(Annual_data$year)
Annual_data$city <- factor(Annual_data$city, levels = c("Huzhou", "Jiaxing","Hangzhou", "Jinhua", "Ningbo","Shaoxing","Taizhou","Quzhou","Lishui", "Wenzhou"))

names(Annual_data)

p11 <- ggplot(Annual_data, aes(x = year, y = urban, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) + 
  labs(title = "Annually urbanization levels", x = "Year", y = "Urbanization rate (%)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + # 图例水平排列
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

p11
#ggsave(p11, file=paste0(path,"Fig2_Urban_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")



p12 <- ggplot(Annual_data, aes(x = year, y = transportation, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) + 
  labs(title = "Annually transportation", x = "Year", y = "Transportation lengths (km)") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + # 图例水平排列
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

p12
#ggsave(p12, file=paste0(path,"Fig2_transportation_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")


p13 <- ggplot(Annual_data, aes(x = year, y = out_migration, group = city, color = city)) +
  geom_line(size = 1) +
  scale_color_manual(values = city_colors) +  
  labs(title = "Annually migration index", x = "Year", y = "Migration index") +
  theme_classic() +  # 使用经典主题
  theme(
    text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
    axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
    axis.text = element_text(color = "black"),  # 坐标轴文字颜色
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # 将图例放在底部
    legend.title = element_text(face = "bold"),  # 图例标题使用粗体
    legend.text = element_text(size = 12)  # 图例文字大小
  ) +
  guides(color = guide_legend(ncol = 1, bycol = TRUE)) + # 图例水平排列
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))

p13
#ggsave(p13, file=paste0(path,"Fig2_mobility_year", ".pdf"), device="pdf", width=4, height=4.5, units="in")

# 
# 
# #save(mobility_month, file = "./codes for figures/Fig4/mobility_month.RData")
# load(file = "./codes for figures/Fig4/mobility_month.RData")
# mobility_month$month <- factor(mobility_month$month)
# mobility_month$city <- factor(mobility_month$city, levels = unique(mobility_month$city ))
# 
# p14 <- ggplot(mobility_month, aes(x = month, y = out_migration, group = city, color = city)) +
#   geom_smooth(method = "loess", formula = y ~ x, se = FALSE, size = 0.8) +  # 添加平滑曲线并调整线条粗细
#   scale_color_brewer(palette = "Paired", name = "City") + 
#   labs(title = "Monthly Migration Index", x = "Month", y = "Migration index") +
#   theme_classic() +  # 使用经典主题
#   theme(
#     text = element_text(size = 14, family = "Helvetica"),  # 调整字体大小和字体
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 居中标题，使用粗体
#     axis.title = element_text(face = "bold"),  # 坐标轴标题使用粗体
#     axis.text = element_text(color = "black"),  # 坐标轴文字颜色
#     legend.position = "right",  # 将图例放在底部
#     legend.title = element_text(face = "bold"),  # 图例标题使用粗体
#     legend.text = element_text(size = 12)  # 图例文字大小
#   ) +
#   guides(color = guide_legend(ncol = 1, bycol = TRUE)) + # 图例水平排列
#   scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
# 
# # 打印图形
# print(p14)
#ggsave(p14, file=paste0(path,"Fig2_migrationmonth", ".pdf"), device="pdf", width=5.5, height=4.5, units="in")


pc1 = gridExtra::grid.arrange(p6,p7,p8,p9,
                              p11,p12,p13,p1,
                              nrow=2, widths=c(1,1,1,1.35))

#ggsave(pc1, file="./output/figures/Fig1_combined.pdf", device="pdf", width=17.5, height=7, units="in")

library(cowplot)
#坐标轴对齐
# Adjust the spacing between plots
p <- plot_grid(p6, p7, p8, p9, p11, p12, p13, p1, nrow = 2, ncol = 4, align = 'hv',
               rel_widths = c(2, 2, 2, 2), # Control widths of columns if needed
               rel_heights = c(1, 1),       # Control row heights if needed
               axis = 'tblr'  )           # Align axis titles (top, bottom, left, right)
#labels = c("A", "B", "C", "D", "E", "F", "G", "H"))  # Optional labels for identification

# Save the plot to a file
ggsave(p, file="./codes for figures/new figures 2025.1.27/Fig1_combined2.tif", device="tiff", width=20, height=7, units="in")
ggsave(p, file="./codes for figures/new figures 2025.1.27/Fig1_combined2.pdf", device="pdf", width=20, height=7, units="in")


#####################################################################################

################## disease spatio distribute ######################################

library(dplyr); library(sf);library(readxl);library(raster); library(rgdal);
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
library(RColorBrewer)

# working directory
PATH = "E:/fdu/PhD project/Infectious disease/spatial/part1/R code/SFTS"
setwd(PATH)

# ================= Build SFTS dataset =================

# districts to be excluded (offshore) 排除舟山市
#offshore_areas = c(70154, 70339, 70273, 70355, 70698)

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
print(shp_prov)

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
#Fig2_sfts_data <- shpxx2
#save(Fig2_sfts_data, file = "./codes for figures/Fig2/Fig2_sfts_data.RData")
load(file = "./codes for figures/Fig2/Fig2_sfts_data.RData")

# map figure
# theme for mapping
maptheme = theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=12),
        legend.title = element_text(size=10), 
        strip.background = element_blank())

#colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(60)
#colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "PuRd"))(60)
colScale <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdPu"))(60)


incidence_range <- range(Fig2_sfts_data$incidence, na.rm = TRUE)
#breaks <- exp(seq(log(incidence_range[1]), log(incidence_range[2]), length.out = 6))
#breaks <- c(1,2,6,16,43,114)
#breaks <- c(1,2.5,5,15,50,100)
breaks <- c(1,5,15,50,100)
p1 <- ggplot() + 
  geom_sf(data=Fig2_sfts_data, aes(fill=log(incidence)), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(
    colors = colScale, 
    name="Mean\nincidence\nrate\n(1/10^7)\n ",
    breaks = log(breaks),
    labels = function(x) round(exp(x), 1)
  ) +
  maptheme +
  #facet_wrap(~epoch, nrow=1) + 
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16),
        legend.position=c(0.95,0.4), 
        #legend.position= "right", 
        axis.line = element_line(color="white"),
        axis.text = element_text(size=9, color="white"),
        axis.title = element_text(size=14, color="white")) + 
  xlab("Longitude") + ylab("Latitude")
p1
#ggsave(p1, file="./codes for figures/Fig2/Figure2_sfts.pdf", device="pdf", width=8, height=6, units="in")





#################################################################################
################################ DEM data ###############################################
library(raster)
library(rasterVis)
library(ggplot2)
library(gridExtra)
# 读取栅格数据
file_path = "E:/fdu/PhD project/Infectious disease/spatial/part1/Rawdata/【立方数据学社】浙江省地形/全省范围的数据/浙江省.tif"

terrain <- raster(file_path)

target_crs <- CRS("+init=epsg:4326")
terrain <- projectRaster(terrain, crs = target_crs, over = TRUE) 

p2 <- plot(terrain)

#the saving process failed, need to save manually in Plots panel
#ggsave(p2, file="./output/figures/Figure2_DEM2.pdf", device="tif", width=8, height=6, units="in")


# 计算最小值和最大值
terrain <- setMinMax(terrain)

# 自定义颜色刻度
#display.brewer.all()
colScale =  rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlGn"))(60))

# 自定义主题
maptheme <- theme(
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 16),
  #legend.position = "right",
  legend.position=c(0.9,0.4), 
  axis.line = element_line(color = "white"),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank()
)

# 绘制地形图
p2 <- gplot(terrain) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradientn(
    colors = colScale, 
    na.value = "white", 
    name = "Elevation (m)",
    breaks = c(0,500,1000,1500),
    labels = function(x) round(x, 0)
  ) + 
  coord_equal() +  # 保持比例
  maptheme +       # 应用自定义主题
  xlab("Longitude") + ylab("Latitude")


# 显示地形图
print(p2)
#ggsave(p2, file="./codes for figures/Fig2/Figure2_DEM.pdf", device="pdf", width=8, height=6, units="in")


#################  Corpland, Grassland, Shurb ##############################################
################### land cover data, area of Cropland,Forest,Grassland ##################################

landcover <- read.csv("./output/data_process/landcover.csv",fileEncoding = "GB18030") %>%
  dplyr::filter(!cityid %in% 330900)

names(landcover)

landcover_area <- landcover %>%
  group_by(countyid) %>%
  summarise(Forest = mean(Forest, na.rm = TRUE), 
            Cropland = mean(Cropland, na.rm = TRUE),
            Grassland = mean(Grassland, na.rm = TRUE),
            Shrub = mean(Shrub, na.rm = TRUE),
            Forest_p = mean(Forest_p, na.rm = TRUE), 
            Cropland_p = mean(Cropland_p, na.rm = TRUE),
            Grassland_p = mean(Grassland_p, na.rm = TRUE),
            Shrub_p = mean(Shrub_p, na.rm = TRUE)
  ) %>%
  mutate(countyid = as.numeric(countyid))  

shp = cbind(shp, as.data.frame(st_coordinates(st_centroid(shp))) %>% dplyr::rename("longitude"=1, "latitude"=2)) #提取经纬度
shpx = full_join(shp, landcover_area,  by=c("countyid"))

shpxx = shpx[ !is.na(shpx$Forest), ]


#save data
#Fig2_landcover_data <- shpxx
#save(Fig2_landcover_data, file = "./codes for figures/Fig2/Fig2_landcover_data.RData")
load(file = "./codes for figures/Fig2/Fig2_landcover_data.RData")


# theme for mapping
maptheme = theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=12),
        legend.title = element_text(size=14), 
        strip.background = element_blank())

# map figure

display.brewer.all()
colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))(60)

p3 <- ggplot() + 
  geom_sf(data=Fig2_landcover_data, aes(fill=Cropland), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(colors = colScale, 
                       name=expression(paste("Cropland ",(km^2)))
  ) +  
  maptheme +
  #facet_wrap(~epoch, nrow=1) + 
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16),
        legend.position=c(0.95,0.4), 
        #legend.position="right", 
        axis.line = element_line(color="white"),
        axis.text = element_text(size=9, color="white"),
        axis.title = element_text(size=14, color="white")) + 
  xlab("Longitude") + ylab("Latitude")
p3
ggsave(p3, file="./codes for figures/Fig2/Figure2_Cropland.pdf", device="pdf", width=8, height=6, units="in")

##################################################################################

colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(60)

p4 <- ggplot() + 
  geom_sf(data=Fig2_landcover_data, aes(fill=Shrub), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(colors = colScale, 
                       na.value = "grey90", 
                       name=expression(paste("Shrub ",(km^2)))
  ) +  
  maptheme +
  #facet_wrap(~epoch, nrow=1) + 
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16),
        #legend.position="right", 
        legend.position=c(0.95,0.4), 
        axis.line = element_line(color="white"),
        axis.text = element_text(size=9, color="white"),
        axis.title = element_text(size=14, color="white")) + 
  xlab("Longitude") + ylab("Latitude")
p4

ggsave(p4, file="./codes for figures/Fig2/Figure2_Shrub.pdf", device="pdf", width=8, height=6, units="in")


##################################################################################
#display.brewer.all()
colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn"))(60)

p5 <- ggplot() + 
  geom_sf(data=Fig2_landcover_data, aes(fill=Grassland), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(colors = colScale, 
                       na.value = "grey90", 
                       name=expression(paste("Grassland ",(km^2)))
  ) +  
  maptheme +
  #facet_wrap(~epoch, nrow=1) + 
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16),
        #legend.position="right", 
        legend.position=c(0.95,0.4), 
        axis.line = element_line(color="white"),
        axis.text = element_text(size=9, color="white"),
        axis.title = element_text(size=14, color="white")) + 
  xlab("Longitude") + ylab("Latitude")
p5

ggsave(p5, file="./codes for figures/Fig2/Figure2_Grassland.pdf", device="pdf", width=8, height=6, units="in")



