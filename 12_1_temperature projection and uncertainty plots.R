library(ggplot2)
library(dplyr); library(sf);library(readxl);library(raster); library(rgdal);
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
library(RColorBrewer)

# working directory
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)


############# Panel A. Predicted Trends using various training dataset ###################

load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028.RData")

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

Uncertainty_df_yr <- dd_yr %>%
  filter(year %in% c(2024,2028)) %>%
  select(year, fitted.incidence, fitted.incidence_lower, fitted.incidence_upper) %>%
  mutate(train_period = "2011-2023")
  

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


load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2022_val2028.RData")

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

Uncertainty_df_yr2 <- dd_yr %>%
  filter(year %in% c(2024,2028)) %>%
  select(year, fitted.incidence, fitted.incidence_lower, fitted.incidence_upper) %>%
  mutate(train_period = "2011-2022")

Uncertainty_df_yr <- rbind(Uncertainty_df_yr,Uncertainty_df_yr2)


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


load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2021_val2028.RData")

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

Uncertainty_df_yr2 <- dd_yr %>%
  filter(year %in% c(2024,2028)) %>%
  select(year, fitted.incidence, fitted.incidence_lower, fitted.incidence_upper) %>%
  mutate(train_period = "2011-2021")

Uncertainty_df_yr <- rbind(Uncertainty_df_yr,Uncertainty_df_yr2)

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


load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2020_val2028.RData")

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

Uncertainty_df_yr2 <- dd_yr %>%
  filter(year %in% c(2024,2028)) %>%
  select(year, fitted.incidence, fitted.incidence_lower, fitted.incidence_upper) %>%
  mutate(train_period = "2011-2020")

Uncertainty_df_yr <- rbind(Uncertainty_df_yr,Uncertainty_df_yr2)


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



library(gridExtra)

pA <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#ggsave(pA, file="./output/figures/Figure7_predictTrends.pdf", device="pdf", width=13, height=9, units="in")



################## Panel B. Uncertainty of predictions for 2024 and 2028 #############

head(Uncertainty_df_yr)

pB <- ggplot(Uncertainty_df_yr, aes(x = train_period, y = fitted.incidence, 
                              ymin = fitted.incidence_lower, ymax = fitted.incidence_upper,
                              color = as.factor(year), fill = as.factor(year))) +
  geom_pointrange(position = position_dodge(width = 0.4), # 为了避免点重叠，稍微调整位置
                  size = 1, shape = 21,  stroke = 0.5) +  # 绘制带区间的点
  scale_color_manual(values = c("2024" = "dodgerblue", "2028" = "#ff6283")) + # 2024为蓝色，2028为橙色
  scale_fill_manual(values = c("2024" = "#8cd3f7", "2028" = "#eb8484")) + # 区域填充色
  labs(title = "Predicted Incidence for 2024 and 2028 \nwith Different Training Periods",
       x = "Training Period",
       y = expression(paste("Predicted Incidence (", 1/10^7, ")")),
       color = "Year", fill = "Year") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # 使x轴标签倾斜，便于显示
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey90"), # 添加横网格线
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "top",
        legend.text = element_text(size = 11), # 增加图例文本字体大小
        legend.title = element_text(size = 11)) +
  guides(color = guide_legend(title = "Year"), fill = guide_legend(title = "Year")) # 添加图例标题

pB
#ggsave(pB, file="./output/figures/Figure7_uncertainty_yr.pdf", device="pdf", width=12, height=5, units="in")

ggsave(pB, file="./codes for figures/new figures 2025.1.27/Figure5_uncertainty_yr.pdf", device="pdf", width=5, height=5, units="in")
ggsave(pB, file="./codes for figures/new figures 2025.1.27/Figure5_uncertainty_yr.tif", device="tiff", width=5, height=5, units="in")



#################### Monthly prediction #########################################

load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028.RData")

dd_yearmonth <- Temp_proj_fitted_result %>%
  #filter(year >2023) %>%
  group_by(year,month)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(paste0(year,"-",month, "-01")) )

dd_yearmonth <- left_join(dd_yearmonth, pop_yr)


dd_yearmonth <- dd_yearmonth %>%
  mutate(population = ifelse(year <= 2023, population, 6627))


dd_yearmonth_2023 <- dd_yearmonth %>%
  filter(year %in% c(2024,2028)) %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))


###################################
load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2022_val2028.RData")

dd_yearmonth <- Temp_proj_fitted_result %>%
  #filter(year >2023) %>%
  group_by(year,month)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(paste0(year,"-",month, "-01")) )

dd_yearmonth <- left_join(dd_yearmonth, pop_yr)


dd_yearmonth <- dd_yearmonth %>%
  mutate(population = ifelse(year <= 2023, population, 6627))


dd_yearmonth_2022 <- dd_yearmonth %>%
  filter(year %in% c(2024,2028)) %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))


#########################
load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2021_val2028.RData")

dd_yearmonth <- Temp_proj_fitted_result %>%
  #filter(year >2023) %>%
  group_by(year,month)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(paste0(year,"-",month, "-01")) )

dd_yearmonth <- left_join(dd_yearmonth, pop_yr)


dd_yearmonth <- dd_yearmonth %>%
  mutate(population = ifelse(year <= 2023, population, 6627))


dd_yearmonth_2021 <- dd_yearmonth %>%
  filter(year %in% c(2024,2028)) %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))


################################
load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2020_val2028.RData")

dd_yearmonth <- Temp_proj_fitted_result %>%
  #filter(year >2023) %>%
  group_by(year,month)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(paste0(year,"-",month, "-01")) )

dd_yearmonth <- left_join(dd_yearmonth, pop_yr)


dd_yearmonth <- dd_yearmonth %>%
  mutate(population = ifelse(year <= 2023, population, 6627))


dd_yearmonth_2020 <- dd_yearmonth %>%
  filter(year %in% c(2024,2028)) %>%
  mutate(incidence = (total_cases/population) *1000, 
         fitted.incidence = (fitted.values/population) *1000, 
         fitted.incidence_lower = (fitted.values_lower/population) *1000, 
         fitted.incidence_upper = (fitted.values_upper/population) *1000
  ) %>%
  mutate(year = as.integer(year),
         date = as.Date(date))



# 合并四个训练集的预测数据
dd_yearmonth_all <- bind_rows(
  dd_yearmonth_2020 %>% mutate(train_period = "2011-2020"),
  dd_yearmonth_2021 %>% mutate(train_period = "2011-2021"),
  dd_yearmonth_2022 %>% mutate(train_period = "2011-2022"),
  dd_yearmonth_2023 %>% mutate(train_period = "2011-2023")
) 

dd_yearmonth_all <- dd_yearmonth_all %>%
  select(year,month,date,train_period,fitted.incidence,fitted.incidence_lower,fitted.incidence_upper )
  
# 检查数据框
head(dd_yearmonth_all)


df1 <- dd_yearmonth_all %>%
  filter(year==2024)

# 设置统一的纵坐标刻度,更好地对比
y_scale <- scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10))


p5 <- ggplot(df1, aes(x = factor(month))) +
  
  # 绘制每个月的预测发病率条形图
  geom_bar(aes(y = fitted.incidence, fill = train_period), stat = "identity", position = "dodge", width = 0.7) +
  
  # 绘制误差条，表示置信区间
  geom_errorbar(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, color = train_period), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  
  # 自定义颜色
  scale_fill_manual(values = c(
    "2011-2020" = "#c4f4cd", 
    "2011-2021" = "#8cd3f7", 
    "2011-2022" =  "#1aa7ee", 
    "2011-2023" = "#4328e7")) +
  scale_color_manual(values = c(
    "2011-2020" = "#c4f4cd", 
    "2011-2021" = "#8cd3f7", 
    "2011-2022" =  "#1aa7ee", 
    "2011-2023" = "#4328e7")) +
  
  # 图例标签
  labs(title = "Monthly Predicted Incidence for 2024",
       x = "Month",
       y = expression(paste("Predicted Incidence (", 1/10^7, ")")),
       color = "Training Period", fill = "Training Period") +
  
  # 主题设置
  theme_classic() +
  theme(
    legend.position = c(0.2, 0.7),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # 月份格式
  scale_x_discrete(labels = month.abb) +  # 设置x轴为月份缩写（Jan, Feb, Mar 等）
  
  # 调整图例
  guides(color = guide_legend(ncol = 1, bycol = TRUE)) +
  y_scale  # 统一纵坐标刻度

# 展示图形
p5


df2 <- dd_yearmonth_all %>%
  filter(year==2028)


p6 <- ggplot(df2, aes(x = factor(month))) +
  
  # 绘制每个月的预测发病率条形图
  geom_bar(aes(y = fitted.incidence, fill = train_period), stat = "identity", position = "dodge", width = 0.7) +
  
  # 绘制误差条，表示置信区间
  geom_errorbar(aes(ymin = fitted.incidence_lower, ymax = fitted.incidence_upper, color = train_period), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  
  # 自定义颜色
  scale_fill_manual(values = c(
    "2011-2020" = "#fddaec", 
    "2011-2021" = "#ffb1c1", 
    "2011-2022" =  "#fbb4ae", 
    "2011-2023" = "#eb8484")) +
  scale_color_manual(values = c(
    "2011-2020" = "#fddaec", 
    "2011-2021" = "#ffb1c1", 
    "2011-2022" =  "#fbb4ae", 
    "2011-2023" = "#eb8484")) +

  # 图例标签
  labs(title = "Monthly Predicted Incidence for 2028",
       x = "Month",
       y = expression(paste("Predicted Incidence (", 1/10^7, ")")),
       color = "Training Period", fill = "Training Period") +
  
  # 主题设置
  theme_classic() +
  theme(
    legend.position = c(0.2, 0.7),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # 月份格式
  scale_x_discrete(labels = month.abb) +  # 设置x轴为月份缩写（Jan, Feb, Mar 等）
  
  # 调整图例
  guides(color = guide_legend(ncol = 1, bycol = TRUE)) +
  y_scale  # 统一纵坐标刻度

# 展示图形
p6


pC <- grid.arrange(p5, p6, ncol = 2, widths = c(2, 2))

pp <- grid.arrange(pB, pC, ncol = 1, heights = c(0.55, 0.45))

#ggsave(pC, file="./output/figures/Figure7_uncertainty_month.pdf", device="pdf", width=12, height=4.2, units="in")
#ggsave(pp, file="./output/figures/Figure7_uncertainty_yr&month.pdf", device="pdf", width=12, height=8.4, units="in")

ggsave(pC, file="./codes for figures/new figures 2025.1.27/Figure5_uncertainty_month.pdf", device="pdf", width=12, height=4.2, units="in")


################# Spatial projecting maps ##########################################


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



##################### map incidence in future 5 year ################################
load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028new(allcounties).RData")

dd_grp <- Temp_proj_fitted_result %>%
  filter(year > 2022) %>%
  group_by(year,countyid)%>%
  summarise(total_cases =sum(fitted.values, na.rm = TRUE),
            pop = exp(logpop)*10000) %>%
  mutate(
    countyid = as.numeric(countyid),
    year = as.integer(year),
    incidence = (total_cases/pop)*1000) %>% distinct()

# mean annual incidence across years
shp = cbind(shp, as.data.frame(st_coordinates(st_centroid(shp))) %>% dplyr::rename("longitude"=1, "latitude"=2)) #提取经纬度
shpt <- left_join(dd_grp, shp,  by="countyid")

shpt <- shpt[order(shpt$city, shpt$year), ]

shptt = shpt %>%
  dplyr::filter(!is.na(incidence)) %>%
  dplyr::group_by(countyid, year) %>%
  dplyr::summarise(incidence = mean(incidence, na.rm=TRUE))

shptt2 <- left_join(shp, shptt,  by="countyid")
shptt2 = shptt2[ !is.na(shptt2$incidence), ]


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


incidence_range <- range(shptt2$incidence, na.rm = TRUE)
breaks <- exp(seq(log(incidence_range[1]), log(incidence_range[2]), length.out = 5))
breaks <- c(1,5,25)
p <- ggplot() + 
  geom_sf(data=shptt2, aes(fill=log(incidence)), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(
    colors = colScale, 
    na.value = "white", 
    name="Mean\nincidence\nrate\n(1/10000)",
    breaks = log(breaks),
    labels = function(x) round(exp(x), 1)
  ) +
  maptheme +
  facet_wrap(~year, nrow=2) + 
  theme(
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    strip.text = element_text(size=16),
    legend.position="right", 
    axis.line = element_line(color="white"),
    axis.text = element_text(size=9, color="white"),
    axis.title = element_text(size=14, color="white")
  ) + 
  xlab("Longitude") + ylab("Latitude")

p
#ggsave(p, file="./output/figures/Figure6_project_Trends_2024-2028_eachyr_ssp119.pdf", device="pdf", width=17, height=7.5, units="in")




#1 year: 2024

shptx1 <- shptt2 %>%
  filter(year == 2024) %>%
  group_by(countyid) %>%
  summarise( incidence = sum(incidence)) %>%
  mutate(lab = "Projections for the next 1 year (2024)")

shptx2 <- shptt2 %>%
  filter(year %in% 2024:2026) %>%
  group_by(countyid) %>%
  summarise( incidence = sum(incidence)) %>%
  mutate(lab = "Projections for the next 3 year (2024-2026)")

shptx3 <- shptt2 %>%
  filter(year %in% 2024:2028) %>%
  group_by(countyid) %>%
  summarise( incidence = sum(incidence)) %>%
  mutate(lab = "Projections for the next 5 year (2024-2028)")


load(file = "./codes for figures/Fig1/Fig1c_data.RData")
shptx0 <- Fig1c_data %>%
  filter(epoch == 2023) %>%
  group_by(countyid) %>%
  summarise( incidence = sum(incidence)) %>%
  mutate(lab = "Incidence for 2023")


shptx <- rbind(shptx0,shptx1,shptx2,shptx3)


incidence_range <- range(shptx$incidence, na.rm = TRUE)
breaks <- exp(seq(log(incidence_range[1]), log(incidence_range[2]), length.out = 6))  
breaks <- c(0.1,1,5,20,120)
p1 <- ggplot() + 
  geom_sf(data=shptx, aes(fill=log(incidence)), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(
    colors = colScale, 
    name="Mean\nincidence\nrate\n(1/10000000)",
    breaks = log(breaks),
    labels = function(x) round(exp(x), 1)
  ) +
  maptheme +
  facet_wrap(~lab, nrow=1) + 
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=13),
        legend.position="right", 
        axis.line = element_line(color="white"),
        axis.text = element_text(size=9, color="white"),
        axis.title = element_text(size=14, color="white"),
        plot.title = element_text(size=18)) + 
  #labs(title = "Projections for the next one (2024), three (2024-2026) and five (2024-2028) year ") +
  xlab("Longitude") + ylab("Latitude")
p1

#ggsave(p1, file="./output/figures/Figure6_project_Trends_2024-2028_future5yr.pdf", device="pdf", width=17, height=5, units="in")




##################################################################################

shpt2028 <- shptt2 %>%
  filter(year==2028)

incidence_range <- range(shpt2028$incidence, na.rm = TRUE)
breaks <- exp(seq(log(incidence_range[1]), log(incidence_range[2]), length.out = 5))
breaks <- c(0.03532274,  0.2 , 1.00815078,  5,  25)
#breaks <- c(1,5,25)
p <- ggplot() + 
  geom_sf(data=shpt2028, aes(fill=log(incidence)), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(
    colors = colScale, 
    na.value = "white", 
    name="Mean\nincidence\nrate\n(1/10000000)",
    breaks = log(breaks),
    labels = function(x) round(exp(x), 1)
  ) +
  labs(title = "Projected diffusion for the future 5 years (2028)") +
  maptheme +
  theme(
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    strip.text = element_text(size=16),
    legend.position="right", 
    axis.line = element_line(color="white"),
    axis.text = element_text(size=9, color="white"),
    axis.title = element_text(size=14, color="white"),
    
  ) + 
  xlab("Longitude") + ylab("Latitude")

p



##################### Uncertainty map ##########################################

load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028new(allcounties).RData")

sd_grp <- Temp_proj_fitted_result %>%
  filter(year > 2022) %>%
  group_by(year,countyid)%>%
  summarise(sd =mean(sd, na.rm = TRUE),
            pop = exp(logpop)*10000) %>%
  mutate(
    countyid = as.numeric(countyid),
    year = as.integer(year),
    sd_norm = (sd/pop)*1000) %>% distinct()

# mean annual incidence across years
#shp = cbind(shp, as.data.frame(st_coordinates(st_centroid(shp))) %>% dplyr::rename("longitude"=1, "latitude"=2)) #提取经纬度
shpsd <- left_join(sd_grp, shp,  by="countyid")

shpsd <- shpsd[order(shpsd$city, shpsd$year), ]

shpsd2 = shpsd %>%
  dplyr::filter(!is.na(sd)) %>%
  dplyr::group_by(countyid, year) %>%
  dplyr::summarise(sd =mean(sd, na.rm = TRUE),
                   sd_norm = mean(sd_norm, na.rm=TRUE))

shpsd2 <- left_join(shp, shpsd,  by="countyid")
shpsd2 = shpsd2[ !is.na(shpsd2$sd), ]


# theme for mapping
maptheme = theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=12),
        legend.title = element_text(size=10), 
        strip.background = element_blank())


shpsd2028 <- shpsd2 %>% filter(year==2028)


colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(60)
colScale = colorRampPalette(RColorBrewer::brewer.pal(9, "PuRd"))(60)
colScale <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(60)

display.brewer.all()

p <- ggplot() + 
  geom_sf(data=shpsd2028, aes(fill=log(1/sd)), col=NA) + 
  geom_sf(data=shp_city, fill=NA, col="grey70", alpha=0.2, size=0.2) + 
  geom_sf(data = shp_prov, fill=NA, col="grey20", size=0.2) +
  scale_fill_gradientn(
    colors = colScale, 
    na.value = "white", 
    name="Certainty\n[log(1/sd)]"
    #breaks = log(1/breaks),
    #labels = function(x) round(exp(x), 1)
  ) +
  labs(title = "Uncertainty of Projections for the next 5 years (2028)") +
  maptheme +
  theme(
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    strip.text = element_text(size=16),
    legend.position="right", 
    axis.line = element_line(color="white"),
    axis.text = element_text(size=9, color="white"),
    axis.title = element_text(size=14, color="white"),
    
  ) + 
  xlab("Longitude") + ylab("Latitude")

p


