library(ggplot2)
library(dplyr); library(sf);library(readxl);library(raster); library(rgdal);
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
library(RColorBrewer);library(tidyr)

# working directory
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)

#load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028.RData")

load("./output/model_outputs/Temperature Projection/Temp_proj_model_result_train2023_val2028new(allcounties).RData")

head(Temp_proj_fitted_result)

dd_area <- Temp_proj_fitted_result %>%
  #filter(year >2023) %>%
  group_by(year,countyid, county,city,cityid)%>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            fitted.values = sum(fitted.values, na.rm = TRUE),
            fitted.values_lower = sum(fitted.values_lower, na.rm = TRUE),
            fitted.values_upper = sum(fitted.values_upper, na.rm = TRUE)
  ) %>%
  mutate(year = as.integer(year))


dd_area$total_cases_copy = dd_area$total_cases

area_data <- dd_area %>%
  mutate(total_cases = ifelse(year <= 2023, total_cases, fitted.values))

head(area_data)


# 计算每年total_cases > 1的县数量
county_num <- area_data %>%
  filter(total_cases >= 1) %>%  # 筛选出total_cases大于1的记录
  group_by(year) %>%           # 按年分组
  summarise(counties_above_1 = n_distinct(countyid))  # 计算每年total_cases>1的县数量

# 绘制条形图

ggplot(county_num, aes(x = year, y = counties_above_1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = counties_above_1), vjust = -0.5, size = 4) +  # 在条形图上标注数值
  theme_minimal() +
  labs(title = "Number of Counties with SFTS Total Cases > 1 per Year",
       x = "Year", 
       y = "Counties with Total Cases > 1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p2 <- ggplot(county_num, aes(x = year, y = counties_above_1)) +
  geom_bar(stat = "identity", fill = "#4C9F70", color = "white", size = 0.7) +  # 使用柔和的渐变色
  geom_text(aes(label = counties_above_1), vjust = -0.5, size = 5, fontface = "bold", color = "black") +  # 标注数值调整
  theme_classic(base_size = 14) +  # 使用简洁风格并调整基本字体大小
  labs(
    title = "Number of Counties with SFTS Cases Exceeding One",  # 更简洁明了的标题
    subtitle = "Observed data from 2011 to 2023, predicted data from 2024 to 2028",  # 可以添加副标题来提供更多信息
    x = "Year", 
    y = "Counties with SFTS Cases > 1"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # x轴标签角度和字体调整
    axis.text.y = element_text(size = 12),  # y轴标签字体调整
    axis.title.x = element_text(size = 14, face = "bold"),  # x轴标题字体调整
    axis.title.y = element_text(size = 14, face = "bold"),  # y轴标题字体调整
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题字体和居中
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # 副标题字体居中
    plot.margin = margin(10, 20, 10, 10)  # 设置图表四周的边距
  ) +
  scale_x_continuous(breaks = 2011:2028, expand = c(0.01, 0.01))  # 设置x轴的breaks并调整扩展
p2
#ggsave(p2, file="./output/figures/Figure9_spatial_spread.pdf", device="pdf", units="in", width=9, height=8)






city_names <- c(
  "杭州市" = "Hangzhou", 
  "宁波市" = "Ningbo", 
  "温州市" = "Wenzhou", 
  "嘉兴市" = "Jiaxing", 
  "湖州市" = "Huzhou", 
  "绍兴市" = "Shaoxing", 
  "金华市" = "Jinhua", 
  "衢州市" = "Quzhou", 
  "舟山市" = "Zhoushan", 
  "台州市" = "Taizhou", 
  "丽水市" = "Lishui"
)

# 替换area_data中的城市中文名称为拼音英文
area_data$city <- recode(area_data$city, !!!city_names)


# 1. 生成变量 'ecoh'，根据年份划分为三个时间段
area_data <- area_data %>%
  mutate(ecoh = case_when(
    year >= 2011 & year <= 2016 ~ "2011-2016",
    year >= 2017 & year <= 2022 ~ "2017-2022",
    year >= 2023 & year <= 2028 ~ "2023-2028"
  ))


area_data <- area_data %>%
  mutate(ecoh = year)



area_data <- area_data %>%
  mutate(ecoh = case_when(
    year >= 2011 & year <= 2013 ~ "2011-2013",
    year >= 2014 & year <= 2016 ~ "2014-2016",
    year >= 2017 & year <= 2019 ~ "2017-2019",
    year >= 2020 & year <= 2022 ~ "2020-2022",
    year >= 2023 & year <= 2025 ~ "2023-2025",
    year >= 2026 & year <= 2028 ~ "2026-2028",
  ))




# 2. group by ecoh，筛选出每个时间段病例数最多的前10个county
top_counties <- area_data %>%
  group_by(ecoh,county,city) %>%
  summarise(total_cases=sum(total_cases))%>%
  ungroup()

top_counties <- top_counties %>%
  group_by(ecoh) %>%
  arrange(desc(total_cases)) %>%
  slice_head(n = 10) %>%
  ungroup()


# 3. 查看病例数最多的前10个county分别属于什么city，并计算占比
# 假设'city'列在你的数据中包含了城市信息
top_counties_city <- top_counties %>%
  group_by(ecoh, city) %>%
  summarise(total_cases = sum(total_cases), .groups = "drop") %>%
  group_by(ecoh) %>%
  mutate(percentage = total_cases / sum(total_cases) * 100) %>%
  ungroup()


# 4. 绘制堆叠百分比柱状图

library(RColorBrewer)

# 定义城市分组
city_groups <- list(
  group1 = c("Huzhou", "Jiaxing", "Hangzhou"),  # 第一类城市
  group2 = c("Jinhua", "Ningbo", "Shaoxing", "Taizhou"),  # 第二类城市
  group3 = c("Quzhou", "Lishui", "Wenzhou")  # 第三类城市
)

# 为每个城市分配颜色
city_colors <- c(
  setNames(scales::alpha(RColorBrewer::brewer.pal(3, "Blues"), 0.8), city_groups$group1),
  setNames(scales::alpha(RColorBrewer::brewer.pal(4, "Reds"), 0.8), city_groups$group2),
  setNames(scales::alpha(RColorBrewer::brewer.pal(3, "Purples"), 0.8), city_groups$group3)
)
unique(top_counties_city$city)
top_counties_city$city <- factor(top_counties_city$city, levels = c("Huzhou", "Hangzhou","Quzhou", "Jinhua", "Ningbo","Shaoxing","Taizhou"))

# 绘制堆叠百分比柱状图，使用自定义的颜色
p3 <- ggplot(top_counties_city, aes(x = ecoh, y = percentage, fill = city)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # 调整条形图的宽度
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # 显示百分比
  scale_fill_manual(values = city_colors) +  # 使用自定义颜色
  labs(title = "Proportion of Top 10 Counties with Highest SFTS Cases",
       x = "Time Period", y = "Percentage",
       fill = "City") +
  theme_minimal(base_size = 16) +  # 调整基本字体大小
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # 调整x轴标签角度和大小
    axis.text.y = element_text(size = 12),  # 调整y轴字体
    axis.title.x = element_text(size = 14, face = "bold"),  # x轴标题
    axis.title.y = element_text(size = 14, face = "bold"),  # y轴标题
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # 标题加粗居中
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # 副标题
    plot.margin = margin(10, 20, 10, 10)  # 增加图表边距
  )+
  # 添加注释
  annotate("text", x = 2.5, y = -1, label = "2011-2023: Observed data, 2024-2028: Predicted data", 
           size = 2.5, hjust = 0, vjust = 1, fontface = "italic", color = "black")
p3
ggsave(p3, file="./output/figures/Figure9_spatial_proportion.pdf", device="pdf", units="in", width=8, height=4)

