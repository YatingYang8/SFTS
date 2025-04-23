library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(car)
library(reshape2)

dd = read.csv("./output/data_process/ModelData_sfts_excluZS_withLags.csv",fileEncoding = "GB18030")

ddf <- dd

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
    Water,Barren,Impervious,Shrub,Cropland_p,Forest_p,Grassland_p,Water_p,Barren_p,Impervious_p,
    Shrub_p,
    elevation,
    in_migration,
    out_migration,
    out_migration_norm,
    out_migration_norm2,
    Tmean_1m_g,
    Precipitation_2m_g,
    Rh_06m,
    Rh_3m_g,
    Rh_6m_g,
    Sun_01m
  ) %>%
  dplyr::mutate(
    urban_log = log(urban + 1),
    transportation_log = log(transportation + 1),
    out_migration_g = inla.group(out_migration_norm2, n=n_clim_bins),
    Cropland_log = log(Cropland + 1),
    Forest_log = log(Forest + 1),
    Grassland_g = inla.group(Grassland, n=n_clim_bins),
    Barren_g = inla.group(Barren, n=n_clim_bins),
    Shrub_g = inla.group(Shrub, n=n_clim_bins),
    elevation_log = log(elevation + 1),
    urban_g = inla.group(urban, n=n_clim_bins),
    transportation_g = inla.group(transportation, n=n_clim_bins),
    Cropland_g = inla.group(Cropland, n=n_clim_bins),
    Forest_g = inla.group(Forest, n=n_clim_bins),
    elevation_g = inla.group(elevation, n=n_clim_bins),
    Rh_06m_g = inla.group(Rh_06m, n=n_clim_bins),
    Sun_01m_g = inla.group(Sun_01m, n=n_clim_bins),
    Water_log = log(Water + 1),
    Barren_log = log(Barren + 1),
    Impervious_log = log(Impervious + 1),
    Shrub_log = log(Shrub + 1),
    Cropland_p_log = log(Cropland_p + 1),
    Forest_p_log = log(Forest_p + 1),
    Grassland_p_log = log(Grassland_p + 1),
    Water_p_log = log(Water_p + 1),
    Barren_p_log = log(Barren_p + 1),
    Impervious_p_log = log(Impervious_p + 1),
    Shrub_p_log = log(Shrub_p + 1)
  ) 

# 计算相关矩阵
cor_matrix <- cor(ddf[, c("elevation_g","Cropland_log","Grassland_g","Shrub_g","Forest_g",
                          "Tmean_g", "Rh_3m_g","Precipitation_2m_log", "Sun_01m_g", 
                          "urban_log", "transportation_log", "out_migration_g"
)], use = "pairwise.complete.obs")



# 打印相关矩阵
print(cor_matrix)

# 自定义变量名称
colnames(cor_matrix) <- c("Elevation","Cropland","Grassland","Shrub","Forest",
                          "Mean temperature", "Relative humidity","Precipitation","Sun duration",
                          "Urbanization", "Transportation", "Migration index"
)
rownames(cor_matrix) <- colnames(cor_matrix)

# 使用corrplot包绘制相关矩阵热图
#palette <- colorRampPalette(brewer.pal(11, "RdYlBu"))(200)
palette <- colorRampPalette(brewer.pal(9, "PuOr"))(200)

# 绘制相关矩阵热图
pdf(file = "./output/figures/FigureS4_Correlation_Matrix.pdf", width = 8, height = 8)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = palette,
         title = "Correlation Matrix of Variables",
         mar = c(0, 0, 1, 0))
dev.off()
#ggsave(p1, file="./output/figures/FigureS4_Correlation Matrix.pdf", device="pdf", width=6.8, height=5, units="in")


# 使用GGally包中的ggpairs函数绘制散点图矩阵
ddf_renamed <- ddf[, c("elevation_g","Cropland_log","Grassland_g","Shrub_g","Forest_g",
                       "Tmean_g", "Rh_3m_g","Precipitation_2m_log", "Sun_01m_g", 
                       "urban_log", "transportation_log", "out_migration_g")]
colnames(ddf_renamed) <- c("Elevation","Cropland","Grassland","Shrub","Forest",
                           "Mean temperature", "Relative humidity","Precipitation","Sun duration",
                           "Urbanization", "Transportation", "Migration index")
ggpairs(ddf_renamed, 
        title = "Scatterplot Matrix of Variables",
        upper = list(continuous = wrap("cor", size = 4, alignPercent = 0.8)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 0.7)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.6)),
        axisLabels = 'show') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"))
ggsave(file="./output/figures/FigureS4_corr.pdf", device="pdf", width=13, height=13, units="in")



#VIF
###All variables
ddf_vif <- ddf[, c("total_cases","elevation_g","Cropland_log","Grassland_g","Shrub_g","Forest_g",
                   "Tmean_g", "Rh_3m_g","Precipitation_2m_log", "Sun_01m_g", 
                   "urban_log", "transportation_log", "out_migration_g")]

# 拟合线性模型以计算VIF
model <- lm(total_cases ~ ., data = ddf_vif)
vif_values <- vif(model)
print(vif_values)

vif_df <- data.frame( Variable = names(vif_values), 
                      Variable_name = c("Elevation","Cropland (log)","Grassland","Shrub","Forest",
                                        "Mean temperature", "Relative humidity","Precipitation (log)","Sun duration",
                                        "Urbanization (log)", "Transportation (log)", "Migration index"),
                      VIF = vif_values )

# 使用ggplot2绘制条形图
p1 <- ggplot(vif_df, aes(x = reorder(Variable_name, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(VIF, 2)), hjust = -0.3, size = 5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variance Inflation Factors (VIF)",
       x = "Variables",
       y = "VIF") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))
p1
ggsave(p1,file="./output/figures/FigureS4_vif_before excluded.pdf", device="pdf", width=6, height=4, units="in")



### Excluded Forest
ddf_vif <- ddf[, c("total_cases","elevation_g","Cropland_log","Grassland_g","Shrub_g",
                   "Tmean_g", "Rh_3m_g","Precipitation_2m_log", "Sun_01m_g", 
                   "urban_log", "transportation_log", "out_migration_g")]

# 拟合线性模型以计算VIF
model <- lm(total_cases ~ ., data = ddf_vif)
vif_values <- vif(model)
print(vif_values)

vif_df <- data.frame( Variable = names(vif_values), 
                      Variable_name = c("Elevation","Cropland (log)","Grassland","Shrub",
                                        "Mean temperature", "Relative humidity","Precipitation (log)","Sun duration",
                                        "Urbanization (log)", "Transportation (log)", "Migration index"),
                      VIF = vif_values )

# 使用ggplot2绘制条形图
p2 <- ggplot(vif_df, aes(x = reorder(Variable_name, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(VIF, 2)), hjust = -0.3, size = 5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variance Inflation Factors (VIF)",
       x = "Variables",
       y = "VIF") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))
p2
ggsave(p2,file="./output/figures/FigureS4_vif_after excluded.pdf", device="pdf", width=6, height=4, units="in")
