PATH = dirname(rstudioapi::getSourceEditorContext()$path)  #加一个dirname可以返回再上一级目录
setwd(PATH)

# ================== Measuring predictive influence of covariates using block cross-validation ===================

# Models were fitted using 5-fold cross validation under 3 block designs.
# Cross validation scripts are stored in the "scripts/modelling" folder and were designed to run on an HPC cluster.
# These scripts output a final file for each block design called "models_completed", which contains summary predictive statistics (MAE/RMSE)
# The first section of this script reads these to produce visualisations.
# The later sections of the scripts read in the full model outputs to calculate more granular summaries;
# those outputs are saved in the folder "output/model_outputs/regional_OOS".

library(dplyr)
library(sf)
library(ggplot2)
library(vroom)
library(raster)

source("00_plot_themes.R")
source("00_inla_setup_functions_r4.R")


# ------------- MS Figure: visualise overall error metrics --------------

# final output files from cross-validation 
r1 = read.csv("./output/model_outputs/spOOS_nb2/models_completed.csv") %>% 
  dplyr::mutate(holdout_type ="Spatial") %>% 
  dplyr::filter(!duplicated(model_identifier)) %>%
  dplyr::filter(unique_id != "dummy")

r2 = read.csv("./output/model_outputs/stempOOS_nb2/models_completed.csv") %>%
  dplyr::mutate(holdout_type = "Spatiotemporal") %>%
  dplyr::filter(!duplicated(model_identifier)) %>%
  dplyr::filter(unique_id != "dummy") 
  

r3 = read.csv("./output/model_outputs/tempOOS_nb1/models_completed.csv") %>%
  dplyr::mutate(holdout_type = "Seasonal") %>%
  dplyr::filter(!duplicated(model_identifier)) %>%
  dplyr::filter(unique_id != "dummy") 

# combine
oos_df = do.call(rbind.data.frame, list(r2)) %>%
  dplyr::mutate(mae_oos = as.numeric(mae_oos),
                rmse_oos = as.numeric(rmse_oos))

# oos_df = do.call(rbind.data.frame, list(r1, r2, r3)) %>%
#   dplyr::mutate(mae_oos = as.numeric(mae_oos),
#                 rmse_oos = as.numeric(rmse_oos))

unique(oos_df$candidate)

oos_df = oos_df %>%
  dplyr::left_join(
    data.frame(
      candidate = c("baseline", "Migration","urban", "transportation", "elevation", "Cropland","Grassland", "Shrub", "Tmean", "Precipitation",
                    "Rh", "Sun", "Social factors","Geographical factors","Climate factors", "full"),
      model2 = c("Baseline (all)","Migration index", "Urbanization", "Transportation","Elevation", "Cropland", "Grassland", "Shrub",  
                 "Mean Temperature", "Precipitation (lag02)", "Relative humidity (lag3)", "Sun duration (lag01)",  
                 "Social factors","Geographical factors","Climate factors", "Full model"),
      vartype = c("Baseline (all)", "Social", "Social","Social", "Geographical", "Geographical","Geographical","Geographical", "Climate", "Climate",
                  "Climate", "Climate", "Social","Geographical", "Climate","Full") )
      
  )

# calculate difference from baseline
oos_df = oos_df %>%
  dplyr::group_by(unique_id) %>%
  dplyr::mutate(delta_mae = mae_oos - mae_oos[ candidate == "full"],
                delta_rmse = rmse_oos - rmse_oos[ candidate == "full"])

#write.csv(oos_df,file="./output/model_outputs/spOOS_nb2/models_completed.csv")
#write.csv(oos_df,file="./output/model_outputs/stempOOS_nb2/models_completed.csv")
#write.csv(oos_df,file="./output/model_outputs/tempOOS_nb1/models_completed.csv")


sm_summary = oos_df %>%
  dplyr::group_by(holdout_type, model2) %>%
  dplyr::summarise(vartype = head(vartype, 1), 
                   deltamae_mean = mean(delta_mae),
                   deltamae_se = plotrix::std.error(delta_mae),
                   deltarmse_mean = mean(delta_rmse),
                   deltarmse_se = plotrix::std.error(delta_rmse))

#write.csv(sm_summary,file = "./output/model_outputs/spOOS_nb2/sm_summary.csv")
# factor order and colours

col_clim = viridis::viridis(200)[40]
col_socio= viridis::viridis(200)[105]
col_geo = viridis::magma(200)[150]

fac_order = sm_summary %>%
  dplyr::filter(holdout_type == "Spatial") %>%
  dplyr::arrange(deltarmse_mean)

fac_order = sm_summary %>%
  dplyr::filter(holdout_type == "Seasonal") %>%
  dplyr::arrange(deltarmse_mean)

fac_order = sm_summary %>%
  dplyr::filter(holdout_type == "Spatiotemporal") %>%
  dplyr::arrange(deltarmse_mean)



fac_order = fac_order$model2[ fac_order$model2 != "Full model" ]

#oos_df$holdout_type = factor(oos_df$holdout_type, levels=c("Spatial", "Spatiotemporal", "Seasonal"), ordered=TRUE)
#sm_summary$holdout_type = factor(sm_summary$holdout_type, levels=c("Spatial", "Spatiotemporal", "Seasonal"), ordered=TRUE)

#write.csv(oos_df,file = "./output/model_outputs/spOOS_poi5/models_completed.csv")
#oos_df <- read.csv("./output/model_outputs/spOOS_poi5/models_completed.csv")
head(oos_df)

p1 = oos_df %>%
  dplyr::filter(model2 != "Full model") %>%
  dplyr::mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  dplyr::mutate(delta_rmse = delta_rmse*100) %>%
  ggplot() + 
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=5, pch=16, alpha=0.15) +
  geom_point(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, deltarmse_mean*100), pch=18, color="black", alpha=1, size=3.25) +
  geom_linerange(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean*100-1.96*deltarmse_se*100, ymax=deltarmse_mean*100+1.96*deltarmse_se*100), color="black", alpha=1, size=0.8) +
  #geom_rect(aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=0), alpha=0.1, fill="green") + 
  geom_hline(yintercept=0, lty=2) + 
  theme_bw() + 
  ylab(expression(paste(Delta, "RMSE (out-of-sample) relative to full model"))) +
  #ylab("Improvement in MAE (out-of-sample)") +
  xlab("Covariate excluded") + 
  theme(strip.background = element_blank(), 
        panel.grid.minor =  element_blank(),
        strip.text=element_text(size=16),
        axis.text.y = element_text(size=12, color="black"), 
        axis.text.x = element_text(size=13), 
        axis.title = element_text(size=15),
        #legend.position="bottom",
        legend.position = c(0.8, 0.2), #legend.background = element_blank(),
        legend.text = element_text(size=14), legend.title = element_blank()) + 
  facet_wrap(~holdout_type, scales="free_x") +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))

p1 = gridExtra::grid.arrange(p1)
#ggsave(p1, file="./output/figures/Figure5_PredictiveHoldouts.pdf", device="pdf", units="in", width=9, height=5)



#################################################################################
# Three types

# Filter and process data for different plot groups
oos_df2 <- oos_df %>%
  filter(candidate %in% c("full","Social factors","Geographical factors","Climate factors","baseline"))

sm_summary2 <- sm_summary %>%
  filter(model2 %in% c("Baseline (all)","Climate factors","Social factors","Geographical factors"))

fac_order <- sm_summary2 %>%
  filter(holdout_type == "Spatiotemporal") %>%
  arrange(deltarmse_mean)
fac_order <- fac_order$model2[fac_order$model2 != "Full model"]

oos_df2$holdout_type <- factor(oos_df2$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)
sm_summary2$holdout_type <- factor(sm_summary2$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)

# Adjusting plot p2
p2 <- oos_df2 %>%
  filter(model2 != "Full model") %>%
  mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  mutate(delta_rmse = delta_rmse ) %>%
  ggplot() +
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=6, pch=16, alpha=0.15) +  # increased size
  geom_point(data=sm_summary2[sm_summary2$model2 != "Full model", ], aes(model2, deltarmse_mean ), pch=18, color="black", alpha=1, size=4) +  # increased size
  geom_linerange(data=sm_summary2[sm_summary2$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean  - 1.96 * deltamae_se , ymax=deltarmse_mean  + 1.96 * deltamae_se ), color="black", alpha=1, size=1) +  # increased size
  geom_hline(yintercept=0, lty=2) +
  theme_bw() +
  ylab(expression(paste(Delta, "RMSE (out-of-sample) relative to full model"))) +
  xlab("Covariate excluded") +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16),
        axis.text.y = element_text(size=12, color="black"),
        axis.text.x = element_text(size=13),
        axis.title = element_text(size=15),
        legend.position = c(0.8, 0.3),
        legend.text = element_text(size=14), legend.title = element_blank()) +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))
p2 <- gridExtra::grid.arrange(p2, widths=1.5)

# Process and plot for Geographical variables
oos_df3 <- oos_df %>%
  filter(vartype %in% c("Geographical"))

sm_summary3 <- sm_summary %>%
  filter(vartype %in% c("Geographical"))

fac_order <- sm_summary3 %>%
  filter(holdout_type == "Spatiotemporal") %>%
  arrange(deltarmse_mean)
fac_order <- fac_order$model2[fac_order$model2 != "Full model"]

oos_df3$holdout_type <- factor(oos_df3$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)
sm_summary3$holdout_type <- factor(sm_summary3$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)

p3 <- oos_df3 %>%
  filter(model2 != "Full model") %>%
  mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  mutate(delta_rmse = delta_rmse ) %>%
  ggplot() +
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=4, pch=16, alpha=0.15) +
  geom_point(data=sm_summary3[sm_summary3$model2 != "Full model", ], aes(model2, deltarmse_mean ), pch=18, color="black", alpha=1, size=3) +
  geom_linerange(data=sm_summary3[sm_summary3$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean  - 1.96 * deltamae_se , ymax=deltarmse_mean  + 1.96 * deltamae_se ), color="black", alpha=1, size=0.8) +
  geom_hline(yintercept=0, lty=2) +
  theme_bw() +
  ylab(expression(paste(Delta, "RMSE"))) +
  xlab("Covariate excluded") +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16),
        axis.text.y = element_text(size=12, color="black"),
        axis.text.x = element_text(size=13, angle=45, hjust=1),
        axis.title = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.3),
        legend.position = "none") +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))+
  ggtitle("Geographical factors")
p3 <- p3 + theme(aspect.ratio=1)  # Set aspect ratio to 1
p3
# Process and plot for Climate variables
oos_df4 <- oos_df %>%
  filter(vartype %in% c("Climate"))

sm_summary4 <- sm_summary %>%
  filter(vartype %in% c("Climate"))

fac_order <- sm_summary4 %>%
  filter(holdout_type == "Spatiotemporal") %>%
  arrange(deltarmse_mean)
fac_order <- fac_order$model2[fac_order$model2 != "Full model"]

oos_df4$holdout_type <- factor(oos_df4$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)
sm_summary4$holdout_type <- factor(sm_summary4$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)

p4 <- oos_df4 %>%
  filter(model2 != "Full model") %>%
  mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  mutate(delta_rmse = delta_rmse ) %>%
  ggplot() +
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=4, pch=16, alpha=0.15) +
  geom_point(data=sm_summary4[sm_summary4$model2 != "Full model", ], aes(model2, deltarmse_mean ), pch=18, color="black", alpha=1, size=3) +
  geom_linerange(data=sm_summary4[sm_summary4$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean  - 1.96 * deltamae_se , ymax=deltarmse_mean  + 1.96 * deltamae_se ), color="black", alpha=1, size=0.8) +
  geom_hline(yintercept=0, lty=2) +
  theme_bw() +
  ylab(expression(paste(Delta, "RMSE"))) +
  xlab("") +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16),
        axis.text.y = element_text(size=12, color="black"),
        axis.text.x = element_text(size=13, angle=45, hjust=1),
        axis.title = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.position = "none") +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))+
  ggtitle("Climate factors")
p4 <- p4 + theme(aspect.ratio=1)  # Set aspect ratio to 1
p4

# Process and plot for Social variables
oos_df5 <- oos_df %>%
  filter(vartype %in% c("Social"))

sm_summary5 <- sm_summary %>%
  filter(vartype %in% c("Social"))

fac_order <- sm_summary5 %>%
  filter(holdout_type == "Spatiotemporal") %>%
  arrange(deltarmse_mean)
fac_order <- fac_order$model2[fac_order$model2 != "Full model"]
#手动调整下顺序
#fac_order = c("Urbanization","Transportation","Migration index","Social factors")  

oos_df5$holdout_type <- factor(oos_df5$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)
sm_summary5$holdout_type <- factor(sm_summary5$holdout_type, levels=c("Spatiotemporal"), ordered=TRUE)

p5 <- oos_df5 %>%
  filter(model2 != "Full model") %>%
  mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  mutate(delta_rmse = delta_rmse ) %>%
  ggplot() +
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=4, pch=16, alpha=0.15) +
  geom_point(data=sm_summary5[sm_summary5$model2 != "Full model", ], aes(model2, deltarmse_mean ), pch=18, color="black", alpha=1, size=3) +
  geom_linerange(data=sm_summary5[sm_summary5$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean  - 1.96 * deltamae_se , ymax=deltarmse_mean  + 1.96 * deltamae_se ), color="black", alpha=1, size=0.8) +
  geom_hline(yintercept=0, lty=2) +
  theme_bw() +
  ylab(expression(paste(Delta, "RMSE"))) +
  xlab("") +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16),
        axis.text.y = element_text(size=12, color="black"),
        axis.text.x = element_text(size=13, angle=45, hjust=1),
        axis.title = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.position = "none") +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))+
  ggtitle("Social factors")
p5 <- p5 + theme(aspect.ratio=1)  # Set aspect ratio to 1
p5
p = gridExtra::grid.arrange(p3,p4,p5,nrow=1,widths=c("1.1","1.2","1") )

pp = gridExtra::grid.arrange(p2,p,nrow=2 )

##################
#ggsave(pp, file="./output/figures/Figure5_PredictiveHoldouts_all_NEW.pdf", device="pdf", units="in", width=12, height=7)




# 
# # ================ RMSE =================


p2 = oos_df %>%
  dplyr::filter(model2 != "Full model") %>%
  dplyr::mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
  ggplot() + 
  geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype), size=5, pch=16, alpha=0.15) +
  geom_point(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, deltarmse_mean), pch=18, color="black", alpha=1, size=3.25) +
  geom_linerange(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean-1.96*deltarmse_se, ymax=deltarmse_mean+1.96*deltarmse_se), color="black", alpha=1, size=0.8) +
  #geom_rect(aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=0), alpha=0.1, fill="green") + 
  geom_hline(yintercept=0, lty=2) + 
  theme_bw() + 
  ylab(expression(paste(Delta, "RMSE (out-of-sample) relative to full model"))) +
  #ylab("Improvement in MAE (out-of-sample)") +
  xlab("Covariate excluded") + 
  theme(strip.background = element_blank(), 
        panel.grid.minor =  element_blank(),
        strip.text=element_text(size=16),
        axis.text.y = element_text(size=12, color="black"), 
        axis.text.x = element_text(size=13), 
        axis.title = element_text(size=15),
        #legend.position="bottom",
        legend.position = c(0.92, 0.2), #legend.background = element_blank(),
        legend.text = element_text(size=14), legend.title = element_blank()) + 
  facet_wrap(~holdout_type, scales="free_x") +
  coord_flip() +
  scale_color_manual(
    values=c("Baseline (all)"="black", "Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)
  ) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha=0.7)))

p2 = gridExtra::grid.arrange(p2)


# oos_df %>%
#   dplyr::filter(model2 != "Full model") %>%
#   dplyr::mutate(model2 = factor(model2, levels=fac_order, ordered=TRUE)) %>%
#   ggplot() + 
#   geom_point(aes(model2, delta_rmse, group=unique_id, color=vartype2), size=5, pch=16, alpha=0.15) +
#   geom_point(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, deltarmse_mean), pch=18, color="black", alpha=1, size=3.25) +
#   geom_linerange(data=sm_summary[ sm_summary$model2 != "Full model", ], aes(model2, ymin=deltarmse_mean-1.96*deltarmse_se, ymax=deltarmse_mean+1.96*deltarmse_se), color="black", alpha=1, size=0.8) +
#   #geom_rect(aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=0), alpha=0.1, fill="green") + 
#   geom_hline(yintercept=0, lty=2) + 
#   theme_bw() + 
#   ylab(expression(paste(Delta, "RMSE (out-of-sample) relative to full model"))) +
#   #ylab("Improvement in MAE (out-of-sample)") +
#   xlab("Covariate excluded") + 
#   theme(strip.background = element_blank(), 
#         panel.grid.minor =  element_blank(),
#         strip.text=element_text(size=16),
#         axis.text.y = element_text(size=12, color="black"), 
#         axis.text.x = element_text(size=13), 
#         axis.title = element_text(size=15),
#         legend.position="none") + 
#   facet_wrap(~holdout_type, scales="free_x") +
#   coord_flip() +
#   scale_color_manual(
#     values=c("Baseline"="black", "Clim"=col_clim, "Socio"=col_socio)
#   )

# 


oos_results <- read.csv(".\\output\\model_outputs\\spOOS_nb2\\models\\1_output_33_157582_spOOS_nb_model_1_.csv")

oos_results <- read.csv(".\\output\\model_outputs\\spOOS_nb1\\models\\3_output_105_160205_spOOS_nb_model_1_.csv")

oos_results <- read.csv(".\\output\\model_outputs\\tempOOS_nb1\\models\\1_output_17_668896_tempOOS_nb_model_1_.csv")

oos_results <- read.csv(".\\output\\model_outputs\\stempOOS_nb2\\models\\1_output_1_969167_stempOOS_nb_model_1_.csv")


oos_results$predicted = exp(oos_results$logpop + oos_results$mean)
oos_results$resid = oos_results$predicted - oos_results$total_cases

head(oos_results)

sum(oos_results$total_cases)
sum(oos_results$predicted)
# calculate summary stats
mae_oos = mean(abs(oos_results$resid), na.rm=TRUE)
mae_oos
rmse_oos = sqrt(mean(oos_results$resid^2, na.rm=TRUE))
rmse_oos
correlation <- cor(oos_results$total_cases, oos_results$predicted)
correlation
cor.test(oos_results$total_cases, oos_results$predicted)


ggplot(oos_results, aes(x = total_cases, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 画出y=x的参考线
  labs(title = "Actual vs Predicted (Total Cases vs Predicted)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()


ggplot(oos_results, aes(x = resid)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Residuals (Total Cases - Predicted)",
       x = "Residuals (Total Cases - Predicted)",
       y = "Frequency") +
  theme_minimal()


ggplot(oos_results, aes(x = predicted, y = resid)) +
  geom_point(alpha = 0.6, color = "blue") +  # 绘制预测值与残差的散点图
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 添加水平参考线 (y=0)
  labs(title = "Residuals vs Predicted",
       x = "Predicted Total Cases",
       y = "Residuals (Total Cases - Predicted)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转x轴标签


ggplot(oos_results, aes(x = total_cases, y = predicted)) +
  geom_point(alpha = 0.6, color = "blue") +  # 绘制实际值与预测值的散点
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 添加线性回归拟合线
  geom_abline(slope = 1, intercept = 0, color = "green", linetype = "dashed") +  # 添加y=x参考线
  labs(title = "Actual vs Predicted (Total Cases)",
       x = "Actual Total Cases",
       y = "Predicted Total Cases") +
  theme_minimal()


ggplot(oos_results, aes(y = resid)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +  # 绘制箱线图
  labs(title = "Boxplot of Residuals (Total Cases - Predicted)",
       y = "Residuals (Total Cases - Predicted)") +
  theme_minimal()

ggplot(oos_results, aes(x = predicted, y = resid)) +
  geom_point(alpha = 0.6, color = "blue") +  # 绘制残差散点
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 添加y=0参考线
  labs(title = "Residuals vs Predicted",
       x = "Predicted Total Cases",
       y = "Residuals (Total Cases - Predicted)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

