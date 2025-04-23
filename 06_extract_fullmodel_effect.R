

# full model and baseline model
load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_1.R")
#summary(mod_i)
mx = mod_i
rm(mod_i)

load("./output/model_outputs/Bayesian nb model/models/fullmod_nb_model_baseline.R")
mb = mod_i
rm(mod_i)

### extractRandomINLA: extract random effect and rename columns

# if effect is grouped/replicated by a factor, automatically assign each subgroup to its grouping factor (labelled 1:n) 
# if BYM model, further partition into u and v components

#' @param summary_random points to model$summary.random$effect_of_interest
#' @param effect_name name to assign to fitted effect in dataframe (can be anything)
#' @param model_is_bym boolean; to specify if model is joint Besag-York-Mollie
#' @param transform specify whether to exponentiate coefficients (i.e. back transform to relative risk)
#' 
extractRandomINLA = function(summary_random, effect_name, model_is_bym=FALSE, transform=FALSE){
  
  # extract model effect
  rf = summary_random %>%
    dplyr::rename("value"=1, "lower"=4, "median"=5, "upper"=6)
  
  # label by grouping factor (if not replicated, group is 1 for all observations)
  rf$group = rep(1:as.vector(table(rf$value)[1]), each=n_distinct(rf$value))
  
  # partition BYM into u and v components
  if(model_is_bym){
    rf$component = rep(c("uv_joint", "u_besag"), each=n_distinct(rf$value)/2)
    rf$value = rep(1:(n_distinct(rf$value)/2), n_distinct(rf$group)*2)
  }
  
  # back transform if specified
  if(transform == TRUE){
    rf[ , 2:7 ] = exp(rf[ , 2:7])
  }
  
  # name and return
  rf$effect = effect_name
  return(rf)
}


### extractFixedINLA: extract fixed effects and rename columns

extractFixedINLA = function(model, model_name="mod", transform=FALSE){
  ff = model$summary.fixed
  ff$param = row.names(ff)
  ff$param[ ff$param == "(Intercept)" ] = "Intercept"
  names(ff)[3:5] = c("lower", "median", "upper")
  if(transform == TRUE){
    ff[ 1:5 ] = exp(ff[ 1:5 ])
  }
  ff
}




# ====================== Extract and save fitted climate effects and parameters ========================

# ranefs
ranefs = mx$summary.random
rf = lapply(ranefs[2:length(ranefs)], extractRandomINLA, effect_name = "x", transform=FALSE)
for(i in 1:length(rf)){
  rf[[i]]$effect = names(ranefs[ 2:length(ranefs)][i] )
}
rf = do.call(rbind.data.frame, rf)
row.names(rf) = c()
write.csv(rf, "./output/model_outputs/Bayesian nb model/fitted_params/fullmodel_nonlinearfunctions_rw2.csv", row.names=FALSE)

# fixed effects
fixefs = extractFixedINLA(mx, model_name = "full_model") %>%
  dplyr::mutate(description = "slope parameter for scaled covariate")
row.names(fixefs) = c()
write.csv(fixefs, "./output/model_outputs/Bayesian nb model/fitted_params/fullmodel_fixedeffects.csv", row.names=FALSE)


# ======================== Visualise full results ===========================

# colours for different variable types
library(viridis)
col_clim = viridis::viridis(200)[40]
col_socio = viridis::viridis(200)[105]
col_geo = viridis::magma(200)[150]


# ranefs
ranefs = mx$summary.random
rf = lapply(ranefs[1:length(ranefs)], extractRandomINLA, effect_name = "x", transform=TRUE)
for(i in 1:length(rf)){
  rf[[i]]$effect = names(ranefs[ 1:length(ranefs)][i] )
}
rf = do.call(rbind.data.frame, rf)

# extract ranefs and create standardised plots
effs = unique(rf$effect)
effs
plots = vector("list", length(effs))
plotRF = function(x){
  
  rr=rf[ rf$effect == effs[x], ]
  
  #distinguish climate and social
  if(effs[x] %in% c("Tmean_g", "Precipitation_2m_g", "Wind_2m_g","Rh_3m_g","Sun_01m_g")){
    colx = col_clim
  } else if(effs[x] %in% c("in_migration_g","urban_g")){
    colx = col_socio
  }else {
    colx = col_geo
  }
  
  # if(effs[x] == "Tmean_g"){
  #   # rr$upper[ rr$value < 14 ] = NA
  #   # rr$lower[ rr$value < 14 ] = NA
  #   rr = rr %>% dplyr::filter(value >= quantile(ddf$tmean_1m, 0.001))
  # }
  
  rr = left_join(
    rr,
    data.frame(
      effect = c("yearx","polyid","Tmean_g","Rh_3m_g","Sun_01m_g", "in_migration_g", "urban_g","Forest_g","Cropland_g","Grassland_g", "Shrub_g","elevation_g"),
      effname = c("year","polyid","Mean temperature","Relative humidity (3-month lag)","Sun duration (1-month accumulative)","Migration index","Urbanization levels","Forest coverage","Cropland coverage","Grassland coverage","Shrub coverage","Elevation"),
      type = c("year","polyid","Climate","Climate","Climate", "Social", "Social",  "Geography","Geography","Geography","Geography","Geography"),
      units = c("year","polyid","Mean Temperature (°C)","Rh (%)","Sun (hours)", "Migration index", "Urbanization (%)", "Forest (km2)","Cropland (km2)","Grassland (km2)","Shrub (km2)","Elevation (km)" )
    )
  )
  
  # visualise plot
  px = ggplot(rr) + 
    geom_line(aes(value, median)) +
    geom_ribbon(aes(value, ymin=lower, ymax=upper), alpha=0.2, fill=colx) +
    geom_hline(yintercept=1, lty=2) +
    #facet_wrap(~effname, scales="free") +
    theme_classic() +
    ggtitle(rr$effname[1]) + 
    ylab("Relative risk") + xlab(rr$units[1]) +
    theme(plot.title = element_text(size=14.25, hjust=0.5),
          axis.text = element_text(size=12.25),
          axis.title = element_text(size=13))
  
  
  #add density of climate vars
  if(effs[x] == "Tmean_g"){
    densx = dd %>%
      dplyr::filter(Tmean >= min(rr$value) & Tmean <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Tmean), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Precipitation_2m_g"){
    densx = dd %>%
      dplyr::filter(Precipitation >= min(rr$value) & Precipitation <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Precipitation), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Wind_2m_g"){
    densx = dd %>%
      dplyr::filter(Wind >= min(rr$value) & Wind <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Precipitation), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Rh_3m_g"){
    densx = dd %>%
      dplyr::filter(Rh >= min(rr$value) & Rh <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Rh), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Sun_01m_g"){
    densx = dd %>%
      dplyr::filter(Sun >= min(rr$value) & Sun <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(Sun), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "in_migration_g"){
    densx = dd %>%
      dplyr::filter(in_migration >= min(rr$value) & in_migration <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.45, col="grey70", alpha=0.4) +
      geom_density(aes(in_migration), fill=colx, alpha=0.2,  size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=12.25),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Forest_g"){
    densx = dd %>%
      dplyr::filter(Forest >= min(rr$value) & Forest <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.25, col="grey70", alpha=0.4) +
      geom_density(aes(Forest), fill=colx, alpha=0.2, size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=12.25, hjust=0.5),
            axis.text.y = element_text(size=10.25),
            axis.text.x = element_text(size=10.25),
            axis.title = element_text(size=11),
            axis.title.y = element_text(size=10, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "Grassland_g"){
    densx = dd %>%
      dplyr::filter(Grassland >= min(rr$value) & Grassland <= max(rr$value)) %>%
      ggplot() +
      geom_vline(xintercept=0, lty=1, size=0.25, col="grey70", alpha=0.4) +
      geom_density(aes(Grassland), fill=colx, alpha=0.2, size=0.4, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=12.25, hjust=0.5),
            axis.text.y = element_text(size=10.25),
            axis.text.x = element_text(size=10.25),
            axis.title = element_text(size=11),
            axis.title.y = element_text(size=10, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Cropland_g"){
    densx = dd %>%
      dplyr::filter(Cropland >= min(rr$value) & Cropland <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Cropland), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "Shrub_g"){
    densx = dd %>%
      dplyr::filter(Shrub >= min(rr$value) & Shrub <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(Shrub), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  if(effs[x] == "urban_g"){
    densx = dd %>%
      dplyr::filter(urban >= min(rr$value) & urban <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(urban), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  
  if(effs[x] == "elevation_g"){
    densx = dd %>%
      dplyr::filter(elevation >= min(rr$value) & elevation <= max(rr$value)) %>%
      ggplot() +
      geom_density(aes(elevation), fill=colx, alpha=0.2,  size=0.6, color="grey50") +
      theme_classic() +
      theme(plot.title = element_text(size=14.25, hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.text.x = element_text(size=12.25),
            axis.title = element_text(size=13),
            axis.title.y = element_text(size=12, color="black")) +
      xlab(rr$units[1]) +
      ylab("Density") +
      scale_y_continuous(n.breaks=2)
    
    px = gridExtra::grid.arrange(px + theme(axis.title.x = element_blank()),
                                 densx, nrow=2, heights=c(0.8, 0.25))
  }
  
  # return
  plots[[x]] = px
}


prf = lapply(1:length(effs), plotRF)




effs = unique(rf$effect)
effs
rr=rf[ rf$effect == effs[1], ]

rr = left_join(
  rr,
  data.frame(
    effect = c("month","polyid","Tmean_g", "Precipitation_2m_g", "Rh_3m_g","Sun_01m_g","out_migration_g", "Grassland_g","Barren_g","Shrub_g"),
    effname = c("month","polyid","Mean temperature (1−month lag)", "Precipitation (2−month lag)",  "Relative humidity (6−month accumulative)", "Sun duration (1−month accumulative)","Migration index","Grassland coverage","Barren coverage","Shrub coverage"),
    type = c("month","polyid","Climate", "Climate", "Climate", "Climate", "Mobility", "Geography","Geography","Geography"),
    units = c("month","polyid","Mean Temperature (°C)", "Precipitation (mm)", "Rh (%)", "Sun (hours)", "Migration index","Grassland (km2)","Barren (km2)","Shrub (km2)" )
  )
)

# visualise plot
px = ggplot(rr) + 
  geom_line(aes(value, median)) +
  geom_ribbon(aes(value, ymin=lower, ymax=upper), alpha=0.2, fill="grey") +
  geom_hline(yintercept=1, lty=2) +
  #facet_wrap(~effname, scales="free") +
  theme_classic() +
  ggtitle(rr$effname[1]) + 
  ylab("Relative risk") + xlab(rr$units[1]) +
  theme(plot.title = element_text(size=14.25, hjust=0.5),
        axis.text = element_text(size=12.25),
        axis.title = element_text(size=13))

px

library(ggh4x)

# fixed effects plot
pfx = extractFixedINLA(mx, model_name="mod", transform=TRUE)
pfx$param

pfx = extractFixedINLA(mx, model_name="mod", transform=TRUE) %>%
  dplyr::filter(param != c("Intercept","logyear")) %>%
  dplyr::left_join(
    data.frame(
      param=c("urban_log", "transportation_log", "Cropland_log",  "elevation_log","Precipitation_02m_log","Sun","logyear","Forest_s"),
      paramname = c("Urban\nexpansion\n(log)", "Transportation\n(log km)", "Cropland\n(log km^2)","Elevation\n(log km)","Precipitation lag02\n(log mm)","Sun","logyear","Forest_s"),
      type = c("Social", "Social", "Geographical", "Geographical", "Climate","Climate","Climate","Geographical"),
      facet = c("Slope (risk ratio) ", "Slope (risk ratio)", "Slope (risk ratio)", "Slope (risk ratio)", "Slope (risk ratio)","Slope (risk ratio)", "Slope (risk ratio)", "Slope (risk ratio)")
    )
  ) %>%
  dplyr::mutate(paramname = factor(paramname, levels=c("Urban\nexpansion\n(log)", "Transportation\n(log km)", "Cropland\n(log km^2)","Elevation\n(log km)","Precipitation lag02\n(log mm)","Sun","logyear","Forest_s"), ordered=TRUE)
  ) %>%
  ggplot() +
  geom_point(aes(paramname, mean, col=type), size=2) +
  geom_linerange(aes(paramname, ymin=lower, ymax=upper, col=type), show.legend=FALSE) +
  geom_hline(yintercept=1, lty=2) +
  theme_classic() +
  ylab("Slope (risk ratio)") + 
  xlab("") + 
  theme(plot.title = element_text(size=14.25, hjust=0.5),
        axis.text = element_text(size=12.25),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("Climate"=col_clim, "Social"=col_socio, "Geographical"=col_geo)) +
  #scale_y_continuous(limits=c(0.7, 1.8), breaks=c(0.75, 1, 1.25, 1.5, 1.75), labels=c(0.75, 1, 1.25, 1.5, 1.75)) +
  #scale_color_viridis_d(begin=0.2, end=0.6) + 
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_text(angle=0, size=11),
  #       axis.title.y = element_blank(),
  #       #axis.title.y = element_text(size=11),
  #       axis.text.y = element_text(size=11)) +
  #ggtitle("Waffle\nwaffles") +
  theme(#strip.text = element_blank(),
    strip.text = element_text(size=11),
    #plot.title = element_text(size=12.15, color="white"),
    strip.background = element_blank(),
    strip.placement="outside",
    legend.text = element_text(size=12), legend.title = element_blank(), #legend.background=element_rect(fill=NA, color="grey70"),
    legend.position=c(0.2, 0.9)) + guides(colour = guide_legend(override.aes = list(size=5, alpha=0.8)))


pfx


# pfx2 = extractFixedINLA(mx, model_name="mod", transform=TRUE) %>%
#   dplyr::filter(param != "Intercept") %>%
#   dplyr::filter(grepl("tmean", param)) %>%
#   dplyr::left_join(
#     data.frame(
#       param=c("tmean_coolestmonth_s", "urban_s", "urbanexp10_log", "traffic_kmperinhab_log"),
#       paramname = c("Tmean\ncoolest\nmonth", "Built-up\nland", "Urban\nexpansion\nrate (log)", "Traffic\nper inhab\n(log)"),
#       type = c("Climate", "Urbanisation", "Urbanisation", "Mobility")
#     )
#   ) %>%
#   ggplot() +
#   geom_point(aes(paramname, mean), size=2, color=col_clim) +
#   geom_linerange(aes(paramname, ymin=lower, ymax=upper), color=col_clim) +
#   geom_hline(yintercept=1, lty=2) +
#   theme_classic() +
#   ylab("Slope (risk ratio)") + 
#   xlab("") + 
#   #scale_color_viridis_d(begin=0.2, end=0.6) + 
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle=0, size=11),
#         axis.text.y = element_text(size=10),
#         axis.title.y = element_text(size=11))

# fixed effects
#pfx = gridExtra::grid.arrange(pfx2, pfx1, widths=c(0.3, 1))

# p <- ggplot(pfx$data, aes(x = param, y = mean)) +
#   geom_point() +  # 添加点估计
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # 添加置信区间
#   theme_minimal() +  # 使用简洁的主题
#   labs(x = "Variables", y = "Mean Estimate")
# p

# row 1: fixed + socioenv
pc1 = gridExtra::grid.arrange(pfx, 
                              prf[[1]], 
                              prf[[2]],
                              prf[[3]], 
                              prf[[4]],
                              prf[[5]],
                              prf[[6]],
                              prf[[7]],
                              #prf[[8]],
                              #prf[[9]],
                              nrow=3)
#ggsave(pc1, file="./output/figures/Figure3_effect_true4.pdf", device="pdf", width=15.5, height=9, units="in")

# row 2: climatic
pc2 = gridExtra::grid.arrange(prf[[1]], prf[[2]],
                              prf[[3]], prf[[4]],
                              prf[[5]], prf[[6]],
                              prf[[7]], nrow=2)
ggsave(pc2, file="./output/figures/Figure3_climates.pdf", device="pdf", width=12, height=5.8, units="in")





# ================== Tabulate full model param results ===============
fxx = extractFixedINLA(mx)
fxx$param
# fxx 
fxx = extractFixedINLA(mx) %>%
  dplyr::left_join(
    data.frame(
      param=c("urban_log", "transportation_log", "Cropland_log", "Forest_log", "elevation_log", "Intercept")
    )
  ) %>%
  dplyr::mutate(Type = "Fixed effect") %>%
  dplyr::select(param, Type, median, lower, upper) %>%
  dplyr::mutate(median = round(median, 3), lower=round(lower, 3), upper=round(upper, 3)) %>%
  dplyr::rename("Parameter" = 1, "Median"=3, "CI_0.025"=4, "CI_0.975"=5) 

# hyperparams
hyp = mx$summary.hyperpar
hyp = round(hyp, 3)
hyp$param = row.names(hyp)
hyp$param
hyp = hyp %>%
  dplyr::left_join(
    data.frame(
      param = c("size for the nbinomial observations (1/overdispersion)", 
                "Precision for month",
                "Precision for polyid",
                "Phi for polyid",
                "Precision for Tmean_g", 
                "Precision for Precipitation_3m_g",
                "Precision for Rh_3m_g",
                "Precision for Sun_4m_g"),
      Parameter = c("Size (1/overdisp) for neg. binom",
                    "Precision for Month (RW1)", 
                    "Precision for District (BYM2)",
                    "Phi for District (BYM2)", 
                    "Precision for Tmean (RW2)", 
                    "Precision for Precipitation 3m (RW2)",
                    "Precision for Rh 3m (RW2)", 
                    "Precision for Sun 4m (RW2)")
    )
  ) %>%
  dplyr::mutate(Type = "Hyperparameter") %>%
  dplyr::select(8, 9, 4, 3, 5) %>%
  dplyr::rename(
    "Median"=3, "CI_0.025"=4, "CI_0.975"=5
  )

# combine and save
tab = rbind(fxx, hyp)
write.csv(tab, "./output/figures/SuppTable_FullModelTabulation.csv", row.names=FALSE)













# ================= Visualise ST random effects ====================

rf = extractRandomINLA(mx$summary.random$polyid, effect_name ="", model_is_bym = TRUE) %>%
  dplyr::filter(component == "uv_joint") %>%
  dplyr::left_join(data.frame(group=1:23, year=1998:2020)) %>%
  dplyr::left_join(
    dd %>% dplyr::select(polyid, areaid, year_useable_from) %>% distinct(),
    by=c("value"="polyid")
  ) %>%
  dplyr::mutate(mean = replace(mean, year < year_useable_from, NA)) 

#cs = colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))(200)
lims = max(abs(rf$mean), na.rm=TRUE)
lims = c(-lims, lims)

p1 = shp %>%
  dplyr::select(areaid) %>%
  dplyr::full_join(rf) %>%
  sf::st_as_sf() %>%
  ggplot()+
  geom_sf(aes(fill=mean), color=NA) + 
  geom_sf(data = shp_vt, color="grey20", size=0.5, fill=NA) + 
  scale_fill_gradientn(colors=rev(colorRampPalette(MetBrewer::met.brewer("Benedictus", 11))(200)), na.value="white", limits=lims, name="Posterior\nmean") +
  theme_void() + 
  facet_wrap(~year, nrow=3) +
  theme(strip.text = element_text(size=13), 
        legend.text = element_text(size=11),
        legend.title = element_text(size=12))
ggsave(p1, file="./output/figures/SuppFigure_STRanefs.jpg", device="jpg", units="in", dpi=300, width=12, height=9, scale=0.85)


