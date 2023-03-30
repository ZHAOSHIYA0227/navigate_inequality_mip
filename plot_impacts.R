# set colors for deciles in plots
colors_deciles <- c("purple", rev(colorspace::diverging_hcl(9, "Cyan-Magenta")))

#### GDP damages ####
reg_dam_plot = function(spec, dir ) {
  
  if(spec == "BHM") {
    
    title <- expression(atop(bold("Damages on GDP, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_reg_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = Reg_damages_bhm_frac*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Damages, %",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    
  }
  
  if(spec == "Adaptation") {
    
    title <- expression(atop(bold("Damages on GDP, across countries")))
  
    
    p_reg_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = Reg_damages_ada_frac*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Damages, %",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  }
  
  ggsave(filename = paste0(spec, "_region_damages_all.png"),
         width = 15, height = 7, path = dir)

  return(p_reg_dam)

}

reg_dam_plot(spec = "BHM", dir = "graphs")
reg_dam_plot(spec = "Adaptation", dir = "graphs")

#### GINI impacts ####
gini_plot = function(spec, dir) {
  
  if(spec == "BHM") {
    
    title <- expression(atop(bold("Impacts on Gini index, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_gini <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650"),
                        aes(x = Year, y = delta_gini_bhm*100,
                            linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "\u0394 Gini",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    
  }
  
  if(spec == "Adaptation") {
    
    title <- expression(atop(bold("Impacts on Gini index, across countries")))
    
    
    p_gini <- ggplot(mip_income_d %>% 
                       na.omit() %>% 
                       filter(Scenario == "REF" | Scenario == "650"),
                     aes(x = Year, y = delta_gini_ada*100,
                         linetype = Scenario, color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "\u0394 Gini",
           title = title) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  }
  
  ggsave(filename = paste0(spec, "_gini_impacts_all.png"),
         width = 15, height = 7, path = dir)
  
  return(p_gini)
  
}

gini_plot(spec = "BHM", dir = "graphs")
gini_plot(spec = "Adaptation", dir = "graphs")

# produce regression table of Delta Gini on Delta regional temperature

gini_temp_reg = function(spec, dir) {

  require(stargazer)
  
## First, compute final temperature

## stopping in 2050: E3ME, AIM/PHi
mip_final_50 <- mip_income_d %>% 
  filter(Model == "E3ME" | Model == "AIM/PHI") %>% 
  filter(Year == 2050) %>% 
  mutate(
    temp_final = temp_regional,
    delta_temp = temp_final - temp_start
  ) %>% 
  select(Scenario, Model, Region, Decile, Year, starts_with("delta"))

mip_final_2100 <- mip_income_d %>% 
  filter(Model != "E3ME" & Model != "AIM/PHI") %>% 
  filter(Year == 2100) %>% 
  mutate(
    temp_final = temp_regional,
    delta_temp = temp_final - temp_start
  ) %>% 
  select(Scenario, Model, Region, Decile, Year, starts_with("delta"))

mip_income_reg <- rbind(mip_final_2100, mip_final_50)


if(spec == "BHM") {
  
  gini_temp_reg <- lm(delta_gini_bhm*100 ~ delta_temp +
                        factor(Model), data = mip_income_reg)
}

if(spec == "Adaptation") {
  gini_temp_reg <- lm(delta_gini_ada*100 ~ delta_temp +
                        factor(Model), data = mip_income_reg)
}

stargazer(gini_temp_reg,
          type = "latex",
          dep.var.caption = "Climate impacts on Gini index",
          covariate.labels = c("Change in regional temperature",
                               "E3ME", "GEM-E3", "Imaclim", "NICE", "REMIND",
                               "RICE50+", "WITCH"),
          dep.var.labels.include=FALSE,
          model.names = FALSE,
          header=F,
          float=T,
          out = paste0("graphs/", spec, "_gini_temp_reg",".tex")
)

}

gini_temp_reg(spec = "BHM")
gini_temp_reg(spec = "Adaptation")


#### Decile damages ####
decile_plot = function (spec, dir) {
  
  mod_letters <- c("A", "E", "G", "I", "N", "r", "R", "W")
  
  mod_letters_utf <- unlist(lapply(mod_letters, utf8ToInt))
  
  
  if(spec == "BHM") {
    
    title <- expression(atop(bold("Damages at decile-level, across countries"),
                             scriptstyle("Without adaptation")))
  
   
    
  p_dec_dam <-  ggplot(mip_income_d %>% 
                         filter(Scenario == "REF" | Scenario == "650"
                         ) %>% 
                         mutate(Decile = factor(Decile,
                                                levels = c("D1", "D2", "D3", "D4", "D5",
                                                           "D6", "D7", "D8", "D9", "D10"))) ,
                       aes(x = Year, y = dam_dist_frac_bhm*100,
                           color = Decile, linetype = Scenario, shape = Model)) +
    geom_line(alpha = 0.5) +
    geom_point() +
    labs(x = "Year", y = "Damages, %", title = title) + ### checking whether common axis in ggarrange works
    scale_color_manual(name = "Income deciles", values = colors_deciles) +
    scale_shape_manual(name = "Models", values = mod_letters_utf) +
    facet_wrap(~ Region, ncol = 5) +
    scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  

  }
  
  if(spec == "Adaptation") {
    
    title <- expression(atop(bold("Damages at decile-level, across countries")))

    
    p_dec_dam <-  ggplot(mip_income_d %>% 
                           filter(Scenario == "REF" | Scenario == "650"
                           ) %>% 
                           mutate(Decile = factor(Decile,
                                                  levels = c("D1", "D2", "D3", "D4", "D5",
                                                             "D6", "D7", "D8", "D9", "D10"))) ,
                         aes(x = Year, y = dam_dist_frac_ada*100,
                             color = Decile, linetype = Scenario, shape = Model)) +
      geom_line(alpha = 0.5) +
      geom_point() +
      labs(x = "Year", y = "Damages, %", title = title) + ### checking whether common axis in ggarrange works
      scale_color_manual(name = "Income deciles", values = colors_deciles) +
      scale_shape_manual(name = "Models", values = mod_letters_utf) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    
  }
  
  ggsave(filename = paste0(spec, "_decile_damages.png"),
         width = 15, height = 7, path = dir)
  
  return(p_dec_dam)
  
}

decile_plot(spec = "BHM", dir = "graphs")
decile_plot(spec = "Adaptation", dir = "graphs")

decile_plot_sel_years = function(spec, dir) {
  
  require(ggpattern)
  
  if(spec == "BHM") {
    p_dec_sel_100 <-  ggplot(mip_income_d %>% 
                               filter(Scenario == "REF" | Scenario == "650"
                               ) %>% 
                               filter(Year == 2100) %>%
                               mutate(Decile = factor(Decile,
                                                      levels = c("D1", "D2", "D3", "D4", "D5",
                                                                 "D6", "D7", "D8", "D9", "D10"))) ,
                             aes(x = Model, y = dam_dist_frac_bhm*100)) +
      geom_bar_pattern(aes(pattern = Scenario,
                           group = Decile, fill = Decile),
                       stat = "identity", position = position_dodge(0.9), width = 2) +
      labs(x = "Model", y = "Damages, %",
           title = "Damages at decile level in 2100") + 
      scale_fill_manual(name = "Income deciles", values = colors_deciles) +
      scale_pattern_manual(name = "Scenario", values = c("stripe", "plasma")) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
  }
  
  if(spec == "Adaptation") {
  
 p_dec_sel_100 <-  ggplot(mip_income_d %>% 
           filter(Scenario == "REF" | Scenario == "650"
           ) %>% 
           filter(Year == 2100) %>%
           mutate(Decile = factor(Decile,
                                  levels = c("D1", "D2", "D3", "D4", "D5",
                                             "D6", "D7", "D8", "D9", "D10"))) ,
         aes(x = Model, y = dam_dist_frac_ada*100)) +
    geom_bar_pattern(aes(pattern = Scenario,
                         group = Decile, fill = Decile),
                     stat = "identity", position = position_dodge(0.9), width = 2) +
    labs(x = "Model", y = "Damages, %",
         title = "Damages at decile level in 2100") + 
    scale_fill_manual(name = "Income deciles", values = colors_deciles) +
    scale_pattern_manual(name = "Scenario", values = c("stripe", "plasma")) +
    facet_wrap(~ Region, ncol = 5) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
 
  }
  
  ggsave(filename = paste0(spec, "_decile_damages_2100.png"),
         width = 15, height = 7, path = dir)
  
  
}

decile_plot_sel_years(spec = "BHM", dir = "graphs")
decile_plot_sel_years(spec = "Adaptation", dir = "graphs")
