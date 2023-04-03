# set colors for deciles in plots
colors_deciles <- c("purple", rev(colorspace::diverging_hcl(9, "Cyan-Magenta")))

#### GDP damages ####
reg_dam_plot = function(spec, dir) {
  
  if(spec == "BHM") {
    
    # country-level damages
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
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    # avoided country-level damages by going from REF to 650
    title_2 <- expression(atop(bold("Avoided damages from REF to 650, across countries"),
                             scriptstyle("Without adaptation")))
    
    p_reg_avoided_dam <- ggplot(mip_income_d %>% 
                          na.omit() %>% 
                          filter(Scenario == "REF" | Scenario == "650") %>% 
                            select(Scenario:Year, Reg_damages_bhm_frac) %>% 
                            select(-Scenario_type) %>% 
                            distinct() %>% 
                            mutate(
                              Reg_damages_bhm_frac = Reg_damages_bhm_frac*100
                            ) %>% 
                            pivot_wider(
                              names_from = Scenario,
                              values_from = Reg_damages_bhm_frac
                            ) %>% 
                            mutate(
                              avoided_reg_dam_bhm = REF - `650`
                            ),
                        aes(x = Year, y = avoided_reg_dam_bhm,
                            color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided damages, %",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    
  }
  
  if(spec == "Adaptation") {
    
    # Country-level damages
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
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    # avoided country-level damages by going from REF to 650
    title_2 <- expression(atop(bold("Avoided damages from REF to 650, across countries"),
                               scriptstyle("Without adaptation")))
    
    p_reg_avoided_dam <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, Reg_damages_ada_frac) %>% 
                                  select(-Scenario_type) %>% 
                                  distinct() %>% 
                                  mutate(
                                    Reg_damages_ada_frac = Reg_damages_ada_frac*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = Reg_damages_ada_frac
                                  ) %>% 
                                  mutate(
                                    avoided_reg_dam_ada = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_reg_dam_ada,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided damages, %",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  }
  
  ggsave(filename = paste0(spec, "_region_damages_all.png"),
         plot = p_reg_dam,
         width = 15, height = 7, path = dir)

  ggsave(filename = paste0(spec, "_region_avoid_damages_all.png"),
         plot = p_reg_avoided_dam,
         width = 15, height = 7, path = dir)
  
  
  return(list(p_reg_dam, p_reg_avoided_dam))
  
}

reg_dam_plot(spec = "BHM", dir = "graphs")
reg_dam_plot(spec = "Adaptation", dir = "graphs")

#### GINI impacts ####
gini_plot = function(spec, dir) {
  
  if(spec == "BHM") {
    
    # Country-level impacts on Gini
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
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    # Avoided Gini impacts from REF to 650 scenario
    title_2 <- expression(atop(bold("Avoided impacts on Gini index from REF to 650, across countries"),
                               scriptstyle("Without adaptation")))
    
    p_avoided_gini <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, delta_gini_bhm) %>% 
                                  select(-Scenario_type) %>% 
                                  distinct() %>% 
                                  mutate(
                                    delta_gini_bhm = delta_gini_bhm*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = delta_gini_bhm
                                  ) %>% 
                                  mutate(
                                    avoided_gini_bhm = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_gini_bhm,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided \u0394 Gini",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
  }
  
  if(spec == "Adaptation") {
    
    # Country-level impacts on Gini index
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
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    # Avoided Gini impacts from REF to 650 scenario
    title_2 <- expression(atop(bold("Avoided impacts on Gini index from REF to 650, across countries")))
    
    p_avoided_gini <- ggplot(mip_income_d %>% 
                                  na.omit() %>% 
                                  filter(Scenario == "REF" | Scenario == "650") %>% 
                                  select(Scenario:Year, delta_gini_ada) %>% 
                                  select(-Scenario_type) %>% 
                                  distinct() %>% 
                                  mutate(
                                    delta_gini_ada = delta_gini_ada*100
                                  ) %>% 
                                  pivot_wider(
                                    names_from = Scenario,
                                    values_from = delta_gini_ada
                                  ) %>% 
                                  mutate(
                                    avoided_gini_ada = REF - `650`
                                  ),
                                aes(x = Year, y = avoided_gini_ada,
                                    color = Model)) +
      geom_line(linewidth = 1) +
      labs(y = "Avoided \u0394 Gini",
           title = title_2) +
      xlim(c(2020, 2100)) +
      facet_wrap(~ Region, ncol = 5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  }
  
  ggsave(filename = paste0(spec, "_gini_impacts_all.png"),
         plot = p_gini,
         width = 15, height = 7, path = dir)
  
  ggsave(filename = paste0(spec, "_gini_avoided_impacts_all.png"),
         plot = p_avoided_gini,
         width = 15, height = 7, path = dir)
  
  return(list(p_gini, p_avoided_gini))
  
}

gini_plot(spec = "BHM", dir = "graphs")
gini_plot(spec = "Adaptation", dir = "graphs")

# produces regression table of Delta Gini on Delta regional temperature

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
                        factor(Model) -1, data = mip_income_reg)
}

if(spec == "Adaptation") {
  gini_temp_reg <- lm(delta_gini_ada*100 ~ delta_temp +
                        Model -1, data = mip_income_reg)
}

stargazer(gini_temp_reg,
          type = "latex",
          # dep.var.caption = "Climate impacts on Gini index",
 #         covariate.labels = c("Change in regional temperature",
#                               "E3ME", "GEM-E3", "Imaclim", "NICE", "REMIND",
#                               "RICE50+", "WITCH"),
          dep.var.labels = "Gini impact  [points]",
          model.names = FALSE,
          header=F,
          float=T,
          single.row = T,
          out = paste0("graphs/", spec, "_gini_temp_reg",".tex")
)

graphdir = "graphs"
hutils::replace_pattern_in("Model|Region","", file_pattern="*.tex", basedir = graphdir)


}

gini_temp_reg(spec = "BHM")
gini_temp_reg(spec = "Adaptation")


#### Decile damages ####
decile_plot = function (spec, dir) {
  
  mod_letters <- c("A", "E", "G", "I", "N", "r", "R", "W")
  
  mod_letters_utf <- unlist(lapply(mod_letters, utf8ToInt))
  
  
  if(spec == "BHM") {
    
    # Decile-level impacts
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
    theme_bw() +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  
  
  # Avoided decile-level impacts from REF to 650
  title_2 <- expression(atop(bold("Avoided damages at decile-level from REF to 650")),
  scriptstyle("Without adaptation"))
  
  p_dec_avoided_dam <-  ggplot(mip_income_d %>% 
    na.omit() %>% 
    filter(Scenario == "REF" | Scenario == "650") %>% 
    select(Scenario:Year, Decile, dam_dist_frac_bhm) %>% 
    select(-Scenario_type) %>% 
    distinct() %>% 
    mutate(
      dam_dist_frac_bhm = dam_dist_frac_bhm*100,
      Decile = factor(Decile,
                      levels = c("D1", "D2", "D3", "D4", "D5",
                                 "D6", "D7", "D8", "D9", "D10"))
    ) %>% 
    pivot_wider(
      names_from = Scenario,
      values_from = dam_dist_frac_bhm
    ) %>% 
    mutate(avoided_reg_dam_bhm = REF - `650`) ,
                       aes(x = Year, y = avoided_reg_dam_bhm,
                           color = Decile, shape = Model)) +
    geom_line(alpha = 0.5) +
    geom_point() +
    labs(x = "Year", y = "Avoided damages, %", title = title_2) +
    scale_color_manual(name = "Income deciles", values = colors_deciles) +
    scale_shape_manual(name = "Models", values = mod_letters_utf) +
    facet_wrap(~ Region, ncol = 5) +
    scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
    theme_bw() +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
   }
  
  if(spec == "Adaptation") {
    
    # Decile-level impacts
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
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    
    # Avoided decile-level impacts from REF to 650
    title_2 <- expression(atop(bold("Avoided damages at decile-level from REF to 650")))
    
    p_dec_avoided_dam <-  ggplot(mip_income_d %>% 
                                   na.omit() %>% 
                                   filter(Scenario == "REF" | Scenario == "650") %>% 
                                   select(Scenario:Year, Decile, dam_dist_frac_ada) %>% 
                                   select(-Scenario_type) %>% 
                                   distinct() %>% 
                                   mutate(
                                     dam_dist_frac_bhm = dam_dist_frac_ada*100,
                                     Decile = factor(Decile,
                                                     levels = c("D1", "D2", "D3", "D4", "D5",
                                                                "D6", "D7", "D8", "D9", "D10"))
                                   ) %>% 
                                   pivot_wider(
                                     names_from = Scenario,
                                     values_from = dam_dist_frac_ada
                                   ) %>% 
                                   mutate(avoided_reg_dam_ada = REF - `650`) ,
                                 aes(x = Year, y = avoided_reg_dam_ada,
                                     color = Decile, shape = Model)) +
      geom_line(alpha = 0.5) +
      geom_point() +
      labs(x = "Year", y = "Avoided damages, %", title = title_2) +
      scale_color_manual(name = "Income deciles", values = colors_deciles) +
      scale_shape_manual(name = "Models", values = mod_letters_utf) +
      facet_wrap(~ Region, ncol = 5) +
      scale_x_continuous(limits = c(2020, 2100), breaks = c(2020, 2060, 2100)) +
      theme_bw() +
      theme(panel.spacing.x = unit(6, "mm"),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
  }
  
  ggsave(filename = paste0(spec, "_decile_damages.png"),
         plot = p_dec_dam,
         width = 15, height = 7, path = dir)
  
  ggsave(filename = paste0(spec, "_decile_avoided_damages.png"),
         plot = p_dec_avoided_dam,
         width = 15, height = 7, path = dir)
  
  
  return(list(p_dec_dam, p_dec_avoided_dam))
  
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
      theme_bw() +
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
   theme_bw() +
    theme(panel.spacing.x = unit(6, "mm"),
          legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
 
  }
  
  ggsave(filename = paste0(spec, "_decile_damages_2100.png"),
         width = 15, height = 7, path = dir)
  
  
}

decile_plot_sel_years(spec = "BHM", dir = "graphs")
decile_plot_sel_years(spec = "Adaptation", dir = "graphs")
