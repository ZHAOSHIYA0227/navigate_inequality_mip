#new plots October


#redo the ordered dataset
data_welfare_effect_reordered <- data_welfare_effect %>% mutate(Scenario.y=as.factor(Scenario.y), Scenario.y=fct_relevel(Scenario.y, c("REF", "1150", "650", "1150_redist", "650_redist", "REF_impact", "1150_impact", "1150_impact_redist", "650_impact", "650_impact_redist"))) 
data_welfare_effect_reordered <- data_welfare_effect_reordered %>% mutate(scenarioclass=case_when(str_detect(Scenario.y, "impact") ~ "EPC with avoided Impacts", str_detect(Scenario.y, "redist$") ~ "EPC redistribution", TRUE ~ "Climate Policy"))



#Imaclim resutls too crazy esp for canada
data_welfare_effect_reordered <- data_welfare_effect_reordered %>% filter(!(Model=="Imaclim" & Region=="Canada"))
data_welfare_effect_reordered <- data_welfare_effect_reordered %>% filter(!(Model=="Imaclim" & Region!="Canada"))



#boxplot Fig 1
avg_label <- function(x){return(data.frame(y = mean(x), label = sprintf("%+.1f",mean(x))))}
ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "650", "1150_redist", "650_redist", "1150_impact_redist", "650_impact_redist")) %>% filter(str_detect(Scenario.y, "650")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5, cbudget=gsub("[a-z_]","", Scenario.y)) %>% mutate(Scenario.y.nice=case_when(Scenario.y=="650" ~ "Climate Policy", Scenario.y=="650_redist" ~ "with EPC redistribution", Scenario.y=="650_impact_redist" ~ "and avoided Impacts"), Scenario.y.nice = fct_relevel(as.factor(Scenario.y.nice), c("Climate Policy", "with EPC redistribution", "and avoided Impacts")))) + geom_point(aes(x = Region, y = 100*(value.x_Equality_index-value.y_Equality_index), color=Model), size = 0.9) + facet_grid(Scenario.y.nice ~ Year, scales = "free")  + theme_bw() + geom_hline(yintercept = 0) + labs(y="Change in Gini from Reference [points]", x="", title="Impact on the Gini index") + geom_boxplot(aes(x = Region, y = 100*(value.x_Equality_index-value.y_Equality_index)), alpha=0.2, color="grey50") + stat_summary(aes(x = 11, y = 100*(value.x_Equality_index-value.y_Equality_index)), fun=mean, geom="crossbar") + stat_summary(aes(x = 11, y = 100*(value.x_Equality_index-value.y_Equality_index)), fun.data = avg_label, geom="text", vjust=1) + scale_x_discrete(limits = c("United States", "Canada", "France", "Japan", "Russia", "Mexico", "China", "Brazil", "South Africa", "India", "AVERAGE")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#color facets background
g <- ggplot_gtable(ggplot_build(last_plot()+ theme(strip.text.y = element_text(colour = 'white', face = "bold")))); stripr <- which(grepl('strip-r', g$layout$name)); fills <- c("darkred","darkblue","darkgreen")
k <- 1; for (i in stripr) {j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder)); g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]; k <- k+1;}
grid::grid.draw(g)
ggsave(filename = file.path(graphdir, "Gini_boxplot_final.png"), width = 12, height = 8, plot = g)
#saveplot("Gini boxplot final", width = 10, height = 6)



#for ggplotly analysis
ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "650", "1150_redist", "650_redist", "1150_impact_redist", "650_impact_redist")) %>% filter(str_detect(Scenario.y, "650")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5, cbudget=gsub("[a-z_]","", Scenario.y)) %>% mutate(Scenario.y.nice=case_when(Scenario.y=="650" ~ "Climate Policy", Scenario.y=="650_redist" ~ "with EPC redistribution", Scenario.y=="650_impact_redist" ~ "and avoided Impacts"), Scenario.y.nice = fct_relevel(as.factor(Scenario.y.nice), c("Climate Policy", "with EPC redistribution", "and avoided Impacts")))) + geom_point(aes(x = Region, y = 100*(value.x_Equality_index-value.y_Equality_index), color=Model), size = 0.9) + facet_grid(Scenario.y.nice ~ Year, scales = "free")  + theme_bw() + geom_hline(yintercept = 0) + labs(y="Change in Gini from Reference [points]", x="", title="Impact on the Gini index") + geom_boxplot(aes(x = Region, y = 100*(value.x_Equality_index-value.y_Equality_index)), alpha=0.2, color="grey50")
plotly::ggplotly()





#now same plot for GDP:
ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "650", "1150_redist", "650_redist", "1150_impact_redist", "650_impact_redist")) %>% filter(str_detect(Scenario.y, "650")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5, cbudget=gsub("[a-z_]","", Scenario.y)) %>% mutate(Scenario.y.nice=case_when(Scenario.y=="650" ~ "Climate Policy", Scenario.y=="650_redist" ~ "with EPC redistribution", Scenario.y=="650_impact_redist" ~ "and avoided Impacts"), Scenario.y.nice = fct_relevel(as.factor(Scenario.y.nice), c("Climate Policy", "with EPC redistribution", "and avoided Impacts")))) + geom_point(aes(x = Region, y = 100*(`value.y_GDP|PPP`/`value.x_GDP|PPP`-1), color=Model), size = 0.9) + facet_grid(Scenario.y.nice ~ Year, scales = "free")  + theme_bw() + geom_hline(yintercept = 0) + labs(y="Change in GDP from Reference [%]", x="", title="Impact on GDP") + geom_boxplot(aes(x = Region, y = 100*(`value.y_GDP|PPP`/`value.x_GDP|PPP`-1)), alpha=0.2, color="grey50") + stat_summary(aes(x = 11, y = 100*(`value.y_GDP|PPP`/`value.x_GDP|PPP`-1)), fun=mean, geom="crossbar") + stat_summary(aes(x = 11, y = 100*(`value.y_GDP|PPP`/`value.x_GDP|PPP`-1)), fun.data = avg_label, geom="text", vjust=1) + scale_x_discrete(limits = c("United States", "Canada", "France", "Japan", "Russia", "Mexico", "China", "Brazil", "South Africa", "India", "AVERAGE")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
g <- ggplot_gtable(ggplot_build(last_plot()+ theme(strip.text.y = element_text(colour = 'white', face = "bold")))); stripr <- which(grepl('strip-r', g$layout$name)); fills <- c("darkred","darkblue","darkgreen")
k <- 1; for (i in stripr) {j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder)); g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]; k <- k+1;}
grid::grid.draw(g)
ggsave(filename = file.path(graphdir, "GDP_boxplot_final.png"), width = 12, height = 8, plot = g)



#finally same plot for Sen-Foster Welfare
ggplot(data_welfare_effect_reordered %>% filter(Year %in% c(2030, 2050, 2100) & Region %in% countries_reported_max) %>% filter(Scenario.x=="REF" & Scenario.y %in% c("1150", "650", "1150_redist", "650_redist", "1150_impact_redist", "650_impact_redist")) %>% filter(str_detect(Scenario.y, "650")) %>% mutate(Scenario.y=factor(Scenario.y)) %>% mutate(Scenario.y.start = as.numeric(Scenario.y) - 0.5, Scenario.y.end = as.numeric(Scenario.y) + 0.5, cbudget=gsub("[a-z_]","", Scenario.y)) %>% mutate(Scenario.y.nice=case_when(Scenario.y=="650" ~ "Climate Policy", Scenario.y=="650_redist" ~ "with EPC redistribution", Scenario.y=="650_impact_redist" ~ "and avoided Impacts"), Scenario.y.nice = fct_relevel(as.factor(Scenario.y.nice), c("Climate Policy", "with EPC redistribution", "and avoided Impacts")))) + 
  geom_point(aes(x = Region, y = 100*((`value.y_GDP|PPP`*value.y_Equality_index)/(`value.x_GDP|PPP`*value.x_Equality_index)-1), color=Model), size = 0.9) + facet_grid(Scenario.y.nice ~ Year, scales = "free")  + theme_bw() + geom_hline(yintercept = 0) + labs(y="Change in Welfare from Reference [%]", x="", title="Impact on Welfare") + geom_boxplot(aes(x = Region, y = 100*((`value.y_GDP|PPP`*value.y_Equality_index)/(`value.x_GDP|PPP`*value.x_Equality_index)-1)), alpha=0.2, color="grey50") + stat_summary(aes(x = 11, y = 100*((`value.y_GDP|PPP`*value.y_Equality_index)/(`value.x_GDP|PPP`*value.x_Equality_index)-1)), fun=mean, geom="crossbar") + stat_summary(aes(x = 11, y = 100*((`value.y_GDP|PPP`*value.y_Equality_index)/(`value.x_GDP|PPP`*value.x_Equality_index)-1)), fun.data = avg_label, geom="text", vjust=1) + scale_x_discrete(limits = c("United States", "Canada", "France", "Japan", "Russia", "Mexico", "China", "Brazil", "South Africa", "India", "AVERAGE")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
g <- ggplot_gtable(ggplot_build(last_plot()+ theme(strip.text.y = element_text(colour = 'white', face = "bold")))); stripr <- which(grepl('strip-r', g$layout$name)); fills <- c("darkred","darkblue","darkgreen")
k <- 1; for (i in stripr) {j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder)); g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]; k <- k+1;}
grid::grid.draw(g)
ggsave(filename = file.path(graphdir, "Welfare_boxplot_final.png"), width = 12, height = 8, plot = g)


