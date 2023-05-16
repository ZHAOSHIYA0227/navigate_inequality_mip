lis_y <- seq(2020, 2100, 10) %>% as.character()

Gini <- read.csv("Gini_full.csv") %>% 
  filter(!is.na(`Inequality.index.Gini`)) %>% 
  gdata::rename.vars(from = "Inequality.index.Gini", to = "Inequality index|Gini") %>% 
  mutate(dis = `Inequality index|Gini` - `Gini_recomputed`)

# 1. Gini trajectory ----
pdata <- Gini %>% 
  select(-"Gini_full") %>% 
  filter(Year %in% lis_y, 
         Scenario == "650") %>% 
  # pivot_longer(c("Inequality index|Gini", "Gini_recomputed"), 
               # names_to = "Gini_type", values_to = "value") %>% 
  mutate(Scenario_Model = paste0(Scenario, "_", Model))
  
  
p <- ggplot(data = pdata) + 
  geom_line(aes(x = Year, y = `Inequality index|Gini`, group = Scenario_Model, color = Model)) + 
  # geom_line(aes(x = Year, y = `Gini_recomputed`, group = Scenario_Model, color = Model)) + 
  facet_wrap(~Region)
p  

pdata <- read.csv("Gini_full.csv") %>% 
  filter(!is.na(`Inequality.index.Gini`)) %>%
  gdata::rename.vars(from = "Inequality.index.Gini", to = "Inequality index|Gini") %>% 
  select(-c("Gini_full", "Gini_recomputed", "X")) %>% 
  filter(Year %in% lis_y, 
         Scenario %in% c("650", "1150", "REF"),
         Region == "Japan",
         Model == "AIM") %>% 
  pivot_wider(names_from = "Scenario", values_from = "Inequality index|Gini") %>% 
  pivot_longer(c("650", "1150"), names_to = "Scenario", values_to = "value") %>% 
  mutate(dis = value-`REF`)


p <- ggplot(data = pdata) + 
  geom_line(aes(x = Year, y = dis, group = Scenario, color = Scenario)) #+ 
  # geom_line(aes(x = Year, y = `Gini_recomputed`, group = Scenario_Model, color = Model)) + 
  # facet_wrap(~Scenario)
p  



# 2. Gini discrepancy ----
pdata <- Gini %>% 
  select(-c("Gini_full", "Inequality index|Gini", "Gini_recomputed")) %>% 
  filter(Year %in% lis_y) %>% 
  mutate(Scenario_Model = paste0(Scenario, "_", Model))


p <- ggplot(data = pdata) + 
  geom_line(aes(x = Year, y = `dis`, group = Scenario_Model, color = Model)) + 
  # geom_line(aes(x = Year, y = `Gini_recomputed`, group = Scenario_Model, color = Model)) + 
  facet_wrap(~Region, scales = "free")
p  


# 3. Gini AIM ----
pdata <- Gini %>% 
  select(-c("Gini_full", "Inequality index|Gini", "Gini_recomputed")) %>% 
  filter(Year %in% lis_y,
         Model == "AIM") #%>% 
  # mutate(Scenario_Model = paste0(Scenario, "_", Model))


p <- ggplot(data = pdata) + 
  geom_line(aes(x = Year, y = `dis`, group = Scenario, color = Scenario)) + 
  # geom_line(aes(x = Year, y = `Gini_recomputed`, group = Scenario_Model, color = Model)) + 
  facet_wrap(~Region, scales = "free")
p  



##########################################################################

# 4. Japan Consumption ----
df_cons_tmp <- read_xlsx("model_results/NAVIGATE_template_inequality_variables_v2_AIM_230505.xlsx") %>% 
  filter(Model == "AIM", Region == "JPN", startsWith(`Variable`, "Consumption Decile"))
df_cons  <- df_cons_tmp %>% 
  pivot_longer(as.character(seq(2010,2100,10)), names_to = "Year", values_to = "value") %>% 
  pivot_wider(names_from = "Scenario", values_from = "value") %>% 
  pivot_longer(c("WP4_1150", "WP4_1150_redist", "WP4_650", "WP4_650_redist"), 
               names_to = "Scenario", values_to = "value") %>% 
  mutate(change = (value-`WP4_REF`)/WP4_REF*100, 
         DEC = gsub("Consumption Decile\\|D","", Variable)) 

write.csv(df_cons, file = "Japan_Consumption.csv")


## 4.1 Change in consumption compared to REF ----
pdata <- df_cons %>% 
  filter(Scenario == "WP4_650")

p <- ggplot(data = pdata) +
  geom_line(aes(x = factor(DEC, level = seq(1,10,1)), y = change, group = Year)) +
  facet_wrap(~Year, scales = "free")
p


## 4.2 Consumption trajectory ----
pdata <- df_cons_tmp %>% 
  pivot_longer(as.character(seq(2010,2100,10)), names_to = "Year", values_to = "value") %>% 
  mutate(DEC = gsub("Consumption Decile\\|D","", Variable))
  

p <- ggplot(data = pdata) +
  geom_line(aes(x = factor(DEC, level = seq(1,10,1)), y = value, group = Scenario, color = Scenario)) +
  facet_wrap(~Year, scales = "free")
p


# 5. GDP growth ----
df_GDP_tmp <- read_xlsx("model_results/NAVIGATE_template_inequality_variables_v2_AIM_230501.xlsx") %>% 
  filter(Model == "AIM", Region == "JPN", startsWith(`Variable`, "GDP"))
df_GDP <- df_GDP_tmp %>% 
  pivot_longer(as.character(seq(2010,2100,10)), names_to = "Year", values_to = "value")

pdata <- df_GDP %>% 
  pivot_wider(names_from = "Scenario", values_from = "value") %>% 
  pivot_longer(c("WP4_1150", "WP4_1150_redist", "WP4_650", "WP4_650_redist"), 
               names_to = "Scenario", values_to = "value") %>% 
  mutate(change = (value-`WP4_REF`)/WP4_REF*100) 
colnames(pdata)
p <- ggplot(data = pdata) +
  geom_line(aes(x = Year, y = change, group = Scenario, color = Scenario)) #+
  # facet_wrap(~, scales = "free")
p

