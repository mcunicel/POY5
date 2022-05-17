##################################################
## Project: poy5
## Script purpose: Basic barplot
## Date:
## Author:
##################################################


poy5 <- read_csv(here("data", "6loc POY5 SAS CSV no checks no parents.csv"))
poy5 <- read_excel(here("data", "POY5 SAS Master.xlsx"), sheet = "everything with equations ")

poy5_reduced <- poy5 %>% 
  mutate(is_check = !grepl("RIL", Purpose)) %>% 
  janitor::clean_names() %>%
  select(year, plot, loc, line, purpose, is_check, yld_kg_ha, prot, oil)


poy5_long <- poy5_reduced %>% 
  pivot_longer(cols = c(yld_kg_ha, prot, oil), names_to = "phenotype") %>% 
  mutate(value = as.numeric(value)) %>% 
  dplyr::filter(!is.na(value)) %>% 
  group_by(is_check, phenotype) %>% 
  summarise(avg_value = mean(value)) %>% 
  mutate(is_check = ifelse(is_check, "Check", "RIL"))

library(ggthemes)


ggplot(poy5_long, aes(x = phenotype, y = avg_value, fill = is_check)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~phenotype, scales = "free") + 
  xlab("Phenotype") + 
  ylab("Average Value") + 
  theme_bw()
