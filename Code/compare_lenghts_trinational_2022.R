library(tidyverse)
library(here) 
library(cowplot)
library(atm)

# Define regions to present in main Results
estimate.regions   <- c("Core", "Nearshore")

# Data frame for adding common names for labels
spp.key <- data.frame(Species = c("Clupea pallasii","Engraulis mordax","Sardinops sagax","Scomber japonicus","Trachurus symmetricus"),
                      Name = c("P. Herring","N. Anchovy","P. Sardine","P. Mackerel","Jack Mackerel"))

# Load abundance summary
load(here("Output/abundance_table_all.Rdata"))
load(here("Output/abundance_table_all_ns.Rdata"))

# Combine abundance summaries for tables
abund.summ.all.2021 <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  bind_rows(abund.summ.ns) %>%
  filter(!is.nan(abundance)) %>%
  group_by(Species, Stock, Region) %>% 
  mutate(abundance.pdf = abundance/sum(abundance)) %>% 
  ungroup() %>% 
  select(Species, Stock, Region, SL, abundance, abundance.pdf) %>%
  left_join(spp.key) %>% 
  mutate(group = paste(Species, Stock, sep = "-"),
         group.name = paste(Name, Stock, sep = "-"),
         survey = "2107RL",
         survey.name = "2021-Summer") 

L.abund.2021 <- ggplot(abund.summ.all.2021, aes(SL,abundance, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group.name, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2021 (2107RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

L.abund.2021.pdf <- ggplot(abund.summ.all.2021, aes(SL,abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity') + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2021 (2107RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

# Load abundance summary
load("C:/KLS/CODE/Github/estimATM/1907RL/Output/abundance_table_all.Rdata")
load("C:/KLS/CODE/Github/estimATM/1907RL/Output/abundance_table_all_ns.Rdata")

# Combine abundance summaries for tables
abund.summ.all.2019 <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  bind_rows(abund.summ.ns) %>%
  filter(!is.nan(abundance)) %>%
  group_by(Species, Stock, Region) %>% 
  mutate(abundance.pdf = abundance/sum(abundance)) %>% 
  ungroup() %>% 
  select(Species, Stock, Region, SL, abundance, abundance.pdf) %>%
  left_join(spp.key) %>% 
  mutate(group = paste(Species, Stock, sep = "-"),
         group.name = paste(Name, Stock, sep = "-"),
         survey = "1907RL",
         survey.name = "2019-Summer") 

L.abund.2019 <- ggplot(abund.summ.all.2019, aes(SL,abundance, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group.name, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2019 (1907RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

L.abund.2019.pdf <- ggplot(abund.summ.all.2019, aes(SL,abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group.name, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2019 (1907RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

# Load abundance summary
load("C:/KLS/CODE/Github/estimATM/2207RL/Output/abundance_table_all.Rdata")
load("C:/KLS/CODE/Github/estimATM/2207RL/Output/abundance_table_all_ns.Rdata")

# Combine abundance summaries for tables-2022 (Preliminary)
abund.summ.all.2022 <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  bind_rows(abund.summ.ns) %>%
  filter(!is.nan(abundance)) %>%
  group_by(Species, Stock, Region) %>% 
  mutate(abundance.pdf = abundance/sum(abundance)) %>% 
  ungroup() %>% 
  select(Species, Stock, Region, SL, abundance, abundance.pdf) %>%
  left_join(spp.key) %>% 
  mutate(group = paste(Species, Stock, sep = "-"),
         group.name = paste(Name, Stock, sep = "-"),
         survey = "2207RL",
         survey.name = "2022-Summer") 

L.abund.2022 <- ggplot(abund.summ.all.2022, aes(SL,abundance, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group.name, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2022 (2207RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

L.abund.2022.pdf <- ggplot(abund.summ.all.2022, aes(SL,abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (n)') +
  facet_wrap(~group.name, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Summer 2022 (2207RL)") + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold"))

abund.summ.all <- bind_rows(abund.summ.all.2019, abund.summ.all.2021, abund.summ.all.2022) %>% 
  filter(Species != "Etrumeus acuminatus") 

abund.summ.all.combo <- abund.summ.all %>%
  group_by(group, group.name, Region, SL) %>%
  summarise(abundance = sum(abundance)) %>% 
  group_by(group, group.name, Region) %>% 
  mutate(abundance.pdf = abundance/sum(abundance),
         survey = "All Years",
         survey.name = "All Years")

abund.summ.all.final <- bind_rows(abund.summ.all, abund.summ.all.combo)

L.abund <- ggplot(abund.summ.all, aes(SL,abundance/1000000, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Abundance (millions)') +
  facet_grid(survey.name~group.name, scales = "free_y") +
  theme_bw() +
  # labs(title = "Summer 2019 (1907RL)") + 
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold",
                                    size = 14),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "italic",
                                    size = 14))
ggsave(L.abund, filename = here("length_comparison_dodge.png"),
       height = 8, width = 12)

L.abund.pdf <- ggplot(abund.summ.all, aes(SL,abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Proportion') +
  facet_grid(survey.name~group.name, scales = "free") +
  theme_bw() +
  # labs(title = "Summer 2019 (1907RL)") + 
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic",
                                    size = 12),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "bold",
                                    size = 14))
ggsave(L.abund.pdf, filename = here("length_comparison_dodge_pdf.png"),
       height = 8, width = 13)

L.abund.pdf.combo <- ggplot(abund.summ.all.combo, aes(SL, abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Proportion') +
  facet_grid(survey.name~group.name, scales = "free") +
  theme_bw() +
  # labs(title = "Summer 2019 (1907RL)") + 
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic",
                                    size = 14),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "bold",
                                    size = 14))

ggsave(L.abund.pdf.combo, filename = here("length_comparison_dodge_pdf_combo.png"),
       height = 8, width = 13)

L.abund.pdf.combo2 <- plot_grid(L.abund.pdf, L.abund.pdf.combo, ncol = 1)

ggsave(L.abund.pdf.combo2, filename = here("length_comparison_dodge_pdf_combo2.png"),
       height = 8, width = 12)

L.abund.pdf.combo3 <- ggplot(abund.summ.all.final, aes(SL, abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Proportion') +
  facet_grid(survey.name~group.name, scale = "free") +
  theme_bw() +
  # labs(title = "Summer 2019 (1907RL)") + 
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic",
                                    size = 12),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "bold",
                                    size = 14))
ggsave(L.abund.pdf.combo3, filename = here("length_comparison_dodge_pdf_combo3.png"),
       height = 8, width = 13)

# Subset groups to include only NSNA and NSPS
abund.summ.sub1 <- abund.summ.all.final %>% 
  filter(group %in% c("Clupea pallasii-All","Engraulis mordax-Northern","Sardinops sagax-Northern"))

L.abund.pdf.sub1 <- ggplot(abund.summ.sub1, aes(SL, abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Proportion') +
  facet_grid(survey.name~group.name, scales = "free") +
  theme_bw() +
  # labs(title = "Summer 2019 (1907RL)") + 
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic",
                                    size = 14),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "bold",
                                    size = 14))

ggsave(L.abund.pdf.sub1, filename = here("length_comparison_dodge_pdf_sub1.png"),
       height = 8, width = 12)

# Subset groups to include remaining species and stocks
abund.summ.sub2 <- abund.summ.all.final %>% 
  filter(group %in% c("Engraulis mordax-Central","Sardinops sagax-Southern",
                      "Scomber japonicus-All","Trachurus symmetricus-All"))

L.abund.pdf.sub2 <- ggplot(abund.summ.sub2, aes(SL, abundance.pdf, group = Region, fill = Region)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
  scale_x_continuous("Length (cm)") + 
  scale_y_continuous('Proportion') +
  facet_grid(survey.name~group.name, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic",
                                    size = 14),
        strip.background.y = element_blank(),
        strip.text.y = element_text(face = "bold",
                                    size = 14))

ggsave(L.abund.pdf.sub2, filename = here("length_comparison_dodge_pdf_sub2.png"),
       height = 8, width = 12)
