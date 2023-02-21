if (get.db) {
  # Configure ODBC connection to AST database ------------------------------------
  ast.con  <- dbConnect(odbc(), 
                        Driver = "SQL Server", 
                        Server = "161.55.235.187", 
                        Database = "AST", 
                        Trusted_Connection = "True")
  
  # Import past estimates --------------------------------------------------------
  biomass.ts      <- tbl(ast.con, "tbl_ATM_BIOMASS") %>% collect()
  survey.info     <- tbl(ast.con, "tbl_SURVEY_LOG") %>% collect() #%>% mutate_if(is.character, str_trim)
  
  # Close database channel
  dbDisconnect(ast.con)  
  
  # Save data
  save(biomass.ts, survey.info,
       file = here("Output/biomass_database.Rdata"))
} else {
  # Load data
  load(here("Output/biomass_database.Rdata"))
}

# Load 2021 data
load("C:/KLS/CODE/Github/estimATM/2107RL/Output/biomass_timeseries_export.Rdata")
be.db.export.2021 <- be.db.export

# Load present year estimates
load(here("Output/biomass_timeseries_export.Rdata"))

# Combine with current survey results
biomass.ts <- biomass.ts %>% 
  filter(!survey %in% unique(be.db.export$survey)) %>%
  bind_rows(filter(be.db.export.2021, region %in% estimate.regions)) %>% 
  bind_rows(filter(be.db.export, region %in% estimate.regions))

# Summarise results across regions
biomass.ts.var <- biomass.ts %>% 
  filter(stratum == "All") %>% 
  select(survey, species, stock, biomass_sd) %>% 
  group_by(survey, species, stock) %>%
  summarise(biomass_sd = sqrt(sum(biomass_sd^2)))

biomass.ts <- biomass.ts %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>%
  filter(stratum == "All", include_ts == TRUE) %>%
  filter(season == "Summer", stratum == "All", include_ts == TRUE) %>%
  select(-season, -region, -stratum, -biomass_sd, -biomass_cv, -date_start, -group, -year, -include_ts) %>%
  group_by(survey, species, stock) %>% 
  summarise_all(list(sum)) %>% 
  left_join(biomass.ts.var) %>% 
  mutate(biomass_cv = biomass_sd/biomass*100)

# Format data ------------------------------------------------------------------
biomass.ts <- biomass.ts %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start),
         season = case_when(
           month(date_start) < 6 ~ "Spring",
           TRUE ~ "Summer")) %>% 
  # filter(!group %in% c("Sardinops sagax-Southern","Engraulis mordax-Northern")) %>% 
  filter(!biomass == 0)

# Summarize community biomass by year
biomass.comm.summ <- biomass.ts %>% 
  # filter(group != "Sardinops sagax-Southern") %>% 
  group_by(year, survey) %>% 
  summarise(biomass.total = sum(biomass))

# Summarize species biomass per year
biomass.spp.summ <- biomass.ts %>% 
  group_by(year, survey, species, stock) %>% 
  summarise(biomass = sum(biomass)) %>% 
  left_join(biomass.comm.summ) %>% 
  mutate(biomass.pct = biomass/biomass.total*100)

save(biomass.ts, biomass.comm.summ, biomass.spp.summ,
     file = here("Output/biomass_timeseries_final.Rdata"))

# Create plot ------------------------------------------------------------------
# Create line plot - single
biomass.ts.line <- ggplot(filter(biomass.ts, biomass != 0), 
                          aes(x = date_start, y = biomass, colour = group, group = group)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  scale_colour_manual(name = 'Species',
                    labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                               "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color, "#93F09F",
                               rnd.herring.color, sardine.color, "#FF7256",
                               pac.mack.color, jack.mack.color)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"))

# biomass.ts.line <- ggplot(filter(biomass.ts, biomass != 0), 
#                           aes(x = date_start, y = biomass, colour = group, group = group)) +
#   geom_path() +
#   geom_point() +
#   geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
#   scale_colour_manual(name = 'Species',
#                       labels = c("Clupea pallasii", "Engraulis mordax-Central", "Etrumeus acuminatus",
#                                  "Sardinops sagax-Northern", "Scomber japonicus", "Trachurus symmetricus"),
#                       values = c(pac.herring.color, anchovy.color, rnd.herring.color, 
#                                  sardine.color, pac.mack.color, jack.mack.color)) +
#   scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
#   scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
#   theme_bw() +
#   theme(legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.line, 
       filename = here("Figs/fig_biomass_ts_line.png"),
       width = 7, height = 3)

# Create line plot - faceted
biomass.ts.line.facet <- ggplot(biomass.ts,
                                aes(x = date_start, y = biomass, group = group)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 5000000) +
  facet_wrap(~group) + 
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  theme_bw() +
  theme(strip.background.x   = element_blank(),
        strip.text.x         = element_text(face = "italic"),
        legend.position      = c(0.95,0.05),
        legend.justification = c(1,0))

# Save figure
ggsave(biomass.ts.line.facet, 
       filename = here("Figs/fig_biomass_ts_line_facet.png"),
       width = 10, height = 6)

# Create stacked bar plot
biomass.ts.bar <- ggplot(biomass.ts, 
                         aes(x = date_start, y = biomass, fill = group)) + 
  geom_bar(colour = "black", position = "stack", stat = "identity") +
  scale_fill_manual(name = 'Species',
                      labels = c("Clupea pallasii", "Engraulis mordax (Central)", "Engraulis mordax (Northern)",
                                 "Etrumeus acuminatus", "Sardinops sagax (Northern)", "Sardinops sagax (Southern)",
                                 "Scomber japonicus", "Trachurus symmetricus"),
                      values = c(pac.herring.color, anchovy.color, "#93F09F",
                                 rnd.herring.color, sardine.color, "#FF7256",
                                 pac.mack.color, jack.mack.color)) +
  scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
  ylab(expression(Biomass~(italic(t)))) +
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 0),
        legend.text = element_text(face = "italic"))

# # Create stacked bar plot
# biomass.ts.bar <- ggplot(biomass.ts,
#                          aes(x = date_start, y = biomass, fill = group)) +
#   geom_bar(colour = "black", position = "stack", stat = "identity") +
#   scale_fill_manual(name = 'Species (Stock)',
#                     labels = c("Clupea pallasii", "Engraulis mordax (Central)", 
#                                "Etrumeus acuminatus", "Sardinops sagax (Northern)", 
#                                "Scomber japonicus", "Trachurus symmetricus"),
#                     values = c(pac.herring.color, anchovy.color, 
#                                rnd.herring.color, sardine.color, 
#                                pac.mack.color, jack.mack.color)) +
#   scale_x_datetime(name = "Year", date_breaks = "2 years", date_labels = "%Y") +
#   scale_y_continuous(expression(Biomass~(italic(t))), labels = scales::comma) +
#   ylab(expression(Biomass~(italic(t)))) +
#   theme_bw() +
#   theme(axis.text.y = element_text(angle = 0),
#         legend.text = element_text(face = "italic"))

# Save figure
ggsave(biomass.ts.bar, 
       filename = here("Figs/fig_biomass_ts_bar.png"),
       width = 8, height = 4)
