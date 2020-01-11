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

# Summarise results across regions
biomass.ts.var <- biomass.ts %>% 
  filter(stratum == "All", !region %in% c("Offshore")) %>% 
  select(survey, species, stock, biomass_sd) %>% 
  group_by(survey, species, stock) %>%
  summarise(biomass_sd = sqrt(sum(biomass_sd^2)))

biomass.ts <- biomass.ts %>% 
  filter(stratum == "All", !region %in% c("Offshore")) %>%
  group_by(survey, species, stock) %>% 
  select(-region, -stratum, -biomass_sd, -biomass_cv) %>% 
  summarise_all(list(sum)) %>% 
  left_join(biomass.ts.var) %>% 
  mutate(biomass_cv = biomass_sd/biomass*100)

# Combine most recent estimates ------------------------------------------------

# Format data ------------------------------------------------------------------
biomass.ts <- biomass.ts %>% 
  left_join(select(survey.info, survey, date_start)) %>% 
  mutate(group = paste(species, stock, sep = "-"),
         year  = year(date_start)) %>% 
  filter(!group %in% c("Sardinops sagax-Southern","Engraulis mordax-Northern"))

# Create plot ------------------------------------------------------------------
# Create line plot - single
biomass.ts.line <- ggplot(biomass.ts, 
                          aes(x = factor(year), y = biomass, colour = group, group = group)) +
  geom_path() +
  geom_point() +
  scale_colour_manual(name = 'Species',
                    labels = c("Clupea pallasii", "Engraulis mordax", "Sardinops sagax",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color,  
                               sardine.color, pac.mack.color, jack.mack.color)) +
  # facet_wrap(~group) + 
  # facet_wrap(~group, scales = "free_y") + 
  xlab("Year") + 
  scale_y_continuous(expression(Biomass~(t)), labels = scales::comma) +
  theme_bw() 

# Save figure
ggsave(biomass.ts.line, 
       filename = here("Figs/fig_biomass_ts_line.png"),
       width = 10, height = 6)

# Create line plot - faceted
biomass.ts.line.facet <- ggplot(biomass.ts,
                                aes(x = factor(year), y = biomass, group = group)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_ci_lower, ymax = biomass_ci_upper), width = 0.1) +
  facet_wrap(~group) + 
  # facet_wrap(~group, scales = "free_y") + 
  xlab("Year") + 
  scale_y_continuous(expression(Biomass~(t)), labels = scales::comma) +
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
                         aes(x = factor(year), y = biomass, fill = group)) + 
  geom_bar(colour = "black", position = "stack", stat = "identity", width = 0.6) +
  scale_fill_manual(name = 'Species (Stock)',
                    labels = c("Clupea pallasii", "Engraulis mordax (Southern)", "Sardinops sagax (Northern)",
                               "Scomber japonicus", "Trachurus symmetricus"),
                    values = c(pac.herring.color, anchovy.color,  
                               sardine.color, pac.mack.color, jack.mack.color)) +
  xlab("Year") + 
  scale_y_continuous(expression(Biomass~(t)), labels = scales::comma) +
  ylab(expression(Biomass~(t))) +
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 0))

# Save figure
ggsave(biomass.ts.bar, 
       filename = here("Figs/fig_biomass_ts_bar.png"),
       width = 10, height = 4)
