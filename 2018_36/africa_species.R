###################################################################
#                                                                 #
#                RBZS DataViz Workshop Series 2021                #
#              ------------------------------------               #
#                  DataViz Challenge - Advanced                   #
#                                                                 #
###################################################################

# ...........    R version 4.1.0 (2021-05-18)   ...................

# Install and load packages --------------------------------------------

if (!require("pacman")) install.packages("pacman", .libPaths()[1])
p_unload("all") 
pacman::p_load(
  here,          # here_1.0.1         
  tidyverse,     # tidyverse_1.3.1     
  sf,            # sf_1.0-3 
  rnaturalearth, # rnaturalearth_0.1.0 
  rgeos,         # rgeos_0.5-8
  patchwork,     # patchwork_1.1.1
  tidytuesdayR   # tidytuesdayR_1.0.1  
  ) 


# Import data  --------------------------------------------------------------------

# Import with {tidytuesdayR}
tt_data <- tt_load(2018, week = 26)

data <- tt_data$africa_species

rm(tt_data)


# Remove stuff that I won't use -------------------------------------------------
data <- data %>% 
  select(-authority, -origin)

# What is "host" (environment_system)? 
# "The environment in which the species occurs or that it is associated with.
# This could include a host plant or host animal in the case of parasites or
# plant diseases, for example." 
# Source: https://www.nature.com/articles/sdata2017202.pdf (paper on GRIIS.com)



# Get the MAP of AFRICA ---------------------------------------------------------

sf_africa <- rnaturalearth::ne_countries(continent = 'africa', returnclass = "sf") 


# Check country names matching & fix --------------------------------------------------------------

# From "africa_species.csv"
country_invspp <- data$country %>% unique() #%>% sort()

# From sf_africa
country_sfafrica <- sf_africa %>% pull(sovereignt)

# Compare the elements of the two vectors
# - found in country_invspp but not in country_sfafrica
setdiff(country_invspp, country_sfafrica)
# [1] "Comoros"               "Gambia (the)"          "Guinea-Bissau"         "Mauritius"            
# [5] "Sao Tome and Principe" "Seychelles"  

# - found in country_sfafrica but not in country_invspp
setdiff(country_sfafrica, country_invspp)
# [1] "Central African Republic" "Ivory Coast"              "Republic of Congo"       
# [4] "Gambia"                   "Guinea Bissau"            "Western Sahara"          
# [7] "Somaliland"

# "Gambia (the)" and "Guinea-Bissau" are easy fixes:
data$country <- recode(data$country, "Gambia (the)" = "Gambia")
data$country <- recode(data$country, "Guinea-Bissau" = "Guinea Bissau") 


# "Comoros" "Mauritius" "Sao Tome and Principe" "Seychelles"    
# These are not included in rnaturalearth although they are officially in Africa ...
# So I won't have them on the map.

# Bummer because they have a lot of invasive spp ... !
data %>% 
  group_by(country) %>% 
  filter(country == "Comoros" | country == "Mauritius" | country == "Seychelles" | country == "Sao Tome and Principe") %>% 
  summarize(nr = length(species))  # %>% view()


# Run this to see that Comoros in not available in the rnaturalearth dataset
# (throws an error)
# comoros <- rnaturalearth::ne_countries(country = "Mauritius", returnclass = "sf") 


# Global plot settings =============================================================================

# Fonts
windowsFonts(descript = windowsFont("Candara")) 

# Set theme for the whole script
theme_set(theme_bw(base_family = "descript"))


# STAKED BARPLOT (all) ================================================================

# Create tibble
by_kingdom <- data %>% 
  group_by(country, kingdom) %>% 
  summarize(spp_nr = length(species)) %>% 
  group_by(country) %>% 
  mutate(tot_spp = sum(spp_nr)) %>% 
  ungroup() %>% 
  mutate(
    kingdom = factor(kingdom, levels = c("Fungi", "Bacteria", "Chromista", "Viruses", "Animalia", "Plantae")) )


# STACKED BAR PLOT --------------------------------------------------------------------------

# Palette
#             "Fungi", "Bacteria", "Chromista", "Viruses", "Animalia", "Plantae"
king_pal <- c("grey", "cadetblue", "khaki4", "darksalmon", "hotpink4", "olivedrab")


stacked <- ggplot(data = by_kingdom, aes(fill = kingdom, y = spp_nr, x = reorder(country, (tot_spp)) )) + # reorder(country, (tot_spp))
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(na.value = "transparent", 
                    values = king_pal) +
  coord_flip() +
  labs(title = "Number of species by biological Kingdom",
       fill = "Kingdom") +
  theme(
    legend.position = c(.83, .10),
    text = element_text(size = 16, color = "white"), 
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "black", color = NA),
    plot.title = element_text(
      size = 17,
      color = "grey78",
      hjust = 0.2, 
      vjust = 3.5),
    axis.title = element_blank(),
    axis.text.y = element_text(color = "white", size = 11, hjust = 1), #hjust = 1
    axis.text.x = element_text(color = "grey45"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = "transparent"),
    legend.key.height = unit(.40, 'cm'), # just changed
    legend.key.width = unit(.43, 'cm'),
    panel.grid.major.x = element_line(color = "grey21"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
    ) +
  guides(fill = guide_legend(reverse=TRUE))
                             
stacked



# MAPS ========================================================================

# Prep data ------------------------------------------------------------------

by_envsys <- data %>% 
  mutate(env_sys = case_when(
    str_detect(environment_system, "terrestrial") ~ "terrestrial",
    str_detect(environment_system, "host") ~ "host",
    str_detect(environment_system, "marine") ~ "aquatic",
    str_detect(environment_system, "freshwater") ~ "aquatic",
    str_detect(environment_system, "brackish") ~ "aquatic",
  )) %>% 
  group_by(country, env_sys) %>% 
  summarise(spp_nr = length(species)) %>% 
  ungroup()

# Spread with pivot wider
by_envsys <- by_envsys %>% 
  pivot_wider(names_from = env_sys, values_from = spp_nr) 

# merge with sf_africa 
plotdata <- left_join(sf_africa, by_envsys, by = c("sovereignt" = "country"))


# Adjust theme ------------------------------------------------------------------

theme_update(
  text = element_text(size = 16, colour = "white"), # , family="A"
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background = element_rect(fill = "black", color = NA),
  plot.margin = margin(0.3, 0.10, 0, 0.17, "cm"), #0.6, ...
  panel.border = element_blank(),
  axis.text = element_blank(),
  plot.title = element_text(
    size = 17,
    color = "grey78",
    hjust = 0.5,
    vjust = 1.5), #2
  legend.text = element_text(size = 12),
  legend.title = element_blank(),
  legend.background = element_rect(fill = NA, color = NA),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.ticks = element_blank() 
)

borderlines <- 0.1

# Map1 : all TERRESTRIAL -------------------------------------------
terr <- ggplot(plotdata) +
  geom_sf(aes(fill = terrestrial), color = "white", size = borderlines) +
  coord_sf(crs = "+proj=moll") +
  scale_fill_viridis_c(na.value = "transparent",
                       begin = 0.35, # 0.2
                       end = 0.8,
                       option = "F") +
  labs(title = "Terrestrial") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.5, "lines"),
      barheight = unit(4, "lines"))  )

terr


# Map2 : all AQUATIC --------------------------------------------
aqua <- ggplot(plotdata) +
  geom_sf(aes(fill = aquatic), color = "white", size = borderlines) +
  coord_sf(crs = "+proj=moll") +
  scale_fill_viridis_c(na.value = "transparent",
                       begin = 0.2,
                       end = 0.85,
                       option = "G") +
  labs(title = "Aquatic") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.5, "lines"),
      barheight = unit(4, "lines"))  )

aqua 


# Map3 : all PARASITES/DISEASES --------------------------------------------
host <- ggplot(plotdata) +
  geom_sf(aes(fill = host), color = "white", size = borderlines) +
  coord_sf(crs = "+proj=moll") +
  scale_fill_viridis_c(na.value = "transparent",
                       begin = 0.5,
                       option = "D") +
  labs(title = "Parasites or diseases") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(0.5, "lines"),
      barheight = unit(4, "lines"))  )

host


# Patch the together nicely ==============================================

patchwork <- stacked + (terr / aqua / host)

asp <- patchwork + plot_annotation(
  title = 'Invasive species in African countries (2018)',
  caption = 'RBZS DataViz Challenge 2021 • Giulia Puntin\nData from GRIIS.com • TidyTuesday Week-26 2018',
  theme = theme(
    plot.margin = margin(3, 1.5, 2, 2.2, "cm"), # top, right, bott, left
    plot.title = element_text(size = 30, vjust = 5), # size = 30, face = "bold", 
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey69", vjust = -14)) ) +
  plot_layout(widths = c(1.5,1))

asp


# Export -------------------------------------------------------------------------

# as PNG
ggsave("./africa_species_GP.png", dpi = 210, units = "cm", width = 35, height = 25)











