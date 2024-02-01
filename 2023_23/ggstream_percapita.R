library(tidyverse)
library(here)


# tuesdata <- tidytuesdayR::tt_load(2023, week = 23)
# 
# owid_energy <- tuesdata$`owid-energy`
# 
# write_csv(owid_energy, "./owid_energy.csv")
# 
# rm(list = ls())

owid_energy <- read_csv("./owid_energy.csv")


# Definitions ========================================

# Watt-hour -----------------------------------
# 
# A watt-hour is the energy delivered by one watt of power for one hour. 
# Since one watt is equivalent to one Joule per second, a watt-hour is equivalent to 3600 Joules of energy.
# 
# Metric prefixes are used for multiples of the unit, usually:
# - kilowatt-hours (kWh), or a thousand watt-hours.
# - Megawatt-hours (MWh), or a million watt-hours.
# - Gigawatt-hours (GWh), or a billion watt-hours.
# - Terawatt-hours (TWh), or a trillion watt-hours.

# Primary energy -----------------------------------
# 
# Primary energy is the energy available as resources – such as the fuels burnt 
# in power plants – before it has been transformed. This relates to the coal before 
# it has been burned, the uranium, or the barrels of oil.
# Primary energy includes energy that the end user needs, in the form of electricity, 
# transport and heating, plus inefficiencies and energy that is lost when 
# raw resources are transformed into a usable form.

# Substitution method ----------------------
# 
# The ‘substitution method’ is used by researchers to correct primary energy consumption for efficiency losses experienced by fossil fuels. It tries to adjust non-fossil energy sources to the inputs that would be needed if it was generated from fossil fuels. It assumes that wind and solar electricity is as inefficient as coal or gas.
# To do this, energy generation from non-fossil sources are divided by a standard ‘thermal efficiency factor’ – typically around 0.4
# Nuclear power is also adjusted despite it also experiencing thermal losses in a power plant. Since it’s reported in terms of electricity output, we need to do this adjustment to calculate its equivalent input value.
# You can read more about this adjustment in our article.


# STREAM CHART from {ggstream} -------------------------------------
# Stream chart of energy consumption over time 
#  each country or region separately 

#  How much electricity (per country over time) and where does it come from (how was it generated)

# Variable to use: "*_consumption"
#  primary energy consumption from *, measured in kWh
#   primary E cons: tot E demand, which includes:
#     consumption of energy sector itself + losses during transf and distrib + final consumption by end users
#  This is a nice var to plot as stream chart because it starts from 0 (when no use or no record)

# biofuel_consumption	:	Primary energy consumption from biofuels, measured in terawatt-hours
# coal_consumption	:	Primary energy consumption from coal, measured in terawatt-hours
# gas_consumption	:	Primary energy consumption from gas, measured in terawatt-hours



# Filter cols to keep only *_consumption
data1 <- owid_energy %>% select(country:gdp, contains("_consumption")) #%>% view()

# Make it long
data1_long <- data1 %>% pivot_longer(
  cols = contains("_consumption"),
  names_to = "esource",
  values_to = "TWh"
)

# Remove "source" from values as it is redundant info
data1_long <- data1_long %>% 
  mutate(
    esource = str_replace(esource, "_consumption", ""),
    esource = str_to_title(esource)) #%>% view()


# # Keep only data about actual countries (not aggregations)
# data1_long_eu <- data1_long %>% 
#   drop_na(iso_code)
#   # filter(!is.na(iso_code))

# Keep only EU27 countries 

# Countries in alphabetic order
eu27 <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
  )
# Countries in latitudinal order (N to S)
eu27_lat <- c("Finland", "Sweden", "Estonia", "Latvia", "Lithuania", "Denmark", 
              "Germany", "Netherlands", "Belgium", "Luxembourg", "France", 
              "Austria", "Czechia", "Slovakia", "Slovenia", "Italy", "Malta",  # "Czech Republic"
              "Spain", "Portugal", "Croatia", "Hungary", "Poland", "Romania", 
              "Bulgaria", "Greece", "Cyprus", "Ireland")

all(sort(eu27) == sort(eu27_lat))

data1_long_eu <- data1_long %>% 
  filter(country %in% eu27)  

data1_long_eu$country %>% n_distinct() # 27 correct (names matched)

## Order countries by latitude -----------------
data1_long_eu$country %>% class() # "character"

# Make factor, then create var for ordering (not order in levels, in case I want use a different way of arranging them later)
# Create a tibble
eu27_lat #<- c("Finland", "Sweden", "Estonia", "Latvia", "Lithuania", "Denmark", "Germany", "Netherlands", "Belgium", "Luxembourg", "France", "Austria", "Czechia", "Slovakia", "Slovenia", "Italy", "Malta", "Spain", "Portugal", "Croatia", "Hungary", "Poland", "Romania", "Bulgaria", "Greece", "Cyprus", "Ireland")
latitude <- c(61.3, 64.09, 59.43, 57.09, 55.19, 56.06, 52.52, 52.29, 50.5, 49.39, 46.01, 47.08, 49.49, 48.12, 46.05, 41.54, 35.55, 40.28, 39.45, 45.17, 47.2, 52.11, 45.3, 42.21, 38.09, 35.11, 53.2)

lat <- tibble(country = eu27_lat, latitude = latitude)

# Add ordering factor
lat <- lat %>% arrange(desc(latitude)) %>% 
  mutate(latitude_order = order(!latitude)) #%>% view()

data1_long_eulat <- left_join(data1_long_eu, lat, by = join_by(country))

# Remove aggregated measures = keep only individual sources (so they're not counted twice)
individual_esources <- c("Biofuel", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar", "Wind")

indivesources <- data1_long_eulat %>% 
  filter(esource %in% individual_esources)

# Specify order of levels for source
indivesources$esource <- factor(indivesources$esource, levels = c("Wind", "Hydro", "Biofuel", "Solar", "Nuclear", "Gas", "Coal", "Oil"))

## Which years to keep? -------------------

# Oldest record
year1 <- indivesources %>% 
  filter(!is.na(TWh)) %>% 
  pull(year) %>% min() # 1965

# Most recent record
yearn <- indivesources %>% 
  filter(!is.na(TWh)) %>% 
  pull(year) %>% max() 

# Keep only years with data between 1965 and 2021 (make data set lighter)
indivesources <- indivesources %>%
  filter(year >= year1 & year <= yearn)


rm(list = setdiff(ls(), c("indivesources", "year1", "yearn"))) # c("all_data", "asv_count_taxa")


# Replace all 0.000 to NA so it can be handled by ggstream ----------------
indivesources <- indivesources %>%
  mutate(TWh = if_else(TWh == 0, NA, TWh))

# Normalize by population -> per-capita kWh ---------------------

# Check if population info is missing anywhere
indivesources %>% 
  filter(is.na(population)) #%>% view() # all good :)


indivesources <- indivesources %>% 
  mutate(kWh_percapita = (TWh * 10^6)/population) #%>% view()
  




# Color palette ----------------------
palette_energy <- c("Biofuel" = "#318C00", # green/brown 37E702 03D114 318C00
                    "Coal" = "#38323B",    # dark grey
                    "Gas" = "#534D5B",     # 
                    "Hydro" = "#87D4F5",   # blue 4cc2f3 25EAFB
                    "Nuclear" = "#ff000c", # red #ff000c #EB3114
                    "Oil" = "#1D171A",     # darker grey
                    "Wind" = "#11aeee",    # light blue #11aeee 27BEFF
                    "Solar" = "#FFC73F"      # yellow/orange FFE70A FFA909
                    )


# Fonts ------------------
windowsFonts()

# install.packages("extrafont")
library(extrafont)
# font_import() # not sure this has to run every time ... ?
loadfonts(device = "win")

windowsFonts()


# {ggstream} of primary energy consumption by year - for one country at a time ----------------------

## Install pkg -----------------
# install.packages("ggstream")
library(ggstream)
library(ggtext) # for element_markdown()

color_text <- "#F7F7F0" # "#B9D0DA"
color_bg <- "#176384"

indivesources %>% 
  # filter(country == "Italy") %>% 
  ggplot(., aes(x = year, y = kWh_percapita, fill = esource)) + # y = TWh
  # ggstream::geom_stream(
  #   geom = "contour",
  #   color = color_text,
  #   linewidth = 0.75,
  #   bw = 0.6
  # ) +
  ggstream::geom_stream(
    geom = "polygon",
    linewidth = 0,
    bw = 0.6,  # bandwidth: higher = smoother
    type = "mirror", # "ridge" "proportional" "mirror"
    color = "transparent" # "#1C5646" #  "#C8EFE4" # sorting = "inside_out"  # "onset"
  )  +
  scale_fill_manual(values = palette_energy) +
  scale_x_continuous(limits = c(year1, yearn), 
                     breaks = seq(1960, 2025, 10), 
                     expand = c(0.05, 0)) +
  # geom_hline(yintercept = c(-100, 0, 100), linetype = "dashed", color = color_bg) +
  labs(title = "Where does our energy come from?",
       subtitle = "Overview of the evolution of energy sources in Europe from 1965 to 2021.\n
Data shown as primary energy consumption from the different sources, measured in terawatt-hour or **TWh**.<br>
A TWh is equivalent to a million (1'000'000) kilowatt-hour (kWh is the unit of your electrcity bill).\n
As per Our World in Data definition, primary energy is the energy available as resources–such as the fuels burnt <br>
in power plants–before it has been transformed. This relates to the coal before it has been burned, the uranium, or the barrels of oil.<br>
Primary energy includes energy that the end user needs, in the form of electricity,transport and heating, plus inefficiencies<br>
and energy that is lost when raw resources are transformed into a usable form.",
       caption = "Visualization by Giulia Puntin  •  Data by Our World in Data  •  TidyTuesday 2023 w23") +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(colour = color_text, size = 15, family = "Roboto Black", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 8, hjust = 0, colour = color_text),
    plot.caption = element_text(size = 7, hjust = 0.5, colour = colorspace::darken(color_text, .05, space = "HLS")),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = color_bg, color = NA), # "#0D303F" 176384 0D303F
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(), #element_line(colour = "#C8EFE4", size = 0.1), # linetype = "dashed",
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # axis.title.x = element_blank(),
    # axis.title.y = element_text(colour = color_text, size = 13, face = "bold", angle = 0, vjust = 0.5),
    axis.text = element_text(colour = color_text),
    legend.title = element_blank(),
    legend.background = element_rect(fill = color_bg, color = NA), # colorspace::lighten(color_bg, .05, space = "HLS")
    legend.margin = margin(rep(10,4)),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(colour = color_text),
    legend.key = element_rect(colour = NA),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.5, "cm"),
    plot.margin = margin(rep(20, 4)),
    strip.text = element_text(colour = color_text, family = "Roboto", size = 12),
    strip.background = element_rect(colour = NA, fill = NA)
  ) +
  facet_wrap(
    ~fct_reorder(country, latitude_order), # country,
    # scales = "free_y",
    ncol = 3
    ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = F)
  )


# TO DO:
# o Remove Malta (no data)
# o write that data for Malta is missing
# o Make country labels smaller
# o make year smaller
# o Subtitle: kWh/year per person
# o say that it is ordered N-S
# o add again dash lines: v or h?


## Export .PNG -----------------------------------
ggsave("./EU27EnergySource.png",
       dpi = 330,
       units = "cm", 
       width = 21, height = 29.7) # A4 format 21 x 29.7




# NOTE: 
# 1. Would  be interesting to normalize by population (cons_per_capita)
#    also try with *_share_elec
# 2. Try with patchwork instead ...
# 3. Make interactive one for Shiny app with {streamgraph}?
  









