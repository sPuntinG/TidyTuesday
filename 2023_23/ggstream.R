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


# Keep only data about actual countries (not aggregations)
data1_long_countries <- data1_long %>% 
  drop_na(iso_code)
  # filter(!is.na(iso_code))

# # Replace NA with 0 in TWh (avoid problems downstream with geom_ggstream)
# data1_long_countries$TWh <- ifelse(is.na(data1_long_countries$TWh), 0, data1_long_countries$TWh)


# Remove aggregated measures (counted twice) = keep only individual sources
individual_esources <- c("Biofuel", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar", "Wind")

indivesources <- data1_long_countries %>% 
  filter(esource %in% individual_esources)

# Specify order of levels for source
indivesources$esource <- factor(indivesources$esource, levels = c("Wind", "Hydro", "Biofuel", "Solar", "Nuclear", "Gas", "Coal", "Oil"))

## Quick check: which year to start? -------------------

# Plot of a few countries to visually check
to_keep <- c("Italy", "Belgium", "United States")

# indivesources %>% 
#   filter(country %in% to_keep) %>% 
#   ggplot(data = ., 
#          mapping = aes(x = year, y = TWh)) +
#   geom_point(aes(color = country)) +
#   facet_wrap(~esource)

# Get number out: year 1985
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



# Color palette ----------------------
palette_energy <- c("biofuel" = "#318C00", # green/brown 37E702 03D114 318C00
                    "coal" = "#38323B",    # dark grey
                    "gas" = "#534D5B",     # 
                    "hydro" = "#87D4F5",   # blue 4cc2f3 25EAFB
                    "nuclear" = "#ff000c", # red #ff000c #EB3114
                    "oil" = "#1D171A",     # darker grey
                    "wind" = "#11aeee",    # light blue #11aeee 27BEFF
                    "solar" = "#FFC73F"      # yellow/orange FFE70A FFA909
                    )


# Fonts ------------------
windowsFonts()

# install.packages("extrafont")
library(extrafont)
# font_import() # not sure this has to run every time ... ?
loadfonts(device = "win")

windowsFonts()


# # Small plot to try
# ggplot(data = ita,
#        aes(x = year, y = TWh)) +
#   geom_point(aes(color = esource)) +
#   labs(title = "Italian stuff") +
#   theme(
#     text=element_text(family="Roboto"),
#     plot.title = element_text(family = "Roboto Black", face = "bold") # "Universe-Condensed" "Franklin Gothic Book"
#   )


# {ggstream} of primary energy consumption by year - for one country at a time ----------------------

## Install pkg -----------------
# install.packages("ggstream")
# library(ggstream)
library(ggtext)

# Try on Italy data
ita <- indivesources %>% filter(country == "Italy")
to_keep <- c("Italy", "Belgium", "Germany", "France", "Austria", "Spain", "Portugal", "Netherlands")
to_keep <- c("Austria", "Portugal")

color_text <- "#F7F7F0" # "#B9D0DA"
color_bg <- "#176384"

indivesources %>% 
  filter(country %in% to_keep) %>% 
  ggplot(., aes(x = year, y = TWh, fill = esource)) +
  ggstream::geom_stream(
    geom = "contour",
    color = color_text,
    linewidth = 0.75,
    bw = 0.6
  ) +
  ggstream::geom_stream(
    geom = "polygon",
    linewidth = 0,
    bw = 0.6,  # bandwidth: higher = smoother
    type = "mirror", # "ridge" "proportional" "mirror"
    color = "transparent" # "#1C5646" #  "#C8EFE4" 
    # sorting = "inside_out"  # "onset"
  )  +
  scale_fill_manual(values = palette_energy) +
  scale_x_continuous(limits = c(year1, yearn), 
                     breaks = seq(1960, 2025, 10), 
                     expand = c(0.05, 0)) +
  geom_hline(yintercept = c(-1000, -500, 0, 500, 1000), linetype = "dashed", color = color_bg) +
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
    plot.subtitle = element_markdown(size = 8, hjust = 0, colour = color_text),
    plot.caption = element_text(size = 7, hjust = 0.5, colour = colorspace::darken(color_text, .05, space = "HLS")),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = color_bg, color = NA), # "#0D303F" 176384 0D303F
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(), #element_line(colour = "#C8EFE4", size = 0.1), # linetype = "dashed",
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = color_text, size = 13, face = "bold", angle = 0, vjust = 0.5),
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
    ~country
    # scales = "free_y",
    # ncol = 2
    ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = F)
  )



## Export .PNG -----------------------------------
ggsave("./EUEnergySource.png",
       # bg = "white",
       dpi = 330,
       units = "cm", 
       width = 21, height = 29.7) # A4 format 21 x 29.7




# NOTE: 
# 1. Would  be interesting to normalize by population (cons_per_capita)
#    also try with *_share_elec
# 2. Set color palette to be meaningful and nice 
# 3. Make patchwork one for #TidyTuesday (decide which countries to pick)
# 4. Make interactive one for Shiny app with {streamgraph}
  









