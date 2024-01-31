
library(here)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2023, week = 23)

owid_energy <- tuesdata$`owid-energy`

rm(tuesdata)


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
# 
# To do this, energy generation from non-fossil sources are divided by a standard ‘thermal efficiency factor’ – typically around 0.4
# 
# Nuclear power is also adjusted despite it also experiencing thermal losses in a power plant. Since it’s reported in terms of electricity output, we need to do this adjustment to calculate its equivalent input value.
# 
# You can read more about this adjustment in our article.


# Explore and tidy data =================================
owid_energy %>% names()

# # Filter by income: "-income countries"
# owid_energy %>% 
#   filter(stringr::str_detect(country, "-income countries")) %>% view()

# Ideas:
# - Composition of energy use over time ("continuous" stacked bar plot)
#   This needs to be by country ... but can make it interactive in Shiny (drop-down select)
# - Trend in energy consumption pro capita as a function of gdp pro capita
#   Two options:
#   1. x = time; y = pro-capita energy consumption, color = gdp pro capita
#   2. x = gdp pro capita; y = pro-capita energy consumption
# - Something to show primary energy if renewable were fossil: basically to show that 
#   to sustain growth of pro-capita energy demand we will need a lot more oil ...
#   -> opportuniy to play with *forecast*!


# Plot 1: Where does your electricity come from? -----------------------------

# Filter cols to keep only *_share_elec
data1 <- owid_energy %>% select(country:gdp, electricity_generation, contains("_share_elec")) #%>% view()

# Make it long
data1_long <- data1 %>% pivot_longer(
  cols = contains("_share_elec"),
  names_to = "elec_source",
  values_to = "share"
)

# Remove "elec_source" from values as it is redundant info
data1_long <- data1_long %>% 
  mutate(elec_source = str_replace(elec_source, "_share_elec", "")) #%>% view()


# How many countries in the data set?
data1_long$country %>% n_distinct() # 306 seems too much ...

data1_long %>% filter(is.na(iso_code)) %>% #view()
  select(country) %>% 
  n_distinct()   # 87 these are actual countries (not aggregations)
                 # although the world tally is 195 countries as of today

# Check what aggregations we have (just curious)
# Note that we have data from different sources: BP, EIA, Shift, Ember
data1_long %>% filter(is.na(iso_code)) %>% #view()
  select(country) %>% unique() #%>% view()
  # n_distinct()
  
# Keep only data about actual countries (not aggregations)
data1_long_countries <- data1_long %>% 
  drop_na(iso_code)
  # filter(!is.na(iso_code))

# # Keep only data about certain aggregations: World, Africa, Asia, Oceania, South America and North America
# 
# # But first check how complete data is and if it matches across diff data sources (BP, Ember, ... )
# data1_long %>% 
#   filter(str_detect(country, "Africa")) %>% 
#   view()

# Remove aggregated measures (counted twice)
# to_remove <- c("low_carbon", "other_renewables_exc_biofuel", "renewables", "fossil") # "other_renewables",
individual_sources <- c("biofuel", "coal", "gas", "hydro", "nuclear", "oil", "wind")

data1_long_countries_indivsources <- data1_long_countries %>% 
  filter(elec_source %in% individual_sources)

# # Check if share is actually already in %
# data1_long_countries %>% 
#   filter(country == "Italy") %>% 
#   filter(elec_source %in% individual_sources) %>% # exclude coz already in ...
#   group_by(year) %>%
#   summarise(total_from_share = sum(share),
#             electricty_generation = unique(electricity_generation)) %>% view()
#   # looks like so however it does not add up to 100 (~88 - ~98  for Italy ...)
#   # For plot treat the rest as "other" = unknown


# Plot: {streamgraph} of electricity source by year - for one country at a time ----------------------

## Install pkg -----------------
# devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)


## Quick check: which year to start? -------------------

# Plot of a few countries to visually check
to_keep <- c("Italy", "Belgium", "United States")

data1_long_countries_indivsources %>% 
  filter(country %in% to_keep) %>% 
ggplot(data = ., 
       mapping = aes(x = year, y = share)) +
  geom_point(aes(color = country)) +
  facet_wrap(~elec_source)

# Get number out: year 1985
data1_long_countries_indivsources %>% 
  filter(!is.na(share)) %>% 
  pull(year) %>% min() # 1985

# # Keep only year >=1985 (make data set lighter)
# data <- data1_long_countries_indivsources %>% 
#   filter(year >= 1985)
# 
rm(list = setdiff(ls(), "data1_long_countries_indivsources")) # c("all_data", "asv_count_taxa")

## Streamgraph ---------------------------

ita <- data1_long_countries_indivsources %>%  filter(country == "Italy")

streamgraph::streamgraph(data = ita, key = elec_source, value = share, date = year) %>%
  sg_legend(show=TRUE, label="Share of electricity source: ")


# NOTE: 
# 1. Would look better if used *_cons_per_capita variable (instead of share)
# 2. Figure out how to personalize colors
# 3. Make patchwork one for #TidyTuesday (decide which countries to pick)
# 4. Make interactive one for Shiny app
  









