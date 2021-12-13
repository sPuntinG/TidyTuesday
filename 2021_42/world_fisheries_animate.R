
# TidyTuesday week 42 "Global Seafood"

if (!require("pacman")) install.packages("pacman", .libPaths()[1])
p_unload("all") 
pacman::p_load(
  here,          # here_1.0.1         
  tidyverse,     # tidyverse_1.3.1     
  sf,            # sf_1.0-3
  rnaturalearth, # rnaturalearth_0.1.0
  rgeos,         # rgeos_0.5-8
  gifski,
  gganimate,
  tweenr,
  transformr,
  glue,
  tidytuesdayR   # tidytuesdayR_1.0.1  
) 


# Import data  --------------------------------------------------------------------

# # Import with {tidytuesdayR}
# # (avoid having to download the heavy data on your machine)
# tt_data <- tt_load(2021, week = 42)
# 
# # list of 7: I pick 1
# # I'm curious about seafood consumption
# data <- tt_data$`fish-and-seafood-consumption-per-capita`
# 
# rm(tt_data)


# # Issue with querying GitHub ...
# tuesdata <- tidytuesdayR::tt_load('2021-10-12')
# 
# data <- tuesdata$`fish-and-seafood-consumption-per-capita`
# 
# rm(tuesdata)
# 
# write_csv(data, "percapita.csv")

data <- read_csv("percapita.csv", col_names = T)

# Adjust stuff -----------------------------------------------------------

# Rename that terrible col name ...
data <- data %>% 
  rename(kg_capita_year = "Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)")


# Keep only sovereign countries (exclude regions and other aggregations)
sovereign <- data %>% 
  filter(!is.na(Code))



# Get the world map from {rnaturalearth} ------------------------------------------------
wrld <- rnaturalearth::ne_countries(returnclass = "sf", scale = "small") 

# "iso_a3" is what I want to keep: it matches the format of data$Code
wrld_ne <- wrld %>% 
  select(iso_a3, geometry) %>% 
  rename(Code = iso_a3)

# remove Antarctica from wrld_ne (no data and big land mass that takes up space in the plot)
wrld_ne <- wrld_ne %>% 
  filter(Code != "ATA")

# Join
toplot <- right_join(sovereign, wrld_ne, by = "Code")

# remove Maldives ('coz I cannot plot it and it messes up the values scale)
toplot <- toplot %>% 
  filter(Code != "MDV")

# remove NA from Code
toplot <- toplot %>% 
  drop_na()

# Note that 'filter()' and "*_join()' preserve the obbject class (stays 'sf')
# but 'drop_na()' CHANGEs the object CLASS to DATA.FRAME !!!

# Convert toplot to class = "sf" (cyrrently class: "spec_tbl_df", "tbl_df", "tbl", "data.frame")
toplot <- st_as_sf(x = toplot, sf_column_name = "geometry")

class(toplot) # :)



# # # Try {gganimate} with simpler plot -----------------------------------------------
# 
# # Pick 3 big countries just to try out viz with a lighter data set
# bigones <- c("China", "Russia", "Australia")
# 
# bigs <- toplot %>% 
#   filter(Entity %in% bigones)
# 
# 
# 
# # static
# static <- ggplot() + 
#   geom_sf(data = wrld_ne, show.legend = FALSE) +
#   geom_sf(data = bigs, aes(fill = kg_capita_year)) +
#   coord_sf(crs = "+proj=robin") + # robinson
#   labs(title = "Prova one due tre") 
# 
# # now animate it
# anim <- static +
#   transition_states(Year, transition_length = 100, state_length = 1, wrap = T) + # transition_manual
#   labs(title = "Year {closest_state}") # {nlayers} {frame_along}
# anim
# # WOOOOOOOORKS!!!



# Make animated plot ---------------------------------

# Set font
windowsFonts(myfont = windowsFont("Consolas")) # Gungsuh  "Bahnschrift"

bkg <- "black"  # background color

static <- ggplot() + 
  geom_sf(data = wrld_ne, fill = NA,
          show.legend = FALSE) +  # aes(geometry = geometry),
  geom_sf(data = toplot, aes(fill = kg_capita_year), size = 0.001, colour = "grey21") + # geometry = geometry, 
  coord_sf(crs = "+proj=robin") +
  labs(title = "A growing appetite for seafood",
       subtitle = "How fish and seafood consumption changed between 1961 and 2017",
       fill = "Kg pro capita",
       caption = "\n\n\nGiulia Puntin  |  TidyTuesday 2021-42  |  Source: OurWorldinData.org"
         ) +
  scale_fill_viridis_c(na.value = "transparent",
                       begin = 0,
                       end = 1,
                       option = "B",  # "G", "H"
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(100, units = "mm"),
                         draw.ulim = FALSE,
                         title.position = 'top',
                         title.hjust = 0.5,
                         title.vjust = 0.5
                       )) +
  theme(
    text = element_text(size = 13, colour = "white", family = "myfont"), # , family="A"
    panel.background = element_rect(fill = bkg, color = NA),
    plot.background = element_rect(fill = bkg, color = NA),
    plot.margin = margin(1, 1, 1, 1, "cm"), #0.3, ...
    axis.text = element_blank(),
    plot.title = element_text(
      size = 23,
      color = "white",
      # face = "bold",
      hjust = 0,
      vjust = 2.5), #2
    plot.title.position = "panel",
    plot.subtitle = element_text(hjust = 0, size = 15),
    plot.caption = element_text(size = 11, hjust = .95),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.background = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "cm"),
    plot.tag = element_text(color = "white", size = 23),
    plot.tag.position = c(0.1, 0.25), #c(0.90, 0.90)
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank() 
  )
# static


a <- static + 
  transition_states(Year) +
  labs(tag = "{closest_state}")



# Make animated gif
animate(a, nframes = 200, 
        start_pause = 10, end_pause = 10, rewind = F,  
        res = 100, units = "cm", width = 30.5, height = 17.9,
        renderer = gifski_renderer("seafood.gif"))

# Just upload to Git!








