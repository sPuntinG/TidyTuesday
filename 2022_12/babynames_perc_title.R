
#------------- R version 4.3.2 (2023-10-31 ucrt) -- "Eye Holes" --------------------


library(tidyverse)
library(here)


# tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
# 
# babynames <- tuesdata$babynames
# 
# write_csv(babynames, "./babynames.csv")
# 
# rm(list = ls())

babynames <- read_csv("./babynames.csv")

babynames$name %>% n_distinct() # 97'310 (!!!)
names10 <- babynames$name %>% unique() %>% .[1:10] # small set for now as it's a huge data set ... !


# Count right key letters in each name ---------------------------------

# Make function to iterate
count_right <- function(x) {
  rightk <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")
  a <- stringr::str_count(x, fixed(rightk, ignore_case = T))
  a <- sum(a)
  return(a)
}


count_right("mario") # 3
# count_right1(names10) # as expected doesn't work on vectorized argument


# Use map_dbl to loop count right function over vector of names -----------------------


# Apply to full list of names 

all_names <- babynames$name %>% unique()

tibble_all <- tibble(name = all_names, 
                     tot_letters = nchar(name),
                     right_letters = map_dbl(all_names, count_right),
                     left_letters = tot_letters - right_letters,
                     ratio = right_letters/left_letters,
                     predominance = case_when(
                       ratio == 1 ~ "none",
                       ratio > 1 ~ "right",
                       ratio < 1 ~ "left"
                     ),           
                     perc_right = right_letters/tot_letters * 100
)
   

# Join to babynames 
babynames_rl <- left_join(babynames, tibble_all, by = "name")


# Color palette ------------------

# USA flag
"#B31942" 
"#FFFFFF"
"#0A3161"




# Fonts OLD ------------------
windowsFonts()

# install.packages("extrafont")
library(extrafont)
# font_import() # to run only when new fonts added to the system?
loadfonts(device = "win")

extrafont::fonttable() %>% view()

windowsFonts()


# Plot: weighted mean % of right vs left key letters in baby names -----------------------------

# Plot average percentage of right key letters in names (per year)

# New version with normalization by n
babynames_rl_toplot <- babynames_rl %>% 
  # filter(year > 1980) %>%
  group_by(year) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  mutate(
    perc_right_n = perc_right * n
  ) %>% 
  # select(year, n, n_tot, perc_right1, perc_right1_n) %>% #view() name, 
  group_by(year) %>%
  reframe(perc_right_n_mean = sum(perc_right_n) / n_tot) %>% #view()
  unique()




# Add image
img <- png::readPNG("qwertyLR.png")
raster <- grid::rasterGrob(img, interpolate = TRUE)


library(ggtext) # for element_markdown() and geom_textbox()

# Main plot
p1 <- ggplot(babynames_rl_toplot, aes(x = year, y = perc_right_n_mean)) +
  geom_hline(yintercept = c(45, 50), color = "white", linewidth = 1.25) +
  # geom_hline(yintercept =  seq(35, 47, 1), color = "white", linewidth = 0.7) +
  # geom_hline(yintercept =  seq(47, 51, 1), color = "white", linewidth = 0.3) +
  geom_hline(yintercept =  seq(44, 53, 1), color = "white", alpha = 0.9,
             linewidth = seq(1, 0.1, -0.1)) + # linewidth = seq(0.9, 0, -0.1)
  geom_area(fill = "#B31942") +
  geom_line(color = "white", linewidth = 1) +
  # geom_hline(yintercept = 40, color = "white") + # c(40, 45, 50, 55) linetype = "dashed",  , alpha = 0.3
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(45, 50),
                     labels = function(x) paste0(x, " %")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1890, 2010, 20)) +
  coord_cartesian(ylim = c(38, 70)) + # coord_cartesian(ylim = c(40, 55)) + coord_cartesian(ylim = c(40, 70)) +
  theme(
    text = element_text(colour = "white", family = "Poppins"),
    plot.background = element_rect(fill = "#0A3161", color = NA),
    panel.background = element_rect(fill = "#0A3161", color = NA),
    # plot.margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "cm"), #margin(rep(1, 4)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(colour = "white",size = 12),
    axis.text.y = element_text(vjust = -0.7, # -1
                               margin = margin(l = 15, r = -45)),  
    axis.text.x = element_text(margin = margin(t = -20, b = 5))
  )

p1


# Add textbox for description

left_align <- 1885

text_caption <- tibble(
  year = c(left_align),
  y = c(68),
  text = "Viz by **Giulia Puntin**<br>Data from SSA.gov <br>#TidyTuesday 2022_12" # www.ssa.gov/oact/babynames
)

text_title <- tibble(
  year = c(left_align),
  y = c(64.5), # c(67.5),
  text = c(
    # "American <span style='font-family:Gloria Hallelujah'>baby names</span> and the <span style='font-family:Special Elite'>QWERTY effect</span>:<br> is tech steering naming preferences?"
    "<span style='font-size:18pt'>American baby names and the QWERTY effect:<br> is tech steering naming preferences?</span><br><br>
    "
    # "American <span style='font-family:gloria_halleluja'>baby names</span> and the <span style='font-family:special_elite'>QWERTY effect</span>: is tech steering naming preferences?"
    )
)

text_descr <- tibble(
  year = c(left_align),
  y = c(59), # 62 59.5
  text = c(
    "The **‘QWERTY effect’** describes a phenomenon where words containing more letters from the right side of the keyboard are perceived more positively (Jasmin and Casasanto 2012).<br><br> 
    This influence seems to extend to the selection of baby names in the U.S., with names rich in right key letters gaining popularity since the 1990s, coinciding with the widespread adoption of QWERTY keyboards as computers and the internet became household staples (Casasanto et al. 2014).<br><br>
This plot shows the **average percentage** of **<span style='color:#B31942'>right key</span>** letters in names given to **newborns** in the **U.S.** each year **from 1880 to 2017**.<br><br>
Essentially, the plotted line (%) goes up as names containing a higher percentage of right key letters rise in popularity.
"
  )
)



p1 +
  annotation_custom(raster, x = 1965, xmax = 2010, y = 67, ymax = 69) + # x = 1950, xmax = 2010, y = 69, ymax = 66
  ggtext::geom_textbox(
    data = text_caption,
    mapping = aes(x = year,
                  y = y,
                  label = text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    width = unit(7.5, "cm"),
    family = "Poppins", # works
    fill = NA,
    box.color = NA,
    color = "white"
  ) +
  ggtext::geom_textbox(
    data = text_title,
    mapping = aes(x = year,
                  y = y,
                  label = text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 5,
    width = unit(15, "cm"),
    family = "Special Elite", # family = "Poppins", # works     # family = "Gloria Hallelujah", # works
    fill = NA,
    box.color = NA,
    color = "white" # text color
  ) +
  ggtext::geom_textbox(
    data = text_descr,
    mapping = aes(x = year,
                  y = y,
                  label = text),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4,
    width = unit(15, "cm"),
    family = "Poppins", # works
    fill = NA,
    box.color = NA,
    color = "white" # text color
  ) +
  geom_textbox(data = data.frame(x = 1952, 
                                 y = 42.5160724790146, 
                              label = "In **1990** computers and internt start to become common in household"),
            mapping = aes(x = x, y = y, label = label),
            family = "Poppins", # "Cutive"
            fill = NA,
            box.color = NA,
            color = "white",
            width = unit(9, "cm"),
            valign = 1,
            # hjust = 0.5,
            inherit.aes = FALSE) +
  geom_curve(data = data.frame(x = 1980, y = 42.5160724790146, xend = 1990, yend = 44.2951979644509),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             angle = 90,
             color = "white",
             arrow = arrow(23L, unit(0.1, "inches"), "last", "closed"),
             inherit.aes = FALSE)


# Print png file
ggsave("./USbabynames.png",
       dpi = 330,
       units = "cm", 
       width = 17, height = 24 # A4 format 21 x 29.7
)
