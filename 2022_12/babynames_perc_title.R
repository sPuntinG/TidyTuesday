
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


# # Right and left keys
# leftk <- c("Q", "W", "E", "R", "T", "A", "S", "D", "F", "G", "Z", "X", "C", "V", "B")
# rightk1 <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")
# rightk2 <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M", "B") # includes B

babynames$name %>% n_distinct() # 97'310 (!!!)
names10 <- babynames$name %>% unique() %>% .[1:10] # small set for now as it's a huge data set ... !


# Count right key letters in each name ---------------------------------

# # str_count() works on single word
# stringr::str_count(names10[1], fixed(rightk1, ignore_case = T)) # 1 0 0 0 0 0 0 0 0 0 1   this can simply be summed up with sum() = 2
# stringr::str_count(names10[1], fixed(rightk1, ignore_case = T)) %>% sum() # 2 which is what I need in the end
# 
# # But does not work on vectors
# stringr::str_count(names10[1:2], fixed(rightk1, ignore_case = T)) # Error in `stringr::str_count()`:
#                                                                  # ! Can't recycle `string` (size 2) to match `pattern` (size 11).

# Make function to iterate
count_right1 <- function(x) {
  rightk1 <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")
  a <- stringr::str_count(x, fixed(rightk1, ignore_case = T))
  a <- sum(a)
  return(a)
}

count_right2 <- function(x) {
  rightk2 <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M", "B")
  # rightk <- paste(rightk, collapse = '|')
  a <- stringr::str_count(x, fixed(rightk2, ignore_case = T))
  a <- sum(a)
  return(a)
}

count_right1("mario") # 3
# count_right1(names10) # as expected doesn't work on vectorized argument


# Use map_dbl to loop count right function over vector of names -----------------------

# map_dbl(names10, count_right1) # so that the output is a numeric vector 
# 
# tibble10 <- tibble(name = names10, 
#                    tot_letters = nchar(name),
#                    right_letters = map_dbl(names10, count_right1),
#                    left_letters = tot_letters - right_letters,
#                    ratio = right_letters/left_letters,
#                    predominance = case_when(
#                      ratio == 1 ~ "none",
#                      ratio > 1 ~ "right",
#                      ratio < 1 ~ "left"
#                      )
#                    )


# Apply to full list of names 

all_names <- babynames$name %>% unique()

tibble_all <- tibble(name = all_names, 
                     tot_letters = nchar(name),
                     right_letters1 = map_dbl(all_names, count_right1),
                     right_letters2 = map_dbl(all_names, count_right2),
                     left_letters1 = tot_letters - right_letters1,
                     left_letters2 = tot_letters - right_letters2,
                     ratio1 = right_letters1/left_letters1,
                     ratio2 = right_letters2/left_letters2,
                     predominance1 = case_when(
                       ratio1 == 1 ~ "none",
                       ratio1 > 1 ~ "right",
                       ratio1 < 1 ~ "left"
                     ),
                     predominance2 = case_when(
                       ratio2 == 1 ~ "none",
                       ratio2 > 1 ~ "right",
                       ratio2 < 1 ~ "left"
                     ),
                     perc_right1 = right_letters1/tot_letters * 100,
                     perc_right2 = right_letters2/tot_letters * 100
)
   

# Join to babynames 
babynames_rl <- left_join(babynames, tibble_all, by = "name")


# Color palette ------------------

# USA flag
"#B31942" 
"#FFFFFF"
"#0A3161"

# midcolor
"#5F2552"

# Lighter versions
"#F7D0EF"
"#F7EBF5"
"#FFDEF9"


# Fonts ------------------
windowsFonts()

# install.packages("extrafont")
library(extrafont)
# font_import() # to run only when new fonts added to the system
loadfonts(device = "win")

windowsFonts()


# Plot: weighted mean % of right vs left key letters in baby names -----------------------------

# Now plot average percentage of right key letters in names (per year)
# NOTE: old version was not normalized for the frequency of the names! 
#       perc_right should be multiplied by n and then averaged (divided by total n)

library(ggtext) # for element_markdown()

# New version with normalization by n
babynames_rl_toplot <- babynames_rl %>% 
  # filter(year > 1980) %>%
  group_by(year) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  mutate(
    perc_right1_n = perc_right1 * n
  ) %>% 
  # select(year, n, n_tot, perc_right1, perc_right1_n) %>% #view() name, 
  group_by(year) %>%
  reframe(perc_right1_n_mean = sum(perc_right1_n) / n_tot) %>% #view()
  unique()


plot1 <- ggplot(babynames_rl_toplot, aes(x = year, y = perc_right1_n_mean)) +
  geom_hline(yintercept = c(45, 50), color = "white", linewidth = 1.25) +
  # geom_hline(yintercept =  seq(35, 47, 1), color = "white", linewidth = 0.7) +
  # geom_hline(yintercept =  seq(47, 51, 1), color = "white", linewidth = 0.3) +
  geom_hline(yintercept =  seq(44, 53, 1), color = "white", alpha = 0.9,
             linewidth = seq(0.9, 0, -0.1)) +
  geom_area(fill = "#B31942") +
  geom_line(color = "white", linewidth = 1) +
  # geom_hline(yintercept = 40, color = "white") + # c(40, 45, 50, 55) linetype = "dashed",  , alpha = 0.3
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(45, 50),
                     labels = function(x) paste0(x, " %")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1890, 2010, 20)) +
  coord_cartesian(ylim = c(40, 55)) -> plot1


# Figure out if annotate() or geom_text() better for title and subtitle etc (which one easier?)
# Then use {ggannotate} for arrows
plot1 + 
  labs(title = "This is a text in <span style='font-family: Cutive; color: red'>QWERTY effect</span> ",
       subtitle = "write the rest of the text here ...") +
  # geom_text(data = data.frame(x = 1889.54553535584, y = 67.9509789002066, label = "The 'QWERTY effect' on U.S. baby names"),
  #           mapping = aes(x = x, y = y, label = label),
  #           family = "Abril Fatface",
  #           # fontface = 2, 
  #           size = 4.59, hjust = 0L, colour = "white", inherit.aes = FALSE) + # 
  geom_text(data = data.frame(x = 1930, y = 42.5160724790146, label = "In 1990 computers start to become popular: 
they are found in ~19% of households"),
             mapping = aes(x = x, y = y, label = label),
             family = "Poppins", # "Cutive"
             color = "white",
             hjust = 0.5,
             inherit.aes = FALSE) +
  geom_curve(data = data.frame(x = 1980, y = 42.5160724790146, xend = 1990, yend = 44.2951979644509),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             angle = 90,
             color = "white",
             arrow = arrow(23L, unit(0.1, "inches"),
                           "last", "closed"),
             inherit.aes = FALSE) +
  theme(
    plot.title = element_markdown(vjust = 50, hjust = 0.1, family = "Poppins", colour = "white"), 
    plot.subtitle = element_markdown(vjust = 40, hjust = 0.1, family = "Poppins", colour = "white"),
    # plot.title.position = "plot", # cambia n'czz
    plot.background = element_rect(fill = "#0A3161", color = NA),
    panel.background = element_rect(fill = "#0A3161", color = NA),
    plot.margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "cm"), #margin(rep(1, 4)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(colour = "white",
                             family = "Poppins",
                             size = 12),
    axis.text.y = element_text(vjust = -1,
                               margin = margin(l = 15, r = -45)),  # -40 aligns with 1890
    axis.text.x = element_text(margin = margin(t = -20, b = 5))
  )



# Annotate with {gganotate} ---------------------------------

# Install package
# remotes::install_github("mattcowgill/ggannotate")
  
# library(ggannotate)
# 
# ggannotate::ggannotate()


# Print png file
ggsave("./try3.png",
       dpi = 330,
       units = "cm", 
       width = 17, height = 24 # A4 format 21 x 29.7
       )

# Add text and arrows following THIS AMAZING blog: http://jenrichmond.rbind.io/post/idhtg-how-to-annotate-plots/#:~:text=Add%20text%20to%20a%20ggplot,want%20the%20label%20to%20say.&text=To%20add%20arrows%2C%20first%20make,line%20to%20start%20and%20stop.
# Use {ggannotate} to add arrows and text with a Shiny interface that generates the code for you !!

# Text:
# " Average right-key letter content is relatively stable for 100 years (1880 - 1980)
#  and then shows a steady increase from 199o which seems to stabilize after 2010 (data available until 2017)."
#  
# Add TAG of text label:
# "1980: < 3 % ...
# "1990: about 16% of American households have a computer" arrow to 1990 
# "2010: > 76.7 %"
# "2017: >90% ... "
# "US Census Bureau’s Current Population Survey (CPS) and American Community Survey (ACS)"


# https://www.census.gov/data/tables/2012/demo/computer-internet/computer-use-2012.html for data on households with computer







