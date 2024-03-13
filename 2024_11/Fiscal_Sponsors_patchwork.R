
#------------- R version 4.3.2 (2023-10-31 ucrt) --------------------

# Load pcks -----------------------------------------

library(tidyverse)
library(here)
library(extrafont)
library(waffle)
# library(treemapify)

packageVersion("ggplot2") # ‘3.4.4’ -> Need to install the new version!
# install.packages("ggplot2") # now ‘3.5.0’
# install.packages("waffle") # 4.3.3 

# Import data from GitHub --------------------------

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')


# Add useful vars ----------------------

# Add col for years since start (now - year_501c3)
data <- data %>% 
  mutate(years_exist = 2023 - year_501c3) #%>% view()

# Focus on Education: add column for filtering
data <- data %>% 
  mutate(project_types = ifelse(is.na(project_types), "other", project_types)) %>% 
  mutate(project_types_Education = if_else(stringr::str_detect(project_types, "(?i)education"), "Education", "other")) %>% 
  relocate(project_types_Education, .after = project_types) #%>% view()


# Remove duplicated values
data$name %>% length()     # 370
data$name %>% n_distinct() # 368

data %>% 
  filter(duplicated(name)) %>% pull(name) #view()

data <- data %>% distinct() 

data %>% pull(name) %>% n_distinct() # 368



# Check where info is missing
data %>%
  filter(if_any(everything(), is.na)) #%>% view()

# Order levels for Education/other
data$project_types_Education <-  factor(data$project_types_Education, 
                                        levels = c("Education", "other"),
                                        ordered = T)
class(data$project_types_Education)
str(data$project_types_Education)

# Idea: make patchwork all about Education vs others with: 
# 1. stacked bar plot of FS over the years
# 2. doughnut pie of proportion of FS that are about Education (with tot or year in the hole)
# 3. scatter plot
# 4. treemap

# Programmatic plotting -------------------------

## Color palette ---------------

color_edu <-  "#F5A413"
color_oth <- "#15AEF3"

palette_FS <- c("Education" = color_edu, "other" = color_oth)

# To play around with darkening or lightning
lavagna <- colorspace::darken("#dbdbdb", 0.8)

color_bkg <- "#2B2B2B" # colorspace::darken("#dbdbdb", 0.8)
color_txt <- "white"


## Fonts --------------------
windowsFonts() %>% unlist()

# font_import() # to run only when new fonts added to the system?
extrafont::loadfonts(device = "win")

# extrafont::fonttable() %>% view()
# windowsFonts()



## Set theme -----------------

# 1. Waffle --------------------------------

data_waffle <- data %>% 
  group_by(project_types_Education) %>% 
  summarise(n = n_distinct(name))

FS_tot <- sum(data_waffle$n)
FS_edu <- data_waffle %>% filter(project_types_Education == "Education") %>% pull(n)

waffle <- ggplot(data_waffle, aes(fill = fct_rev(project_types_Education),
                        values = n)) +
  geom_waffle(make_proportional = F,
              flip = T,
              n_rows = 25,
              size = 0.5) + 
  scale_fill_manual(values = palette_FS) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "How many possible FS?",
       subtitle = glue::glue('There are currently {FS_tot} FS, of which {FS_edu} sponsor projects in EDUCATION')) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
waffle


# 2: Scatter -------------------------------

# Scatter plot basic
scatter <- ggplot(data) +
  geom_smooth(aes(x = years_exist, y = n_sponsored),
              method = "lm",
              formula = 'y ~ x',
              color = "white",
              linewidth = .1,
              se = F  ) +
  geom_point(aes(x = years_exist, y = n_sponsored, fill = project_types_Education),
             shape = 21,
             show.legend = F) + # show.legend = F
  scale_fill_manual(values = palette_FS) +
  labs(title = "Age (of FSs) is relative",
       subtitle = "Do older organizations sponsor more projects? Not necessarily ...",
       x = "Years since existance",
       y = "No. of projects sponsored") +
  coord_cartesian(ylim = c(0, 900), xlim = c(0, 150)) 
scatter





# 3: Histogram nr of projects per FS --------------------

hist_projx <- ggplot(data, aes(x = n_sponsored, fill = fct_rev(project_types_Education))) +
  geom_density(alpha = 0.5,
               adjust = 1/3) + 
  scale_fill_manual(values = palette_FS) +
  facet_wrap(~project_types_Education, ncol = 1)

hist_projx


medians_projx <- data %>%
  group_by(project_types_Education) %>%
  summarise(median_value = median(n_sponsored, na.rm = T))

median_edu <- medians_projx %>% filter(project_types_Education == "Education") %>% pull(median_value)
median_other <- medians_projx %>% filter(project_types_Education == "other") %>% pull(median_value)

hist_projx_median <- hist_projx + 
  geom_vline(data = medians_projx, aes(xintercept = median_value, color = project_types_Education)) +
  scale_color_manual(values = palette_FS) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Are there major players?",
       subtitle = glue::glue("Most FS sponsor a small number of projects. Indeed, half of them has sponsorer less than {median_edu} or {median_other} projects so far (median). However, there are some very big outliers, with a few individuals FS having sponsored 100s of projects."),
       x = "no. of projects sponsored") +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = colorspace::darken("white", 0.25))
  )
hist_projx_median

# old 1. Stacked bar ---------------------------

# Use cumsum individually and then join tibbles (Education; Other)

data %>% 
  filter(is.na(year_501c3)) # why 6 have no data except details_url??


edu <- data %>% 
  select(name, year_501c3, project_types_Education) %>% 
  filter(project_types_Education == "Education") %>% 
  complete(., year_501c3 = c(1900:2023))

other <- data %>% 
  select(name, year_501c3, project_types_Education) %>% 
  filter(project_types_Education == "other") %>% 
  complete(., year_501c3 = c(1900:2023))
  
edu <- edu %>% 
  group_by(year_501c3) %>% 
  summarise(count = n())  %>% 
  mutate(Education = cumsum(count)) %>% 
  select(-count)
  
other <- other %>% 
  group_by(year_501c3) %>% 
  summarise(count = n())  %>% 
  mutate(other = cumsum(count)) %>% 
  select(-count)

data_bar <- full_join(edu, other, by = "year_501c3") %>% 
  pivot_longer(
    cols = -year_501c3,
    names_to = "type",
    values_to = "n"
  )

data_bar$type <-  factor(data_bar$type, 
                         levels = c("Education", "other"),
                         ordered = T)



bar <- ggplot(data_bar, aes(x = year_501c3, y = n, fill = type)) + 
  geom_bar(stat = "identity") +
  # coord_cartesian(xlim = c(1950, 2024)) +
  scale_fill_manual(values = palette_FS) +
  labs(title = "Number of FS per year") +
  theme(
    legend.position = "none"
  ) 
bar



# old 2: doughnut plot ------------------------

data$name %>% length()     # 370
data$name %>% n_distinct() # 368

# Looks like there are duplicated entries
data %>% 
  filter(duplicated(name)) %>% pull(name) #view()

data %>% 
  group_by(project_types_Education) %>% 
  summarise(n = n()) #%>% view()

data %>% 
  filter(name == "Side Project Inc.")

# Counts to add as text
count_edu <- data %>% 
  filter(project_types_Education == "Education") %>% 
  summarise(n = n()) %>% pull(n)

count_oth <- data %>% 
  filter(project_types_Education == "other") %>% 
  summarise(n = n()) %>% pull(n)

count_tot <- data$name %>% length()


# Donut
donut <- data %>% 
  filter(!is.na(year_501c3)) %>% 
  ggplot(., aes(y = factor(1), fill = factor(project_types_Education))) +
  geom_bar(width = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = palette_FS) +
  geom_text(label = count_edu, x = count_edu/2, y = 1, vjust = -3) +
  geom_text(label = count_oth, x = (count_tot - count_edu) + count_oth/2, y = 1) + 
  geom_text(label = "numbers identity now wrong, redo!", 
            color = "red",
            x = 1, y = 1, vjust = -3) +
  # labs(title = "check the factor order and counts!") +
  coord_radial(inner.radius = 0.5, 
               direction = 1,
               # clip = "off", # doesn't do shit
               # start = 1 * pi, #end = 0.5 * pi,
               expand = F # to close circle
        ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 
donut

# How to add number in the middle?

# To check "old school" approach here: https://r-graph-gallery.com/128-ring-or-donut-plot.html





# 3: scatter plot ------------------------

## TO BE FINISHED: Test correlation between years and nr of projects ------------------------------
lm(data = data, formula = n_sponsored ~ years_exist)

cor.test(data$years_exist, data$n_sponsored, method = "pearson") # c("pearson", "kendall", "spearman")


## Scatter plot basic ----------------------------
scatter <- ggplot(data) +
  geom_smooth(aes(x = years_exist, y = n_sponsored),
              method = "lm",
              formula = 'y ~ x',
              color = "white",
              linewidth = .1,
              se = F  ) +
  geom_point(aes(x = years_exist, y = n_sponsored, fill = project_types_Education),
             shape = 21,
             show.legend = F) + # show.legend = F
  scale_fill_manual(values = palette_FS) +
  labs(title = "Should you choose and younger or an older sponsors?",
       subtitle = "When exploring the relationship between number of projects sponsored and age (since establishment as 501(c)(3) charitable organization), we don't see a clear pattern",
       x = "Years since existance",
       y = "No. of projects sponsored") +
  coord_cartesian(ylim = c(0, 900), xlim = c(0, 150)) 
scatter

## Scatter plot pretty ----------------------------
ggplot(data) +
  geom_smooth(aes(x = years_exist, y = n_sponsored),
              method = "lm",
              formula = 'y ~ x',
              color = "white",
              linewidth = .1,
              se = F
              # inherit.aes = F,
  ) +
  geom_point(aes(x = years_exist, y = n_sponsored, fill = project_types_Education),
             shape = 21, 
             color = colorspace::darken("#dbdbdb", 0.75), 
             alpha = 0.7,
             size = 3,
             show.legend = F) + # show.legend = F
  # geom_point(shape = 21, 
  #            color = NA, 
  #            # alpha = 0.75,
  #            size = 2,
  #            show.legend = F)
  scale_x_continuous(expand = c(0,0), breaks = seq(25, 125, 25)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(250, 750, 250)) +
  scale_fill_manual(values = palette_FS) +
  # scale_fill_manual(breaks = c("other", "Education"), values = c("#E8B723", "#006DBA")) + # #2F94A8 # 
  labs(title = "Should you choose and younger or an older sponsors?",
       subtitle = "When exploring the relationship between number of projects sponsored and age (since establishment as 501(c)(3) charitable organization), we don't see a clear pattern",
       caption = "#TidyTuesday 2024 Week 11 | Source: Fiscal Sponsor Directory | Data Viz: Giulia Puntin",
       x = "Years since existance",
       y = "No. of projects sponsored") +
  coord_cartesian(ylim = c(0, 900), xlim = c(0, 150)) +
  theme(
    text = element_text(colour = "white", # colorspace::darken("#dbdbdb", 0.25),
                        family = "Ink Free"),
    # plot.title = element_text(family = "Abril Fatface"),
    plot.title = element_text(face = "bold"),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, 
                             # margin = margin(t = 10, r = 10),
                             colour = colorspace::darken("#dbdbdb", 0.25)), # #F9F4E1
    axis.title = element_text(size = 13),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", color = colorspace::darken("#dbdbdb", 0.5)),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = colorspace::darken("#dbdbdb", 0.8), color = NA),
    panel.background = element_blank(), # element_rect(fill = "#0A3161", color = NA),
    plot.caption = element_text(vjust = -5, size = 7),
    # panel.margin = margin(rep(0.5, 4), unit = "cm"),
    plot.margin = margin(rep(0.5, 4), unit = "cm")
  )



# 4: tree map ---------------------

# install.packages("treemapify")
# library(treemapify)

# # Plotting all names is messy: let's keep only the ones with more projects
# data <- data %>% 
#   # filter(n_sponsored < 50) %>% 
#   filter(!is.na(n_sponsored)) %>% 
#   mutate(name_big = if_else(n_sponsored < 40, "", name)) #%>% view()

# Treemap plots!
tree <- ggplot(data, aes(area = n_sponsored, 
                         fill = project_types_Education,
                         group = project_types_Education,
                         label = name)) +
  scale_fill_manual(values = palette_FS) +
  # facet_wrap( ~ project_types_Education) +
  treemapify::geom_treemap(start = "topleft", 
                           layout = 'srow',
                           alpha = 0.7,
                           color = lavagna) + # show.legend = F, layout = 'squarified' (the default), 'scol', 'srow' or 'fixed'
  treemapify::geom_treemap_text(colour = "white",
                                place = "centre", 
                                reflow = T,
                                start = "topleft",
                                layout = 'srow') +
  theme(
    legend.position = "none"
  )

tree






# Conclusions: 
# 1. Most FS sponsor a small number of project (in both cases)
#    with some very large outliers 
# 2. similar distribution of sponsored projects (median: almost same)


# 6: Histogram: age of FS faceted --------------------

p <- ggplot(data, aes(x = years_exist, fill = fct_rev(project_types_Education))) +
  geom_density(alpha = 0.5,
               adjust = 1/3) + #position = "stack"
  scale_fill_manual(values = palette_FS) +
  # geom_vline(xintercept = median_all) +
  facet_wrap(~project_types_Education, ncol = 1)
# geom_histogram(binwidth = 5)
p

medians <- data %>%
  group_by(project_types_Education) %>%
  summarise(median_value = median(years_exist))

p + geom_vline(data = medians, aes(xintercept = median_value, color = project_types_Education)) +
  scale_color_manual(values = palette_FS)


# median_all <- median(data$years_exist, rm.na = T)
# 
# ggplot(data, aes(x = years_exist)) +
#   geom_density(#alpha = 0.5,
#                adjust = 1/3) +
#   geom_vline(xintercept = median_all)



# Patchwork -------------------------------
library(patchwork)

# combined_plot <- (waffle + hist_projx_median) / (scatter) +
#   plot_layout(widths = c(1, 2), heights = c(1, 1))
# combined_plot


# Manually define areas
layout <- '
ABBB
ABBB
CCCC
CCCC
'
combo <- wrap_plots(A = waffle, B = hist_projx_median, C = scatter,
           design = layout)
combo

combo + plot_annotation(
  title = 'Fiscal Sponsorship Opportunities in Education',
  subtitle = 'A fiscal sponsor is a nonprofit organization that teams up with community projects lacking tax-exempt status, managing their funds to support charitable endeavors while ensuring compliance with tax laws. <br>This arrangement allows donors to make tax-deductible contributions to the project through the fiscal sponsor.',
  caption = "#TidyTuesday 2024 Week 11 | Source: Fiscal Sponsor Directory | Data Viz: Giulia Puntin",
  theme = theme(plot.title = element_text(size = 18, family = "Ink Free"))
  ) &
  theme(
    text = element_text(colour = "white", family = "Ink Free"),
    plot.title = element_text(face = "bold", family = "Ink Free"),
    plot.background = element_rect(fill = lavagna, color = NA),
    panel.background = element_blank(), # element_rect(fill = "#0A3161", color = NA),
    plot.caption = element_text(vjust = -5, size = 7),
    plot.margin = margin(rep(0.5, 4), unit = "cm"),
    panel.grid.minor = element_blank()
  )



## Export .PNG -----------------------------------

# Find way to export that preserves FONT!!
# Now png has wrong font in the title!

ggsave("./Fiscal_Sponsors_combo.png",
       dpi = 330,
       units = "cm", 
       width = 30, height = 30)









# # This combo doesn't work
# combo_tree <- scatter + tree
#   # plot_layout(widths = c(3, 1))
# combo_tree

combined_plot3 <- (hist_projx_median + donut) / (scatter) + # here tree works!
  plot_layout(widths = c(1, 3), heights = c(3, 2))
combined_plot3



combined_plot2 <- donut + 
  bar +
  plot_layout(widths = c(1, 2), heights = c(2, 1))
combined_plot2


# Add insert
#  see here for logic: https://patchwork.data-imaginist.com/reference/area.html
layout <- c(
  # area(t = 2, l = 1, b = 5, r = 4),
  # area(t = 1, l = 3, b = 3, r = 5)
  area(1,1,3,3),
  area(1,3,1,3)
  )
a <- hist_projx_median + donut + 
  plot_layout(design = layout)

a / scatter

# Manually define areas
layout <- '
AAAB
AAA#
'
wrap_plots(A = scatter, B = donut, design = layout, )




# Simple plot: projects sponsored as function of time ----------------------------




# To do:
# - do proper correlation test
# - add results of corr test to plot
# - Write subtitle with colors to indicate legend
# - Add annotation to point out outliers
# - find better title and caption
# - Change font



## Export .PNG -----------------------------------
ggsave("./Fiscal_Sponsors.png",
       dpi = 330,
       units = "cm", 
       width = 17, height = 15)



# text = element_text(colour = "white", family = "Poppins"),
# plot.background = element_rect(fill = "#0A3161", color = NA),
# panel.background = element_rect(fill = "#0A3161", color = NA),
# panel.grid.major = element_blank(), 
# panel.grid.minor = element_blank(),
# axis.ticks = element_blank(),
# axis.title = element_blank(),
# axis.text = element_text(colour = "white",size = 12),
# axis.text.y = element_text(vjust = -0.7, # -1
#                            margin = margin(l = 15, r = -45)),  
# axis.text.x = element_text(margin = margin(t = -20, b = 5))



