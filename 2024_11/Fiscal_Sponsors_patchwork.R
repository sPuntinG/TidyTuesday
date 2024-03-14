
#------------- R version 4.3.2 (2023-10-31 ucrt) --------------------

# Load pcks -----------------------------------------

library(tidyverse)
library(here)
library(ggtext)
library(extrafont)
library(waffle)
# library(treemapify)

packageVersion("ggplot2") # ‘3.4.4’ -> Need to install the new version!
# install.packages("ggplot2") # now ‘3.5.0’
# install.packages("waffle") # 4.3.3 

# Import data from GitHub --------------------------

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')


# Clean & add useful vars ----------------------

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



# 1. Waffle --------------------------------

data_waffle <- data %>% 
  group_by(project_types_Education) %>% 
  summarise(n = n_distinct(name))

FS_tot <- sum(data_waffle$n)
FS_edu <- data_waffle %>% filter(project_types_Education == "Education") %>% pull(n)
FS_other <- FS_tot - FS_edu


waffle <- ggplot(data_waffle, 
                 aes(fill = project_types_Education,
                     values = n)) +
  geom_waffle(make_proportional = F,
              flip = T,
              color = lavagna,
              n_rows = 15,
              size = 0.5) + 
  scale_fill_manual(values = palette_FS) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_reverse() +
  labs(title = "How many possible Fiscal Sponsors?",
       subtitle = glue::glue("There are currently {FS_tot} FS, of which <span style='color:#F5A413'>{FS_edu}</span><br>
                             sponsor projects in <span style='color:#F5A413'>education</span>, and<br>
                             <span style='color:#15AEF3'>{FS_other}</span> in <span style='color:#15AEF3'>other</span> sectors." )) + 
  coord_fixed(ratio = 1) +
  theme(
    plot.title = element_markdown(size = 10),
    plot.subtitle = element_markdown(size = 8),
    plot.title.position = "plot",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) 
waffle



# 2: Histogram nr of projects per FS --------------------

hist_projx <- ggplot(data, 
                     aes(x = n_sponsored, fill = fct_rev(project_types_Education))) +
  geom_density(alpha = 0.75,
               color = "white",
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
  geom_vline(data = medians_projx,
             aes(xintercept = median_value), #, color = project_types_Education),
             color = "white",
             linewidth = 0.5) +
  scale_color_manual(values = palette_FS) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Are there major players?",
       subtitle = glue::glue("Most FS sponsor a small number of projects. Indeed, half of them has sponsorer less than <span style='color:#F5A413'>{median_edu}</span><br>
                             or <span style='color:#15AEF3'>{median_other}</span> projects so far (median). However, there are some very big outliers, with a few<br>
                             individuals FS having sponsored 100s of projects."),
       x = "no. of projects sponsored") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 10),
    plot.subtitle = element_markdown(size = 8),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(colour = "white"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = colorspace::darken("white", 0.25),
                                      linetype = "dashed")) +
  ggtext::geom_textbox(
    data = data.frame(x = 150, 
                      y = 0.027, 
                      project_types_Education = factor("Education", levels = levels(data$project_types_Education)),
                      label = "**Most organizations** sponsored a **small number of projects**<br>
                      This is applies to <span style='color:#F5A413'>education</span>, as well as for <span style='color:#15AEF3'>other</span> projects"),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    width = unit(9, "cm"),
    family = "Ink Free", # works
    fill = NA,
    box.color = NA,
    color = "white") +
  geom_curve(data = data.frame(x = 150, 
                               y = 0.027,
                               xend = 5,
                               yend = 0.029,
                               project_types_Education = factor("Education", levels = levels(data$project_types_Education))),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             angle = 90,
             size = 0.2,
             color = "white",
             arrow = arrow(23L, unit(0.05, "inches"), "last", "closed"),
             inherit.aes = FALSE) +
  ggtext::geom_textbox(
    data = data.frame(x = 400, 
                      y = 0.025, 
                      project_types_Education = factor("other", levels = levels(data$project_types_Education)),
                      label = "Some very extreme values here ... !"),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    width = unit(7.5, "cm"),
    family = "Ink Free", # works
    fill = NA,
    box.color = NA,
    color = "white") +
  geom_curve(data = data.frame(x = 740, 
                               y = 0.024,
                               xend = 845,
                               yend = 0.005,
                               project_types_Education = factor("other", levels = levels(data$project_types_Education))),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             angle = 90,
             curvature = -0.25,
             size = 0.2,
             color = "white",
             arrow = arrow(23L, unit(0.05, "inches"), "last", "closed"),
             inherit.aes = FALSE) 
hist_projx_median



# 3: scatter plot ------------------------

scatter <- ggplot(data) +
  geom_point(aes(x = years_exist, y = n_sponsored, fill = project_types_Education),
             shape = 21,
             size = 3,
             alpha = 0.7,
             color = lavagna,
             show.legend = F) + # show.legend = F
  scale_fill_manual(values = palette_FS) +
  labs(title = "Should you choose a younger or an older sponsors?",
       subtitle = "When exploring the relationship between number of projects sponsored and age (since establishment as 501(c)(3) charitable organization),<br>
       we don't see a clear pattern",
       x = "Years since existance",
       y = "No. of projects sponsored") +
  coord_cartesian(ylim = c(0, 900), xlim = c(0, 150))  +
  theme(
    # panel.background = element_rect(fill = "grey23"),
    plot.title = element_markdown(size = 10),
    plot.subtitle = element_markdown(size = 8),
    plot.title.position = "plot",
    axis.ticks = element_blank(),
    axis.text = element_text(colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = colorspace::darken("white", 0.25),
                                      linetype = "dashed")
  )
scatter

n_max <- max(data$n_sponsored, na.rm = T)
year_max <- max(data$years_exist, na.rm = T)

scatter <-  scatter +
  ggtext::geom_textbox(
    data = data.frame(x = 45, 
                      y = 830, 
                      label = glue::glue("One organization sponsered {n_max} projects alone")),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    width = unit(7.5, "cm"),
    family = "Ink Free", # works
    fill = NA,
    box.color = NA,
    color = "white") +
  geom_curve(data = data.frame(x = 45, 
                               y = 830, 
                               xend = 36, 
                               yend = n_max + 5),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             angle = 90,
             size = 0.2,
             curvature = 0.5,
             color = "white",
             arrow = arrow(23L, unit(0.05, "inches"), "last", "closed"),
             inherit.aes = FALSE) +
  ggtext::geom_textbox(
    data = data.frame(x = 95, 
                      y = 355, 
                      label = glue::glue("The oldest organization is {year_max} years old<br>and it works with education projects")),
    mapping = aes(x = x,
                  y = y,
                  label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    width = unit(7.5, "cm"),
    family = "Ink Free", # works
    fill = NA,
    box.color = NA,
    color = "white") +
    geom_curve(data = data.frame(x = 110, 
                                 y = 300, 
                                 xend = year_max - 3, 
                                 yend = 37),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 90,
               size = 0.2,
               color = "white",
               arrow = arrow(23L, unit(0.05, "inches"), "last", "closed"),
               inherit.aes = FALSE)
scatter



# Patchwork -------------------------------
library(patchwork)

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
  title = "Fiscal Sponsorship Opportunities in **<span style='color:#F5A413'>Education</span>**",
  subtitle = "A fiscal sponsor is a nonprofit organization that teams up with community projects lacking tax-exempt<br>
  status, managing their funds to support charitable endeavors while ensuring compliance with tax laws.<br>
  This arrangement allows donors to make tax-deductible contributions to the project through the fiscal sponsor.<br><br>
  The R4DS Online Learning community is curretnly seeking a new fiscal sponsor.<br>
To aid in the search, let's have a look at organizations that sponsor projects in the field of <span style='color:#F5A413'>education</span>.",
  caption = "#TidyTuesday 2024 Week 11 | Source: Fiscal Sponsor Directory | Data Viz: Giulia Puntin",
  theme = theme(plot.title = element_markdown(size = 19, family = "Ink Free"),
                plot.title.position = "panel",
                plot.subtitle = element_markdown(size = 11))
  ) &
  theme(
    text = element_text(colour = "white", family = "Ink Free"),
    plot.background = element_rect(fill = lavagna, color = NA),
    panel.background = element_blank(), # element_rect(fill = "#0A3161", color = NA),
    plot.caption = element_text(size = 7), # vjust = -5, 
    plot.margin = margin(rep(0.5, 4), unit = "cm"),
    panel.grid.minor = element_blank()
  )



# Export figure -----------------------------------

# Make pdf so it preserves fonts and all
ggsave(filename = "./Fiscal_Sponsors_combo.pdf", #p4,
       width = 8, height = 9, device = cairo_pdf)

# Convert pdf to png
pdftools::pdf_convert(pdf = "./Fiscal_Sponsors_combo.pdf",
                      filenames = "./Fiscal_Sponsors_combo.png",
                      format = "png", dpi = 400)

