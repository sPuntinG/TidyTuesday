
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

babynames$name %>% unique() %>% n_distinct() # 97'310 (!!!)
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



# Plot: weighted mean % of right vs left key letters in baby names -----------------------------

# Now plot average percentage of right key letters in names (per year)
# NOTE: old version was not normalized for the frequency of the names! 
#       perc_right should be multiplied by n and then averaged (divided by total n)


# New version with normalization by n
babynames_rl %>% 
  # filter(year > 1980) %>%
  group_by(year) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  mutate(
    perc_right1_n = perc_right1 * n
  ) %>% 
  group_by(year) %>%
  reframe(perc_right1_n_mean = sum(perc_right1_n) / n_tot) %>% 
  unique() %>% 
  # view() %>% 
  ggplot(., aes(x = year, y = perc_right1_n_mean)) +
  geom_area(fill = "#B31942") +
  geom_line(color = "white", linewidth = 1) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(40, 50),
                     labels = function(x) paste0(x, " %")) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1890, 2010, 20)) +
  coord_cartesian(ylim = c(35, 70)) -> plot1

plot1 + 
  geom_hline(yintercept = c(40, 45, 50, 55), color = "white", linetype = "dashed", alpha = 0.3) +
  # geom_vline(xintercept = 1990, color = "white", linetype = "solid") +
  annotate("text", 
           x = 1890, y = 68, 
           hjust = 0,
           color = "white",
           size = 3,
           label = "The QWERTY effect: does technology influence how Americans chose baby names?") +
  annotate("text", 
           x = 1890, y = 65, 
           hjust = 0,
           color = "white",
           size = 2,
           label = "Percentage of baby names dominated by right-hand keys (red) and left-hand keys (blue) per year of birth") +
  theme(
    plot.title = element_text(vjust = -20, hjust = 0, colour = "white"),
    plot.subtitle = element_text(vjust = -20, hjust = 0, colour = "white"),
    panel.background = element_rect(fill = "#0A3161", color = NA),
    plot.margin = margin(rep(1, 4)),
    # plot.background = element_blank(), # element_rect(fill = color_bg, color = NA)
    panel.grid.major = element_blank(), #element_line(colour = "#C8EFE4", size = 0.1), # linetype = "dashed",
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # axis.text.y = element_text(margin = margin(l = -40)) # colour = "white", 
    axis.text.y = element_text(colour = "white",
                               vjust = -1,
                               margin = margin(l = 28, r = -45)),  # -40 aligns with 1890
    axis.text.x = element_text(colour = "white",
                               # vjust = -1,
                               margin = margin(t = -20, b = 10))
  )

  


# babynames_rl_long <- babynames_rl %>%
#   group_by(year) %>% 
#   summarise(method1_mean = mean(perc_right1),
#             method1_median = median(perc_right1),
#             method1_sd = sd(perc_right1),
#             method2_mean = mean(perc_right2),
#             method2_median = median(perc_right2),
#             method2_sd = sd(perc_right2)) %>% 
#   pivot_longer(
#     cols = -year,
#     values_to = "value",
#     names_to = "percent_summary_stat"
#   ) %>% 
#   mutate(method = str_extract(percent_summary_stat, "[:digit:]")) %>% 
#   mutate(percent_stat = str_extract(percent_summary_stat, "[:alpha:]{1,6}$"))
# 
# 
# plot <- babynames_rl %>%
#   group_by(year) %>% 
#   summarise(perc_r1 = mean(perc_right1),
#             perc_r2 = mean(perc_right2)) %>% 
#   ggplot(., aes(x = year, y = perc_r2)) +
#   geom_area(fill = "#B31942") +
#   geom_line(color = "white", linewidth = 1) +
#   scale_y_continuous(expand = c(0,0), 
#                      breaks = c(40, 50),
#                      labels = function(x) paste0(x, " %")) + 
#   scale_x_continuous(expand = c(0,0),
#                      breaks = seq(1890, 2010, 20)) +
#   # scale_y_continuous(limits = c(0, 100))
#   coord_cartesian(ylim = c(35, 70)) 
#   # scale_y_continuous(limits = c(40, 60)) +
#   # coord_cartesian(ylim = c(43, 53)) # sensationalist plot
#   # labs(
#   #     title = "The QWERTY effect on USA babynames?",
#   #   subtitle = "Percentage of baby names dominated by right-hand keys (red) and left-hand keys (blue) per year of birth"
#   # )
# 
# plot

# plot +
  # geom_hline(yintercept = c(40, 45, 50, 55), color = "white", linetype = "dashed", alpha = 0.3) +
  # # geom_vline(xintercept = 1990, color = "white", linetype = "solid") +
  # annotate("text", 
  #          x = 1890, y = 68, 
  #          hjust = 0,
  #          color = "white",
  #          size = 3,
  #          label = "The QWERTY effect: does technology influence how Americans chose baby names?") +
  # annotate("text", 
  #          x = 1890, y = 65, 
  #          hjust = 0,
  #          color = "white",
  #          size = 2,
  #          label = "Percentage of baby names dominated by right-hand keys (red) and left-hand keys (blue) per year of birth") +
  # theme(
  #   plot.title = element_text(vjust = -20, hjust = 0, colour = "white"),
  #   plot.subtitle = element_text(vjust = -20, hjust = 0, colour = "white"),
  #   panel.background = element_rect(fill = "#0A3161", color = NA),
  #   plot.margin = margin(rep(1, 4)),
  #   # plot.background = element_blank(), # element_rect(fill = color_bg, color = NA)
  #   panel.grid.major = element_blank(), #element_line(colour = "#C8EFE4", size = 0.1), # linetype = "dashed",
  #   panel.grid.minor = element_blank(),
  #   axis.ticks = element_blank(),
  #   axis.title = element_blank(),
  #   # axis.text.y = element_text(margin = margin(l = -40)) # colour = "white", 
  #   axis.text.y = element_text(colour = "white",
  #                              vjust = -1,
  #                              margin = margin(l = 28, r = -45)),  # -40 aligns with 1890
  #   axis.text.x = element_text(colour = "white",
  #                              # vjust = -1,
  #                              margin = margin(t = -20, b = 10))
  # )

ggsave("./try2.png",
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


# To Try's:
# - plot mean with sd ribbon
# - plot median and mean
# - calculate average before 1990 and after 1990
# - plot average before/after 1990 on plot ... ?
# - find cool font
# - find nice viz arrangement: plot without margin??
# 
# 




# Plot: Dominance 1 (B as left) -------------------------------------------

# Right- vs none- vs Left-dominated names (no sex distinction)

# B as left
babynames_rl %>% 
  group_by(year, predominance1) %>%
  summarise(n = n()) %>%
  # summarise(n = length(predominance)) %>% # same as n()
  # view() %>% 
  ggplot(., aes(x = year, y = n, fill = predominance1)) +
  ggstream::geom_stream(
    bw = 0,
    # type = "mirror"
    type = "proportional" # "ridge" "proportional" "mirror"
  ) +
  scale_fill_manual(values = c("#B31942", "#FFFFFF", "#0A3161")) +
  # theme_bw() +
  # labs(title = "B as Left") +
  geom_vline(xintercept = c(1990), linetype = "dotted", color = "#FFDEF9") + # "dashed"
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "#FFDEF9", color = NA),
    panel.grid.major = element_blank(), #element_line(colour = "#C8EFE4", size = 0.1), # linetype = "dashed",
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )
  # See an increase from the 1990s but still left dominated (also, left has more keys ... )
 

# Plot: Dominance 2 (B as right) -------------------------------------------

# B as right
babynames_rl %>% 
  group_by(year, predominance2) %>%
  summarise(n = n()) %>%
  # summarise(n = length(predominance)) %>% # same as n()
  # view() %>% 
  ggplot(., aes(x = year, y = n, fill = predominance2)) +
  ggstream::geom_stream(
    bw = 0,
    # type = "mirror"
    type = "proportional" # "ridge" "proportional" "mirror"
  ) +
  scale_fill_manual(values = c("#B31942", "#FFFFFF", "#0A3161")) +
  theme_void() +
  labs(title = "B as right")



# Plot: Dominance 1 (B as left) by sex -------------------------------------------

# Right- vs none- vs Left-dominated names, by sex 
babynames_rl %>% 
  group_by(year, sex, predominance1) %>%
  summarise(n = n()) %>%
  # summarise(n = length(predominance)) %>% # same as n()
  # view() %>% 
  ggplot(., aes(x = year, y = n, fill = predominance1)) +
  ggstream::geom_stream(
    # type = "mirror"
    type = "proportional" # "ridge" "proportional" "mirror"
  ) +
  scale_fill_manual(values = c("#B31942", "#FFFFFF", "#0A3161")) +
  theme_light() +
  geom_vline(xintercept = 1990) +
  facet_wrap(~sex,
             ncol = 1)
  # effect is more pronounced for Female names


# Plot: how many distinct names per sex ---------------------------------

# Now just plot the "diversity" of names by sex (how many unique names per sex?)
babynames %>% 
  group_by(year, sex) %>% 
  summarise(unique_names = n_distinct(name)) %>% 
  # view() %>% 
  ggplot(., aes(x = year, y = unique_names)) +
  geom_line(aes(color = sex))
  # there are more different female names than male names


# Plot: percentage of girls and boy borns per year ---------------------------

# Are there more female or male?
babynames %>% 
  group_by(year, sex) %>% 
  summarise(births = sum(n)) %>% 
  # view() %>% 
  # ggplot(., aes(x = year, y = births, color = sex)) +
  # geom_line() 
  ggplot(., aes(x = year, y = births, fill = sex)) +
  ggstream::geom_stream(
    # type = "mirror"
    type = "proportional", # "ridge" "proportional" "mirror"
    bw = 0
  ) +
  geom_hline(yintercept = 0.5, color = "white")
 # so many more baby girls than boys in the 1800! Why??
 # and why more males since the 1940???

# Which year saw "perfect" girls:boys ratio?
babynames %>% 
  group_by(year, sex) %>% 
  summarise(n = n()) %>% 
  pivot_wider(
    # cols = -year,
    names_from = sex,
    values_from = n
  ) %>% 
  rename(Fx = 'F') %>% 
  mutate(
    ratio = Fx/M,
    ratio_round = round(ratio, 1)) %>% 
  filter(ratio_round == 1) %>%
  view()
  # filter()


# # Try with different keyboard (B on right instead of left)
# # How important is key "B" (how many names contain at least 1 B?)
# babynames_rl %>% 
#   filter(str_detect(name, "b")) %>% 
#   view()
  

# Plot: right vs left key letter count by year, B and no B -----------------------------

# Now count total of right hand key letters per year
babynames_rl %>% 
  group_by(year) %>% 
  summarise(right_1 = sum(right_letters1),
            left_1 = sum(left_letters1),
            right_2 = sum(right_letters2),
            left_2 = sum(left_letters2)) %>% 
  pivot_longer(
    cols = -year,
    names_to = "side",
    values_to = "tot_letters"
  ) %>% 
  mutate(method = str_extract(side, "[:digit:]")) %>% #view()
  ggplot(., aes(x = year, y = tot_letters, fill = side)) +
  ggstream::geom_stream(
    # type = "mirror"
    type = "proportional", # "ridge" "proportional" "mirror"
    bw = 0
  ) +
  geom_hline(yintercept = 0.5) +
  facet_wrap(~method,
             ncol = 1)
 # still similar pattern as before



# # Plot: mean % of right vs left key letters in baby names -----------------------------
# 
# # Now plot average percentage of right key letters in names (per year)
# 
# babynames_rl_long <- babynames_rl %>%
#   group_by(year) %>% 
#   summarise(method1_mean = mean(perc_right1),
#             method1_median = median(perc_right1),
#             method1_sd = sd(perc_right1),
#             method2_mean = mean(perc_right2),
#             method2_median = median(perc_right2),
#             method2_sd = sd(perc_right2)) %>% 
#   pivot_longer(
#     cols = -year,
#     values_to = "value",
#     names_to = "percent_summary_stat"
#   ) %>% 
#   mutate(method = str_extract(percent_summary_stat, "[:digit:]")) %>% 
#   mutate(percent_stat = str_extract(percent_summary_stat, "[:alpha:]{1,6}$"))
# 
# 
# babynames_rl %>%
#   group_by(year) %>% 
#   summarise(perc_r1 = mean(perc_right1),
#             perc_r2 = mean(perc_right2)) %>% 
#   ggplot(., aes(x = year, y = perc_r2)) +
#   geom_area(fill = "#0A3161") +
#   geom_line(color = "red", size = 1) +
#   # scale_y_continuous(limits = c(0, 100))
#   # scale_y_continuous(limits = c(40, 60)) +
#   coord_cartesian(ylim = c(43, 53)) # sensationalist plot






# WORKS UNTIL HERE :) ------------------












babynames_rl$year %>% n_distinct() # 138 years

ggplot(data = babynames_rl, aes(x = year, y = n)) +
  geom_line(aes(color = predominance))



babynames_rl %>% 
  filter(year > 2000) %>%
  ggplot(., aes(x = year, y = n, fill = predominance)) + # y = TWh
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
  ) 

            







# 
# # Make function to iterate if necessary ... stll doesn't work 
# count_right <- function(x) {
#   rightk <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")
#   rightk <- paste(rightk, collapse = '|')
#   a <- stringr::str_count(x, fixed(rightk, ignore_case = T))
#   a <- sum(a)
#   return(a)
# }


count_right(names10) # Error in `stringr::str_count()`:
   # ! Can't recycle `string` (size 10) to match `pattern` (size 11).
   # Run `rlang::last_trace()` to see where the error occurred.







rrr <- paste(rightk, collapse='|')

str_count(names10[1], fixed(rrr, ignore_case = T))

stringr::str_detect("mario", fixed(rightk, ignore_case = T)) %>% sum()

tibble10 <- tibble(name = names10)

tibble10 %>% 
  # group_by(name) %>% 
  mutate(
    right = count_right(name))
   #%>% view()

stringr::str_count(toupper(names10[1]), rightk)

stringr::str_count(toupper("mario"), right) %>% sum()

stringr::str_count(toupper(test), right)


fruit <- c("apple", "banana", "pear", "pineapple")
str_count(fruit, "a")
str_count(fruit, c("a", "b", "p", "p"))
str_count("apple", c("a", "b", "p", "p"))
str_count("apple", c("a", "p", "z"))
str_count("MARIO", right)




stringr::str_count(toupper("mario"), paste(right, collapse = ""))



str_count(word, paste(right_hand_vector, collapse = ""))






# Function to count right-hand letters in a word
count_right_hand_letters <- function(word) {
  # Convert the word to uppercase for case-insensitive matching
  word <- toupper(word)
  
  # Count the occurrences of right-hand letters in the word
  count <- sum(str_extract(word, paste(right, collapse = "")) %in% toupper(right), na.rm = TRUE)
  
  return(count)
}

# Example usage
word <- "JUMP"
count <- count_right_hand_letters(word)

cat("Number of right-hand letters in", word, ":", count, "\n")

word <- "mario"



