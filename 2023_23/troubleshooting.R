
# Example 1: Ordered and same number of elements 
# WORKS
df1 <- data.frame(x = rep(1:10, 3), 
                 y = rpois(30, 2), 
                 group = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", #10 
                           "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", #10 
                           "C", "C", "C", "C", "C", "C", "C", "C", "C", "C")) #10
df

# Example 2: Ordered and different number of elements.
# Does NOT work  
df2 <- data.frame(x = rep(1:10, 3), 
                 y = rpois(30, 2), 
                 group = c("A", "A", "A", "A", "A", "A", #6 
                           "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", #10  
                           "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C")) #14

# Example 3: Partially mixed and same number of elements.
# WORKS
df3 <- data.frame(x = rep(1:10, 3), 
                 y = rpois(30, 2), 
                 group = c("A", "A", "B", "B", "C", "C",  "A", "A", "B", "B", 
                           "C", "C", "A", "A", "B", "B", "C", "C", "A", "A", 
                           "B", "B", "C", "C", "A", "A", "B", "B", "C", "C"))

df3 %>% 
  group_by(group) %>% 
  summarise(n = n())


df3 <- df3 %>%
  group_by(x,group) %>%
  summarise(y=sum(y))


indivesources %>% 
  group_by(country, year) %>% 
  summarise(nr_esource = n_distinct(esource),
            missing = length(is.na(TWh))) %>% view()


indivesources %>% 
  replace_na(list(TWh = 0.000)) %>% 
  view()



# Plot NAs, 0s and both to try to find patterns that is problematic for geom_stream() ...
indivesources %>% 
  # filter(country == "Austria" | country == "Portugal" | country == "Italy" | country == "Belgium") %>% 
  mutate(empty = if_else(TWh == 0 | is.na(TWh), "x", ""),
         NAs = if_else(is.na(TWh), "x", ""),
         zeros = if_else(TWh == 0, "x", "")) %>%
  replace_na(list(zeros = "")) %>% 
  pivot_longer(
    cols = empty:zeros,
    names_to = "stuff_missing",
    values_to = "where"
  ) %>% 
  ggplot(., aes(
    x = year, y = esource
  )) +
  geom_point(aes(color = where)) +
  scale_color_manual(values = c(NA, "black")) +
  facet_grid(stuff_missing~country)
# See that both and only Austria&Portugal have a full "empty" row! That's the problem I guess ...

# *** Malta has no data -> remove from plot



# This version of the plot actually displays values (also for countries that are blank in "ggstream.R")
# So ... let 's take it from here 
indivesources %>% 
  filter(country == "Portugal") %>% 
  # replace_na(list(TWh = 0.000)) %>% 
  # mutate(TWh = if_else(is.na(TWh), 0, TWh)) %>% # Converting all NAs to 0.000 doesn't do the trick ...
  mutate(TWh = if_else(TWh == 0, NA, TWh)) %>% # This is to convert all 0.000 t0 NA so it can be better handled by ggstream
  ggplot(., aes(x = year, y = TWh, fill = esource)) +
  ggstream::geom_stream(
    geom = "polygon",
    color = color_text,
    linewidth = 0.75,
    bw = 0.6
  )


