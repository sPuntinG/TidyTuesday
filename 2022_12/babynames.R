
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

# What I need to do is:
# 1. make vector with names and vector with right keys
# 2. write function (done above)
# 3. loop function over list (vectors)
# 4. make a tibble of this (names, right_letters)
# 5. mutate() to calculate tot nr of letter (nchar()) and the nchar - right_letters = left_letters
# 6. Calculate ratio : right/left 
# 7. Calculate predominance: ratio >1 = right (ration <1 = left; == = center)
# 8. figure out how to plot


# Right and left keys
leftk <- c("Q", "W", "E", "R", "T", "A", "S", "D", "F", "G", "Z", "X", "C", "V", "B")
rightk <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")

babynames$name %>% unique() %>% n_distinct() # 97'310 (!!!)
names10 <- babynames$name %>% unique() %>% .[1:10] # small set for now as it's a huge data set ... !


# Count how many letters in a name are from the right hand keys
# str_count() works on single word
stringr::str_count(names10[1], fixed(rightk, ignore_case = T)) # 1 0 0 0 0 0 0 0 0 0 1   this can simply be summed up with sum() = 2
stringr::str_count(names10[1], fixed(rightk, ignore_case = T)) %>% sum() # 2 which is what I need in the end

# But does not work on vectors
stringr::str_count(names10[1:2], fixed(rightk, ignore_case = T)) # Error in `stringr::str_count()`:
                                                                 # ! Can't recycle `string` (size 2) to match `pattern` (size 11).

# Make function to iterate
count_right <- function(x) {
  rightk <- c("Y", "U", "I", "O", "P", "H", "J", "K", "L", "N", "M")
  # rightk <- paste(rightk, collapse = '|')
  a <- stringr::str_count(x, fixed(rightk, ignore_case = T))
  a <- sum(a)
  return(a)
}

count_right("mario") # 3
count_right(names10)# as expected doesn't work on vectorized argument

# Use map_dbl to loop so that the output is a numeric vector 
map_dbl(names10, count_right) 

tibble10 <- tibble(name = names10, 
                   tot_letters = nchar(name),
                   right_letters = map_dbl(names10, count_right),
                   left_letters = tot_letters - right_letters,
                   ratio = right_letters/left_letters,
                   predominance = case_when(
                     ratio == 1 ~ "none",
                     ratio > 1 ~ "right",
                     ratio < 1 ~ "left"
                     )
                   )


# WORKS UNTIL HERE :) ------------------
# Now repeat with whole data set and join table and plot ...


               
      





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



