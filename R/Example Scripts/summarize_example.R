
# Normally I'd not put scripts in a package, but I'll relax that for the sake
# of making these examples easy to access

# A friend gave me an interesting problem to get me warmed up. The issue is that
# they have a data frame with columns that are numeric and non-numeric. They want
# to make a summary table using summarize() which has the row count for non-
# numeric columns and the sum for numeric columns. This script demos some diff-
# -erent approaches to solving the problem.


# --- Setup ---

library(tidyverse)

# Create a demo table
demo_data <- tibble(
  client = c("1", "1", "2", "2", "2", "3", "4", "4"),
  sale_val = c(123, 1000, 4572, 120, 387, 900, 3075,1000),
  sale_qty = c(61, 1, 3, 561, 56, 12, 13, 98),
  note = c("", "", "pricey!", "", "", "nice number", "", "vertigo")
)

# Determine the correct answers manually
check_data <- tibble(
  client = unique(demo_data$client),
  sale_val = c(sum(demo_data$sale_val[1:2]), sum(demo_data$sale_val[3:5]),
               sum(demo_data$sale_val[6]), sum(demo_data$sale_val[7:8])),
  sale_qty = c(sum(demo_data$sale_qty[1:2]), sum(demo_data$sale_qty[3:5]),
               sum(demo_data$sale_qty[6]), sum(demo_data$sale_qty[7:8])),
  note = c(2L, 3L, 1L, 2L)
)

# --- Original solution ---

# This is the partial solution given to me as a starting point:

orig_sln <- demo_data %>%
  summarise_all(list(~ (if(is.numeric(.)) sum(.) else n() )) )

# This looks odd and doesn't work!
identical(check_data, orig_sln) # returns FALSE

# --- Basic fix ---

# The primary issue here is that the original solution isn't grouping to get the
# answer, so let's fix that using group_by()

basic_sln <- demo_data %>%
  group_by(client) %>%
  summarise_all(list(~ (if(is.numeric(.)) sum(.) else n() )) )

# This works!
identical(check_data, basic_sln)

# ... but it still looks weird! Can we do it a better way?

# --- Fix using precomputed list? ---

# summarise_all takes a list argument which tells the function which summary
# statistics to compute, in order. The original & basic solutions are weird
# because the list is length 1 and the summary statistic contains logic:

# Example, running:
list(~ (if(is.numeric(.)) sum(.) else n() ))
# returns a list of length 1 which contains a long formula implementing the if???
# This is difficult to read and interperet & so hard to debug.

# I THOUGHT that what you strictly want here is a list that looks like this:
demo_list <- list(~sum(.x), ~sum(.x), ~n())

test_sln <- demo_data %>%
  group_by(client) %>%
  summarise_all(demo_list)

# BUT this doesn't work because it turns out that the list of summary statistics
# is applied to each argument.

# --- Fix using adjectives ---

# What if we can do this instead using the inbuilt function across()?
adj_sln <- demo_data %>%
  group_by(client) %>%
  summarise(
    across(where(is.numeric), sum),
    across(!where(is.numeric), ~n())
    )

# Here the "across" adjective lets you give a condition and an answer when that
# condition is met. By putting mutually exclusive adjectives inside summarise,
# it automatically creates the table with all the summary statistics we're
# interested in. I think this is the best way to do it because it makes it clear
# why this works :)

# This works!
identical(check_data, adj_sln)




