
# Data frames and tibbles

# Tibbles are provided by the tibble package and share the same structure as 
# data frames. The only difference is that the class vector is longer, and 
# includes tbl_df. This allows tibbles to behave differently in the key ways 
# which we’ll discuss below.

library(tibble)

df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)
#> [1] "list"

attributes(df2)
#> $names
#> [1] "x" "y"
#> 
#> $row.names
#> [1] 1 2 3
#> 
#> $class
#> [1] "tbl_df"     "tbl"        "data.frame"

## ---- Creating ----
# Beware of the default conversion of strings to factors. 
# Use stringsAsFactors = FALSE to suppress this and keep character vectors as 
# character vectors:
df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
str(df1)
#> 'data.frame':    3 obs. of  2 variables:
#>  $ x: int  1 2 3
#>  $ y: chr  "a" "b" "c"

# Creating a tibble is similar to creating a data frame. The difference between 
# the two is that tibbles never coerce their input (this is one feature 
# that makes them lazy):
df2 <- tibble(
  x = 1:3, 
  y = c("a", "b", "c")
)
str(df2)
#> tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ x: int [1:3] 1 2 3
#>  $ y: chr [1:3] "a" "b" "c"

# Additionally, while data frames automatically transform non-syntactic names 
# (unless check.names = FALSE), tibbles do not (although they do print non-syntactic 
# names surrounded by `).

# While every element of a data frame (or tibble) must have the same length, 
# both data.frame() and tibble() will recycle shorter inputs. However, while data 
# frames automatically recycle columns that are an integer multiple of the longest 
# column, tibbles will only recycle vectors of length one.
data.frame(x = 1:4, y = 1:2)
#>   x y
#> 1 1 1
#> 2 2 2
#> 3 3 1
#> 4 4 2
data.frame(x = 1:4, y = 1:3)
#> Error in data.frame(x = 1:4, y = 1:3): arguments imply differing number of
#> rows: 4, 3

tibble(x = 1:4, y = 1)
#> # A tibble: 4 x 2
#>       x     y
#>   <int> <dbl>
#> 1     1     1
#> 2     2     1
#> 3     3     1
#> 4     4     1
tibble(x = 1:4, y = 1:2)
#> Error: Tibble columns must have compatible sizes.
#> * Size 4: Existing data.
#> * Size 2: Column `y`.
#> ℹ Only values of size one are recycled.

# Data frames allow you to label each row with a name, a character vector 
# containing only unique values:
df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blond", "brown", "black"),
  row.names = c("Bob", "Susan", "Sam")
)
df3
#>       age  hair
#> Bob    35 blond
#> Susan  27 brown
#> Sam    18 black

# There are three reasons why row names are undesirable:
#   - Metadata is data, so storing it in a different way to the rest of the data 
#     is fundamentally a bad idea.
#   - Row names are a poor abstraction for labelling rows because they only work 
#     when a row can be identified by a single string.
#   - Row names must be unique, so any duplication of rows (e.g. from bootstrapping) 
#     will create new row names.

# For these reasons, tibbles do not support row names. Instead the tibble package 
# provides tools to easily convert row names into a regular column with either 
# rownames_to_column(), or the rownames argument in as_tibble():
as_tibble(df3, rownames = "name")
#> # A tibble: 3 x 3
#>   name    age hair 
#>   <chr> <dbl> <chr>
#> 1 Bob      35 blond
#> 2 Susan    27 brown
#> 3 Sam      18 black


x <- c(2.1, 4.2, 3.3, 5.4)
x[c(TRUE, TRUE, FALSE, FALSE)]
#> [1] 2.1 4.2
x[x > 3]
#> [1] 4.2 3.3 5.4
x[c(TRUE, FALSE)]
#> [1] 2.1 3.3
# Equivalent to
x[c(TRUE, FALSE, TRUE, FALSE)]
#> [1] 2.1 3.3

## Exercises 

# Fix each of the following common data frame subsetting errors:
mtcars[mtcars$cyl = 4, ]
mtcars[-1:4, ]
mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl == 4 | 6, ]

mtcars[mtcars$cyl == 4,]
mtcars[-c(1:4), ]
mtcars[mtcars$cyl <= 5, ]
mtcars[mtcars$cyl == 4 | mtcars$cyl ==  6, ]

# When the element is missing, pluck() always returns NULL (or the value of the 
# .default argument) and chuck() always throws an error. The behaviour of pluck() 
# makes it well suited for indexing into deeply nested data structures where the 
# component you want may not exist (as is common when working with JSON data from 
# web APIs). pluck() also allows you to mix integer and character indices, and
# provides an alternative default value if an item does not exist:
x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)

purrr::pluck(x, "a", 1)
#> [1] 1

purrr::pluck(x, "c", 1)
#> NULL

purrr::pluck(x, "c", 1, .default = NA)
#> [1] NA

# Given a linear model, e.g., mod <- lm(mpg ~ wt, data = mtcars), extract the 
# residual degrees of freedom. Then extract the R squared from the model summary 
# (summary(mod))
mod <- lm(mpg ~ wt, data = mtcars)
summary(mod)
mod$df.residual
mod[["df.residual"]]
summary(mod)$r.squared

x <- sample(10) < 4
which(x)
#> [1] 2 3 4

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)
#>  [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

# ----

### Using Reduce() to simulate a trajectory of a Markov Chain without a loop ----

# Create a transition matrix
P <- matrix(c(0, 0.1, 0.9, 0.2, 0.5, 0.3, 0, 0.5, 0.5), ncol = 3, byrow = T);
P

# Starting state and number of iterations
x_0 <- 2
K <- 100000

# simulate the next state of the Markov Chain given the current state and a 
# random number from a uniform distribution on [0,1]
newstate <- function(oldstate,u) {
  which.min(u>cumsum(P[oldstate,]))
}

x_1 <- newstate(x_0, runif(1))
x_1

# use the function newstate() on the result and a new random number:
x_2 <- newstate(x_1, runif(1))
x_2

# use newstate() in Reduce()
mc_without_loop <- Reduce(newstate, c(x_0, runif(K)), accumulate = TRUE)

head(mc_without_loop)
table(mc_without_loop)/length(mc_without_loop)

# ----








