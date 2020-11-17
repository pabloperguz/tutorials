
## Quiz ----
# Given the following data frame, how do I create a new column called “3” 
# that contains the sum of 1 and 2? You may only use $, not [[. 
# What makes 1, 2, and 3 challenging as variable names?
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)
df$'3' <- runif(3)

# In the following code, how much memory does y occupy?
x <- runif(1e6)
y <- list(x, x, x)
lobstr::obj_size(y)
# 8,000,128 B

# On which line does a get copied in the following example?
a <- c(1, 5, 3, 2)
b <- a 
b[[1]] <- 10 # this one; a is copied when b is modified
# ----

## Chapter ----

# We’ll use the lobstr package to dig into the internal representation of R objects
require(lobstr)

# You can access an object’s identifier with lobstr::obj_addr();
# that is, the object’s memory “address”. Identifiers are long, 
# and change every time you restart R.
lobstr::obj_addr(y)

# Explain the relationship between a, b, c and d in the following code:
a <- 1:10
b <- a
c <- b
d <- 1:10
lobstr::obj_addr(a)
# a and b are the same object, pointing to the same identifier. Whilst c equals b, 
# it has a different identifier given it was created as equivalent to an object
# other than the original (a). `d`` is completely different object with its own
# identifier, albeit it has the same values as a, b and c. 

# The following code accesses the mean function in multiple ways. 
# Do they all point to the same underlying function object? 
# Verify this with lobstr::obj_addr().
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")
lobstr::obj_addr(match.fun("mean"))
# mean, get("mean") and match.fun("mean") have the same identifier, base::mean doesn't

# To see values that are shared across lists, use lobstr::ref(). ref() prints 
# the memory address of each object, along with a local ID so that you can easily 
# cross-reference shared components.
l1 <- list(1, 2, 3)
l2 <- l1
l2[[3]] <- 4
lobstr::ref(l1, l2)

# Data frames are lists of vectors, so copy-on-modify has important consequences 
# when you modify a data frame. If you modify a column, only that column needs to 
# be modified; the others will still point to their original references. 
# However, if you modify a row, every column is modified, which means every 
# column must be copied.
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d2 <- d1
d2[, 2] <- d2[, 2] * 2
d3 <- d1
d3[1, ] <- d3[1, ] * 3
lobstr::ref(d1, d2, d3)

x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4
# ----
