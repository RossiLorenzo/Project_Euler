# The arithmetic sequence, 1487, 4817, 8147,
# in which each of the terms increases by 3330, 
# is unusual in two ways: 
#   (i) each of the three terms are prime, and, 
#   (ii) each of the 4-digit numbers are permutations 
#     of one another.
# 
# There are no arithmetic sequences made up of 
# three 1-, 2-, or 3-digit primes, 
# exhibiting this property, but there is one other 
# 4-digit increasing sequence.
# 
# What 12-digit number do you form by concatenating the
# three terms in this sequence?

# Libraries
library(Rcpp)
library(gtools)
library(dplyr)
library(parallel)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# Generate all the primes >= 10^4
top_primes = Eratosthenes_Sieve(10010)

# Get only primes with 4 digits 
primes_4_digits = data.frame(number = top_primes[nchar(top_primes) == 4])

# Split the prime into digits
digits = strsplit(as.character(primes_4_digits$number), "", fixed = TRUE)
primes_4_digits = Map(as.numeric, digits) %>% 
  do.call(rbind, .) %>% 
  cbind(primes_4_digits)

# For every number calculate all the permutations and check 
# if they are on the list.
# When a number is found such as all the permutations are
# still primes then flag it.
cores = makeCluster(11)
res = parLapplyLB(cores, seq_along(row(primes_4_digits)), function(i, primes_4_digits){
  library(dplyr)
  library(gtools)
  # Get the 4 digits
  x = as.numeric(primes_4_digits[i, 1:4])
  # Create all permutations removing the numbers that are 
  # not 4 digits anymore (leading 0) and the ones that
  # are not primes
  all_perms = permutations(4, 4, x, set = FALSE) %>% 
    data.frame() %>% 
    mutate(number = as.numeric(paste0(X1, X2, X3, X4))) %>% 
    filter(nchar(number) == 4,
           number %in% primes_4_digits$number) %>% 
    distinct()
  # If I'm left with less than 3 numbers than is definitely 
  # not the right answer and I can return NULL
  if(nrow(all_perms) < 3)
    return(NULL)
  # Expand unique combinations and calculate the diffs
  # Then choose only the one that have same difference
  all_diffs = expand.grid(rep(list(all_perms$number), 3)) %>% 
    filter(Var1 > Var2) %>% 
    filter(Var2 > Var3) %>% 
    mutate(diff1 = Var1 - Var2,
           diff2 = Var2 - Var3) %>% 
    filter(diff1 == diff2)
  # If is empty then return NULL
  if(nrow(all_diffs) == 0)
    return(NULL)
  # Otherwise we found one solution
  return(all_diffs)
}, primes_4_digits)
stopCluster(cores)

# Print the result
res = rbind_all(res)
unique(apply(res, 1, function(x){
  paste0(x[3], x[2], x[1], collapse = "")
}))
