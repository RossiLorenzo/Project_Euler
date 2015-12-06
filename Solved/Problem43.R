# The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the
# digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
# 
# Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
#   
# d2d3d4=406 is divisible by 2
# d3d4d5=063 is divisible by 3
# d4d5d6=635 is divisible by 5
# d5d6d7=357 is divisible by 7
# d6d7d8=572 is divisible by 11
# d7d8d9=728 is divisible by 13
# d8d9d10=289 is divisible by 17
# Find the sum of all 0 to 9 pandigital numbers with this property

# Libraries
library(gtools)
library(dplyr)

# d2d3d4 %% 2 == 0 --> d4 %% 2 == 0
# d3d4d5 %% 3 == 0 --> d3+d4+d5 %% 3 == 0
# d4d5d6 %% 5 == 0 --> d6 %% 5 == 0
d3d4d5d6 = data.frame(permutations(10, 4, 0:9)) %>% 
  filter(X2 %% 2 == 0,
         (X1 + X2 + X3) %% 3 == 0,
         X4 %% 5 == 0)

# For the other 7 numbers do all permutations
all_pandigital = lapply(1:nrow(d3d4d5d6), function(i){
  remaining = setdiff(0:9, d3d4d5d6[i, ])
  possible = data.frame(permutations(6, 6, remaining)) %>% 
    transmute(D1 = X1, D2 = X2, 
              D3 = d3d4d5d6[i, 1], 
              D4 = d3d4d5d6[i, 2], 
              D5 = d3d4d5d6[i, 3], 
              D6 = d3d4d5d6[i, 4],
              D7 = X3, D8 = X4, 
              D9 = X5, D10 = X6)
})
all_pandigital = rbind_all(all_pandigital)

# Filter the ones with the remaining four rules and print 
# the final sum
res = all_pandigital %>% 
  filter((100*D5 + 10*D6 + D7) %% 7 == 0,
         (100*D6 + 10*D7 + D8) %% 11 == 0,
         (100*D7 + 10*D8 + D9) %% 13 == 0,
         (100*D8 + 10*D9 + D10) %% 17 == 0)
sum(as.numeric(apply(res, 1, paste, collapse = "")))
