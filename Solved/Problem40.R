# An irrational decimal fraction is created by concatenating the positive integers:
#   
#   0.123456789101112131415161718192021...
# 
# It can be seen that the 12th digit of the fractional part is 1.
# 
# If dn represents the nth digit of the fractional part, 
# find the value of the following expression.
# 
# d1 × d10 × d100 × d1,000 × d10,000 × d100,000 × d1,000,000

# Collapse firs 10^6 numbers
numbs = 1:999999
x = as.numeric(strsplit(paste(numbs, collapse = ""), "", fixed = TRUE)[[1]])
# Get the 10^i with i in 1:6 numbers and multiply the together
prod(x[10^seq(1, 6)])
