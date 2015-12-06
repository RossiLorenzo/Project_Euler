# Take the number 192 and multiply it by each of 1, 2, and 3:
#   
#   192 × 1 = 192
#   192 × 2 = 384
#   192 × 3 = 576
# 
# By concatenating each product we get the 1 to 9 pandigital, 192384576. 
# We will call 192384576 the concatenated product of 192 and (1,2,3)
#   
# The same can be achieved by starting with 9 and multiplying 
# by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, 
# which is the concatenated product of 9 and (1,2,3,4,5).
#   
# What is the largest 1 to 9 pandigital 9-digit number that can be formed 
# as the concatenated product of an integer with (1,2, ... , n) where n > 1?
  
# The upper bound is 10,000 since the concatenated 10,000 * 1 + 10,000 *2 
# Will already have 10 digits
# For every n < 10,000 calculate all possible concatened products
all_prod = lapply(1:9999, function(k){
  i = 2
  conc = k
  numb_digits = nchar(conc)
  while(numb_digits < 9){
    conc = paste0(conc, k * i)
    digits = strsplit(conc, "", fixed = TRUE)[[1]]
    if("0" %in% digits)
      return(NULL)
    numb_digits = length(digits)
    if(numb_digits != length(unique(digits)))
      return(NULL)
    i = i + 1
  }
  if(numb_digits > 9)
    return(NULL)
  return(list(Value = as.numeric(conc), Number = k))
})

# Print the result
all_values = unlist(sapply(all_prod, "[[", 1))
all_values[which(all_values == max(all_values))]
