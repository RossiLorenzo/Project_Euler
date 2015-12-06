# The nth term of the sequence of triangle numbers is given by, 
# tn = ½n(n+1); so the first ten triangle numbers are:
#   
#   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
# 
# By converting each letter in a word to a number corresponding to its 
# alphabetical position and adding these values we form a word value. 
# For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. 
# If the word value is a triangle number then we shall call the word a 
# triangle word.
# 
# Using words.txt (right click and 'Save Link/Target As...'), a 16K text 
# file containing nearly two-thousand common English words, 
# how many are triangle words?

# Extract the words straight from the website and create dictionary
words = readLines("http://projecteuler.net/project/resources/p042_words.txt")
words = strsplit(words, ",", fixed = TRUE)[[1]]

# Function that gives a value to a word
word_to_value = function(w){
  let = strsplit(w, "", fixed = TRUE)[[1]]
  sum(unlist(lapply(let, function(l){ which(l == LETTERS) })))
}

# Calculate value of all words
words = data.frame(words, stringsAsFactors = FALSE)
words$value = unlist(lapply(words$words, word_to_value))

# It can be proven that first 20 triangle numbers are all the ones
# <= max(words value)
n = 1:20
triangles = (1/2) * n * (n - 1)

# Find all the traingle words
words_triangle = words$words[words$value %in% triangles]
length(words_triangle)
