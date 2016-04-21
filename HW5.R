x <- c("ATGCATTGGACGTTAG") 
# Here I defined a nucleotide sequence as x 

reversecomplementer <- function(x,y="ALL"){
  # named a function reversecomplementer, performs various functions defined by y==... on my 
  # nucleotide sequence x , body of the function is confined in {} squigly brackets 
  reverse <- paste(rev(substring(x, 1:nchar(x), 1:nchar(x)) ), collapse="")
  # Here I am wrapping multiple commands together using parentheses
  # EXPLAIN SUBSTRING
  revcomp <- chartr("ATGC", "TACG", reverse) 
  # EXPLAIN CHARACTER REPLACEMENT
  if(y=="revcomp"){
    return(revcomp)
    # Here I used an if statement, where you place the variable, here revcomp, as y==... and 
    # return a specific output so that you can get multiple outputs on one x variable by altering
    # the y variable all contained under one function 
  } else if (y=="rev"){
    reverse <- paste(rev(substring(x, 1:nchar(x), 1:nchar(x)) ), collapse="")
    return(reverse)
    # Here I used an else if statement in addition to the if statement becuase in this function I 
    # wanted to contain 3 different possible outputs 
  } else if (y=="comp"){
    complement <- chartr("ATGC", "TACG", x)
    return(complement)
  }
  # each else if statement needs brackets to contain the function body
}
# the entire function must be closed off with brackets

reversecomplementer(x, "revcomp")
reversecomplementer(x, "rev")
reversecomplementer(x, "comp")
# you can then perform each of the subfunctions within the function by entering a sequence, x, 
# and specifying which output you would like 

