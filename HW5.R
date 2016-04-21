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



sequencevector <- c("ATGCGATCGGGCTAGGCT", "GTGGGCAAGATAGC", "GGGAAATTCCTGATCCTAG")
# Here I created a vector of 3 different nucleotide sequences by concatenating 3 sequences
# together 

revcomploop <- NULL
# Here I created an empty vector to deposit my reverse complemented sequences into later
for(i in seq(sequencevector[])) {
  # here I opened a forward loop that will perform functions on the variable i, which in this case are
  # individual nucleotides, within the vector named sequencevector 
  revcomplement2 <- function(i){
  revcomplement2 <- chartr("ATGC", "TACG", paste(rev(substring(i, 1:nchar(i), 1:nchar(i)) ), collapse="")) 
    return(revcomplement2) 
  # here I am again defining the function for reverse complementing a sequence as described above
  # in this instance however, I am using the variable i instead of x becuase i refers to each
  # character in a sequence within my vector whereas before x referred to the only sequence present 
  # in the sequence I named x
}
  revcomploop <- c(revcomploop, revcomplement2(sequencevector[i]))
  # here I concatenated the reversed sequences from sequencevector that I generated with the 
  # forward loop of my reversecomplement function with the revcomploop empty vector I made earlier
  # in order to deposit these reverse complemented sequences into the empty vector, creating a new
  # vector with the reverse complemented sequences 
}
  
revcomploop
sequencevector
# here I visualized my reverse complemented sequences and my original sequences 


