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


#
sequencevector <- c("ATGCGATCGGGCTAGGCT", "GTGGGCAAGATAGC", "GGGAAATTCCTGATCCTAG")
sequencematrix <- as.matrix(sequencevector)
sequencematrix
# Here I am again using the sequence vector as an example of a list of DNA sequences
# next I made the sequence vector into a matrix

ORF1 <- function(x){paste(substring(x, 1))}
ORF2 <- function(x){paste(substring(x, 2))}
ORF3 <- function(x){paste(substring(x, 3))}
# Here I created three different functions which paste a substring of a string
# starting at the first, 2nd, and 3rd character in the string
# in this way, I can get the 1st, 2nd, and 3rd ORF of a DNA sequence 

ORF_sequencematrix <- cbind (ORF1(sequencematrix), ORF2(sequencematrix), ORF3(sequencematrix))
# Here I performed the ORF1-3 functions on my sequence matrix.
# With each ORF function, I generated a new matrix with rows 1-3 being 
# either ORF 1, 2, or 3. 
# I think combined these three new matrices by column with cbind, to create
# one large matrix listing the three sequences by row and the three different
# ORFs by column

ORF_sequencedf <- as.data.frame(ORF_sequencematrix)
# Here I converted my new matrix into a data frame 

colnames(ORF_sequencedf) <- c("ORF1", "ORF2", "ORF3")
# Here I renamed the columns in my ORF sequencematrix to read as ORF1-3



ORFs <- NULL
for(i in seq(sequencematrix[,]) {
  revcomplement2 <- function(i){
    revcomplement2 <- chartr("ATGC", "TACG", paste(rev(substring(i, 1:nchar(i), 1:nchar(i)) ), collapse="")) 
    return(revcomplement2) 
  }
  ORFs <- c(ORFs, revcomplement2(sequencevector[i]))
}


AAdf <- read.table(file="http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/AA.txt", header=TRUE, sep="\t") 
AAdf[1:4,]

AAv <- as.character(AAdf[,2]) 
names(AAv) <- AAdf[,1] 
AAv

sequencevector
sequencevector[1]
ORF1 <- substring(sequencevector[1], 1)
substring(sequencevector[1], 2)
ORF31212 <- substring(sequencevector[1], 3)

ORFy(seq)
ORF2

seq <- c("ATGATGATAG")
seq <- substring(sequencevector, 1:nchar(sequencevector), 1:nchar(sequencevector))

ORFg <- c("ATGGATAG")
ORFg <- gsub("(...)", "\\1_", ORFg) 
ORFg <- unlist(strsplit(ORFg, "_")) 
ORFg <- ORFg[grep("^...$", ORFg)]
ORFg

ORF1 <- function(z) {
  ORFa <- substring(z, 1)
  ORFa <- paste(ORFa, collapse="")
  return(ORFa)
}

ORF2 <- function(z) {
  ORFb <- substring(z, 2)
  ORFb <- paste(ORFb, collapse="")
  return(ORFb)
}

ORF3 <- function(z) {
  ORFa <- substring(z, 3)
  ORFa <- paste(ORFa, collapse="")
  return(ORFa)
}

ORF1(seq)
ORF2(seq)
ORF3(seq)

ORFgenerator <-- function(z, frame="ALL") 
{
  if (frame=="1") {
    ORF <- paste((substring(z, 1), collapse="")) 
  } else if(frame=="2"){
    ORF <- paste((substring(z, 2), collapse="")) 
  } else if(frame=="3"){
    ORF <- paste((substring(z, 3), collapse="")) 
  } else {
    ORF <- c(paste(seq[1:length(seq)], collapse=""), 
             paste(seq[2:length(seq)], collapse=""), 
             paste(seq[3:length(seq)], collapse=""))
  }
  ORF <- gsub("(...)", "\\1_", ORF) 
  ORF <- unlist(strsplit(ORF, "_")) 
  ORF <- ORF[grep("^...$", ORF)]
  sequence_translation <- AAv[ORF]
  return(sequence_translation)


#sapply(sequencevector[i], function(sequencevector, frame="ALL"){
sapply(sequencevector, function(z, frame="ALL") 
{
    if (frame=="1") {
      ORF <- paste((substring(sequencevector[1], 1), collapse="")) 
    } else if(frame=="2"){
      ORF <- paste((substring(sequencevector[1], 2), collapse="")) 
    } else if(frame=="3"){
      ORF <- paste((substring(sequencevector[1], 3), collapse="")) 
    } else {
      ORF <- c(paste(seq[1:length(seq)], collapse=""), 
               paste(seq[2:length(seq)], collapse=""), 
               paste(seq[3:length(seq)], collapse=""))
    }
    ORF <- gsub("(...)", "\\1_", ORF) 
    ORF <- unlist(strsplit(ORF, "_")) 
    ORF <- ORF[grep("^...$", ORF)]
    sequence_translation <- AAv[ORF]
    return(as.matrix(sequence_translation))
    
} )

translate(sequencevector, frame="1")
sequencevector
