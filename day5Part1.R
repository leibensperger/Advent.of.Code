#--- Day 5: Alchemical Reduction ---
# You've managed to sneak in to the prototype suit
# manufacturing lab. The Elves are making decent progress,
# but are still struggling with the suit's size reduction
# capabilities.
#
# While the very latest in 1518 alchemical technology
# might have solved their problem eventually, you can do
# better. You scan the chemical composition of the suit's
# material and discover that it is formed by extremely long
# polymers (one of which is available as your puzzle input).

# The polymer is formed by smaller units which, when
# triggered, react with each other such that two adjacent
# units of the same type and opposite polarity are destroyed.
# Units' types are represented by letters; units' polarity
# is represented by capitalization. For instance, r and R
# are units with the same type but opposite polarity,
# whereas r and s are entirely different types and do not
# react.

# For example:
# In aA, a and A react, leaving nothing behind.
# In abBA, bB destroys itself, leaving aA. As above,
#     this then destroys itself, leaving nothing.
# In abAB, no two adjacent units are of the same type, 
#     and so nothing happens.
# In aabAAB, even though aa and AA are of the same type,
#     their polarities match, and so nothing happens.
# Now, consider a larger example, dabAcCaCBAcCcaDA:
# dabAcCaCBAcCcaDA  The first 'cC' is removed.
# dabAaCBAcCcaDA    This creates 'Aa', which is removed.
# dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
# dabCBAcaDA        No further actions can be taken.

# After all possible reactions, the resulting polymer
# contains 10 units.

# How many units remain after fully reacting the polymer
# you scanned? (Note: in this puzzle and others, the input
# is large; if you copy/paste your input, make sure you
# get the whole thing.)
# To begin, get your puzzle input.
input.file<-'/Users/eleib003/Google.Drive/Advent.of.Code.2018/input5.txt'
# Open connection
con<-file(input.file,open='r')
# Read 
input<-readLines(con,n=1)
close(con)

inChar <- strsplit(input,'')[[1]]
outChar<-inChar
n<-1
nRec<-length(inChar)
# Loop through
while (n < nRec){
  print(n)
  #print(outChar[n:(n+5)])
  # Check if same letter
  if(toupper(outChar[n])==toupper(outChar[n+1])){
    # Check if mixed case; -1 is returned for lower, +1
    # for upper, so sum is 0
    if(unlist(gregexpr("[A-Z]",outChar[n+1])) +
       unlist(gregexpr("[A-Z]",outChar[n]))==0){
      # Chop out this combo:
      if(nRec != length(outChar)){stop()}
      # Catch case if n is 1
      if(n!=1 & n != (nRec-1)){
        outChar<-c(outChar[1:(n-1)],outChar[(n+2):nRec])
      } else{
        if(n==1){outChar<-outChar[3:nRec]} else{
           if(n == (nRec-1)){outChar<-outChar[1:(n-1)]}}
      }
      nRec<-length(outChar)
      n<-n-1 # need to go back and check for previous match
    } else {n<-n+1}    
  } else {n<-n+1}
  if(n <= 0){n<-1}
}
