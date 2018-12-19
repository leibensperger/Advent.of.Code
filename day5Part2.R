#--- Part Two ---
# Time to improve the polymer.

# One of the unit types is causing problems; it's preventing
# the polymer from collapsing as much as it should. Your
# goal is to figure out which unit type is causing the
# most problems, remove all instances of it (regardless of
# polarity), fully react the remaining polymer, and measure
# its length.

# For example, again using the polymer dabAcCaCBAcCcaDA
# from above:

# Removing all A/a units produces dbcCCBcCcD.
#   Fully reacting this polymer produces dbCBcD,
#  which has length 6.
# Removing all B/b units produces daAcCaCAcCcaDA.
#   Fully reacting this polymer produces daCAcaDA,
#   which has length 8.
# Removing all C/c units produces dabAaBAaDA.
#   Fully reacting this polymer produces daDA, which
#   has length 4.
# Removing all D/d units produces abAcCaCBAcCcaA.
#   Fully reacting this polymer produces abCBAc, which
#   has length 6.
# In this example, removing all C/c units was best,
# producing the answer 4.

# What is the length of the shortest polymer you can
# produce by removing all units of exactly one type
# and fully reacting the result?

# Although it hasn't changed, you can still get your
# puzzle input.
input.file<-'/Users/eleib003/Google.Drive/Advent.of.Code.2018/input5.txt'
# Open connection
con<-file(input.file,open='r')
# Read 
input<-readLines(con,n=1)
# Close
close(con)
# Split up into characters
inChar <- strsplit(input,'')[[1]]
# Find unique letters (all of them)
letts<-unique(toupper(inChar))
nLetts<-length(letts) # number of letters

# Vector to store the length after removing letters
dat <- vector(mode='numeric',length = nLetts)

# Loop through each letter
for (l in 1:nLetts){
  # remove letter
outChar<-inChar[inChar != tolower(letts[l]) & inChar != letts[l]]
n<-1
nRec<-length(outChar)
# Loop through
while (n < nRec){
  #print(n)
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
print(c(l,n,letts[l]))
dat[l]<-n
}
