#--- Part Two ---
# Confident that your list of box IDs is complete, you're
# ready to find the boxes full of prototype fabric.
#
# The boxes will have IDs which differ by exactly one
# character at the same position in both strings.
# For example, given the following box IDs:
#
# abcde
# fghij
# klmno
# pqrst
# fguij
# axcye
# wvxyz
# 
# The IDs abcde and axcye are close, but they differ by
# two characters (the second and fourth). However, the
# IDs fghij and fguij differ by exactly one character,
# the third (h and u). Those must be the correct boxes.
#
# What letters are common between the two correct box IDs?
# (In the example above, this is found by removing the
# differing character from either ID, producing fgij.)

# Read in data
input<-as.vector(read.table('/Users/eleib003/Google.Drive/Advent.of.Code/input2.txt',header=F)[,1])
nInput<-length(input)

# Variable to hold IDs that match except one character
savedIDs<-character()
# Length of each record. 
# **NOTE THAT THIS AN ASSUMPTION THAT ALL HAVE SAME LENGTH**
recLength<-length(unlist(strsplit(input[1],'')))

# Loop through each record
for (n in 1:(nInput-1)){
  # First record for comparison
  rec1<-unlist(strsplit(input[n],''))
  
  # Loop through remaining records to compare
  for (nn in n:nInput){
    # Second record for comparison
    rec2<-unlist(strsplit(input[nn],''))
    # Reset number of mismatches
    wrong<-0
    # Loop through each position
    for (r in 1:recLength){
      # If there is a mismatch, tally
      if(rec1[r] != rec2[r]){wrong<-wrong+1}
      # If we get more than one, move on
      if(wrong > 1) {break}
    }
    # If there is just 1 mismatch, then we add the IDs to our
    # saved list
    if(wrong == 1){savedIDs<-c(savedIDs,input[n],input[nn])}
  }
}

print(savedIDs)
