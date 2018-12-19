# --- Part Two ---
# You notice that the device repeats the same frequency
# change list over and over. To calibrate the device, you
# need to find the first frequency it reaches twice.

# For example, using the same list of changes above, the
# device would loop as follows:
  
# Current frequency  0, change of +1; resulting frequency  1.
# Current frequency  1, change of -2; resulting frequency -1.
# Current frequency -1, change of +3; resulting frequency  2.
# Current frequency  2, change of +1; resulting frequency  3.
#  (At this point, the device continues from the start of the list.)
# Current frequency  3, change of +1; resulting frequency  4.
# Current frequency  4, change of -2; resulting frequency  2, which has already been seen.

# In this example, the first frequency reached twice is 2.
# Note that your device might need to repeat its list of
# frequency changes many times before a duplicate frequency
# is found, and that duplicates might be found while in the
# middle of processing the list.
#
# Here are other examples:
#  
#  +1, -1 first reaches 0 twice.
#  +3, +3, +4, -2, -4 first reaches 10 twice.
#  -6, +3, +8, +5, -6 first reaches 5 twice.
#  +7, +7, -2, -7, -4 first reaches 14 twice.
# What is the first frequency your device reaches twice?

######## SOLUTION
# This time we might have to actually loop through and 
# do the summation manually. Store previous answers and if
# there is a duplicate, we've found our answer.

startFreq <- 0 # starting point

# Read in frequency shift data
input<-read.table('/Users/eleib003/Google.Drive/Advent.of.Code/input1.txt')[,1]
nInput<-length(input)

val<-startFreq # first value is starting frequency
savedVals<-integer() # create vector to save data
go<-T # keep going until found, then false
nn<-0 # count iterations through list
while(go){
  print(nn) # display iteration #
for (n in 1:nInput){
  val <- val + input[n]
  savedVals<-c(savedVals,val)
  # Both methods below work; anyDuplicated appeared slower than brute force
  #here<-anyDuplicated(savedVals)
  #if(here[1] != 0) {print(val); go<-F;break}
  if(length(which(savedVals == val)) > 0) {print(val); go<-F;break}
}
  nn<-nn+1
}
