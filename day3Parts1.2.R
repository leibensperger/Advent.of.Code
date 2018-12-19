#--- Day 3: No Matter How You Slice It ---
# The Elves managed to locate the chimney-squeeze prototype
# fabric for Santa's suit (thanks to someone who helpfully
# wrote its box IDs on the wall of the warehouse in the
# middle of the night). Unfortunately, anomalies are still
# affecting them - nobody can even agree on how to cut the
# fabric.
#
# The whole piece of fabric they're working on is a very
# large square - at least 1000 inches on each side.
#
# Each Elf has made a claim about which area of fabric
# would be ideal for Santa's suit. All claims have an ID
# and consist of a single rectangle with edges parallel
# to the edges of the fabric. Each claim's rectangle is
# defined as follows:
#  
#  The number of inches between the left edge of the fabric
#     and the left edge of the rectangle.
#  The number of inches between the top edge of the fabric
#     and the top edge of the rectangle.
#  The width of the rectangle in inches.
#  The height of the rectangle in inches.
#
# A claim like #123 @ 3,2: 5x4 means that claim ID 123
# specifies a rectangle 3 inches from the left edge,
# 2 inches from the top edge, 5 inches wide, and
# 4 inches tall. Visually, it claims the square inches
# of fabric represented by # (and ignores the square
# inches of fabric represented by .) in the diagram below:

#      ...........
#      ...........
#      ...#####...
#      ...#####...
#      ...#####...
#      ...#####...
#      ...........
#      ...........
#      ...........

# The problem is that many of the claims overlap, causing
# two or more claims to cover part of the same areas.
# For example, consider the following claims:
  
  #1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2
#  Visually, these claim the following areas:
  
#  ........
#  ...2222.
#  ...2222.
#  .11XX22.
#  .11XX22.
#  .111133.
#  .111133.
#  ........

# The four square inches marked with X are claimed by both 1
# and 2. (Claim 3, while adjacent to the others, does not
# overlap either of them.)

# If the Elves all proceed with their own plans, none of
# them will have enough fabric. How many square inches of
# fabric are within two or more claims?

# To begin, get your puzzle input.
input<-read.table('/Users/eleib003/Google.Drive/Advent.of.Code.2018/input3.txt',header=F,comment.char = '?')
# Assign column names
colnames(input)<-c('Record','sym','Offset','Area')
# Find the number of records we're dealing with
nInput<-length(input$Record)

# Create a matrix "cloth" to mark up when records indicate
fabric<-matrix(ncol = 1200,nrow=1200,data = 0)
# Loop through all records
for (n in 1:nInput){
  # Get offset (top, left)
  sp <- unlist(strsplit(as.character(input$Offset[n]),','))
  left<- as.numeric(sp[1])
  top<-as.numeric(unlist(strsplit(sp[2],':')))
  # Get area (width, height)
  sp <- unlist(strsplit(as.character(input$Area[n]),'x'))  
  width<-as.numeric(sp[1])
  height<-as.numeric(sp[2])
  
  # "Mark" the fabric
  fabric[(top+1):(top+height),(left+1):(left+width)]<-
    fabric[(top+1):(top+height),(left+1):(left+width)]+1
}

# To find which record is still good, loop through again
# There is probably faster way since we're recalculating
# coordinates, but this works
for (n in 1:nInput){
  # Get offset (top, left)
  sp <- unlist(strsplit(as.character(input$Offset[n]),','))
  left<- as.numeric(sp[1])
  top<-as.numeric(unlist(strsplit(sp[2],':')))
  # Get area (width, height)
  sp <- unlist(strsplit(as.character(input$Area[n]),'x'))  
  width<-as.numeric(sp[1])
  height<-as.numeric(sp[2])
  
  # If our area is only used once, it'll have an area average of 1
  if(mean(fabric[(top+1):(top+height),(left+1):(left+width)])==1){
   saveID<-n
   break
  }
}

length(which(fabric > 1))
input[saveID,]