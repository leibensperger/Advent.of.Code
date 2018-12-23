#--- Day 6: Chronal Coordinates ---
# The device on your wrist beeps several times, and once
# again you feel like you're falling.

# "Situation critical," the device announces. "Destination
# indeterminate. Chronal interference detected. Please
# specify new target coordinates."

# The device then produces a list of coordinates (your
# puzzle input). Are they places it thinks are safe or
# dangerous? It recommends you check manual page 729.
# The Elves did not give you a manual.

# If they're dangerous, maybe you can minimize the danger
# by finding the coordinate that gives the largest distance
# from the other points.

# Using only the Manhattan distance, determine the area
# around each coordinate by counting the number of integer
# X,Y locations that are closest to that coordinate (and
# aren't tied in distance to any other coordinate).

# Your goal is to find the size of the largest area that
# isn't infinite. For example, consider the following
# list of coordinates:

#1, 1
#1, 6
#8, 3
#3, 4
#5, 5
#8, 9
# If we name these coordinates A through F, we can draw
# them on a grid, putting 0,0 at the top left:

# ..........
# .A........
# ..........
# ........C.
# ...D......
# .....E....
# .B........
# ..........
# ..........
# ........F.

# This view is partial - the actual grid extends infinitely
# in all directions. Using the Manhattan distance, each
# location's closest coordinate can be determined, shown
# here in lowercase:
  
#  aaaaa.cccc
#  aAaaa.cccc
#  aaaddecccc
#  aadddeccCc
#  ..dDdeeccc
#  bb.deEeecc
#  bBb.eeee..
#  bbb.eeefff
#  bbb.eeffff
#  bbb.ffffFf

# Locations shown as . are equally far from two or more
# coordinates, and so they don't count as being closest
# to any.

# In this example, the areas of coordinates A, B, C,
# and F are infinite - while not shown here, their areas
# extend forever outside the visible grid. However, the
# areas of coordinates D and E are finite: D is closest
# to 9 locations, and E is closest to 17 (both including
# the coordinate's location itself). Therefore, in this
# example, the size of the largest area is 17.

#What is the size of the largest area that isn't infinite?
#To begin, get your puzzle input.

input<-read.table('/Users/eleib003/Google.Drive/Advent.of.Code.2018/input6.txt',header=F,sep=',')                                                                                                                                                                  
colnames(input)<-c('X','Y')                                                                                                                                                                  
nInput<-length(input$X)

minX<-min(input$X); maxX<-max(input$X)
minY<-min(input$Y); maxY<-max(input$Y)
input$X<-input$X-minX+1
input$Y<-input$Y-minY+1
nX<-maxX-minX+1
nY<-maxY-minY+1
domain<-matrix(ncol=nX+1,nrow=nY+1,0)
region<-matrix(ncol=nX+1,nrow=nY+1,0)
# DOMAIN:
#      x= 0, 1, 2, 3, 4, 5, ...
#  y= 0
#  y= 1

for (x in 1:(nX+1)){
  for (y in 1:(nY+1)){
    diffs<-abs(x-input$X)+abs(y-input$Y) 
    if(sum(diffs) < 10000){region[y,x]<-1}
    itis<-which(diffs==min(diffs))
    if(min(diffs)==0){itis<- -which(diffs ==0)}
    if(length(itis)>1){itis<-0}
    domain[y,x]<-itis
  }
}

counts<-vector(mode='numeric',length=nInput)
for (n in 1:nInput){
  counts[n]<-length(which(domain==n | domain== -n))
  if(length(which(abs(domain[1,])==n))>0 |
     length(which(abs(domain[,1])==n))>0 |
     length(which(abs(domain[nY,])==n))>0 |
     length(which(abs(domain[,nX])==n))>0) {counts[n]<-0}
}
