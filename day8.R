rm(list=ls())
input<-as.numeric(read.table('/Users/eleib003/Google.Drive/Advent.of.Code.2018/input8.txt',header=F))

#2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
#A----------------------------------
#  B----------- C-----------
#  D-----
#  In this example, each node of the tree is also marked
# with an underline starting with a letter for easier
# identification. In it, there are four nodes:
  
# A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
# B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
# C, which has 1 child node (D) and 1 metadata entry (2).
# D, which has 0 child nodes and 1 metadata entry (99).
# The first check done on the license file is to simply
# add up all of the metadata entries. In this example,
# that sum is 1+1+2+10+11+12+2+99=138
#2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
#A----------------------------------
#    B----------- C-----------
#                     D-----

# This one was tricky for me, got some help with the
# recursive idea from Reddit!
sumMeta <- function(n,input){
  print(n) # where are we
  # setup output
  out<-c(n+2,0,0) # default next place is two ahead, sum
  nchild<-input[n] # num childreen is first
  nmeta<-input[n+1] # then num meta data
  childs<-vector(mode='numeric',length = nchild)
  # loop through each child
  if(nchild > 0){
  for (nn in 1:nchild){
    tot <- sumMeta(out[1],input)
    out[2]<-out[2]+tot[2]
    out[1]<-tot[1]
    childs[nn]<-tot[3]
  }}
  out[2]<-out[2]+sum(input[out[1]:(out[1]+nmeta-1)])
  if(nchild == 0){out[3]<-out[2]}else{
    for (nn in 1:nmeta){
      j<-input[out[1]+nn-1]
      if(j <= nchild){out[3]<-out[3]+childs[j]}
    }
  }
  out[1]<-out[1]+nmeta
  return(out)
}

sumMeta(1,input)
