rm(list=ls())
input<-read.table('/Users/eleib003/Google.Drive/Advent.of.Code.2018/input7.txt',header=F)

# Find each unique step
input[,2]<-as.character(input[,2])
input[,8]<-as.character(input[,8])

steps<-unique(c(input[,2],input[,8]))
nSteps<-length(steps)

depends<-matrix(ncol=10,nrow=nSteps,NA)
for (n in 1:nSteps){
  here<-which(input[,8]==steps[n])
  nHere<-length(here)
  if(nHere > 0){depends[n,1:nHere]<-input[here,2]}
}

go<-TRUE
ord<-character()
while(go){
  saveSpot<-integer()
  for (n in 1:nSteps){
    if(sum(!is.na(depends[n,]))==0){saveSpot<-c(saveSpot,n)}
  }
  nSaved<-length(saveSpot)
  if(nSaved == 0) {stop('here')}
  if(nSaved == 1) {
    ord<-c(ord,steps[saveSpot[1]])
    depends[saveSpot[1],1]<-'out'
    depends[depends==steps[saveSpot[1]]]<-NA
  } else {
    # Find lowest
    sorted<-sort(steps[saveSpot],index.return=TRUE)
    lowestLett<-sorted$x[1]
    hereLow<-saveSpot[sorted$ix[1]]
    ord<-c(ord,lowestLett)
    depends[hereLow,1]<-'out'
    depends[depends==lowestLett]<-NA
  }
  print(length(which(depends[,1]!='out' | is.na(depends[,1]))))
  if(length(which(depends[,1]!='out' | is.na(depends[,1])))==0){go<-FALSE}
}

paste(ord,sep='',collapse = '')
# # Find which is missing in begin list. This is the starting place.
# for (n in 1:nSteps){
#   if(length(which(input[,8]==steps[n]))==0){
#     ord<-steps[n]
#     break
#   }
# }
# 
# for (n in 1:nSteps){
#   here<-which(input[,2] == ord[n])
#   opts<-sort(input[here,8])
#   nOpts<-length(opts)
#   for (nn in 1:nOpts){
#     if(length(which(opts[nn]==ord))==0){ord<-c(ord,opts[nn]); break}
#     if(nn == nOpts){stop()}
#   }
# }
