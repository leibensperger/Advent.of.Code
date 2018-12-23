# Ugly, inelegant, but it works!
rm(list=ls())
input<-read.table('/Users/eleib003/Google.Drive/Advent.of.Code.2018/input7.txt',header=F)

# Find each unique step
input[,2]<-as.character(input[,2])
input[,8]<-as.character(input[,8])

steps<-unique(c(input[,2],input[,8]))
nSteps<-length(steps)

depends<-matrix(ncol=15,nrow=nSteps,NA)
for (n in 1:nSteps){
  here<-which(input[,8]==steps[n])
  nHere<-length(here)
  if(nHere > 0){depends[n,1:nHere]<-input[here,2]}
}

go<-TRUE
ord<-character()
timed<-0
myLetters <- toupper(letters[1:26])
numWorkers<-5
depends[,13]<-'dependency'
#match("a", myLetters)
while(go){
  #if(timed==5){stop()}
  saveSpot<-integer()
  for (n in 1:nSteps){
    if((sum(!is.na(depends[n,1:12]))==0 |
       depends[n,13]=='wait' |
       depends[n,13]=='processing') & depends[n,13]!='out'){saveSpot<-c(saveSpot,n)}
  }
  nSaved<-length(saveSpot)
  if(nSaved == 0) {stop('here')}
  if(nSaved == 1) {
    if(depends[saveSpot[1],13]!='dependency'){
      if(depends[saveSpot[1],13]=='processing'){
        if(as.numeric(depends[saveSpot[1],15])==timed){
          ord<-c(ord,steps[saveSpot[1]])
          depends[saveSpot[1],13]<-'out'
          depends[saveSpot[1],14]<-NA
          depends[depends==steps[saveSpot[1]]]<-NA
        }
      } else {
        if(depends[saveSpot[1],13]=='wait'){
          # is one available?
          workers<-depends[which(!is.na(depends[,14])),14]
          if(length(workers)<numWorkers){
            depends[saveSpot[1],13]<-'processing'
            depends[saveSpot[1],14]<-1
            depends[saveSpot[1],15]<-timed+60+match(steps[saveSpot[1]],myLetters)-1
            if(timed == depends[saveSpot[1],15]){
              ord<-c(ord,steps[saveSpot[1]])
              depends[saveSpot[1],13]<-'out'
              depends[saveSpot[1],14]<-NA
              depends[depends==steps[saveSpot[1]]]<-NA
            }
          }
        } 
      }
    } else{
      workers<-depends[which(!is.na(depends[,14])),14]
      if(length(workers)<numWorkers){
        depends[saveSpot[1],13]<-'processing'
        depends[saveSpot[1],14]<-1
        depends[saveSpot[1],15]<-timed+60+match(steps[saveSpot[1]],myLetters)-1
        if(timed == depends[saveSpot[1],15]){
          ord<-c(ord,steps[saveSpot[1]])
          depends[saveSpot[1],13]<-'out'
          depends[saveSpot[1],14]<-NA
          depends[depends==steps[saveSpot[1]]]<-NA
        }
      } else {depends[saveSpot[1],13]<-'wait'}          
    }
  } else {
    # Find lowest
    sorted<-sort(steps[saveSpot],index.return=TRUE)
    inds<-saveSpot[sorted$ix]
    hold<-0
    for (nn in 1:nSaved){
      if(depends[inds[nn],13]!='dependency'){
        if(depends[inds[nn],13]=='processing'){
          if(as.numeric(depends[inds[nn],15])==timed){
            ord<-c(ord,steps[inds[nn]])
            depends[inds[nn],13]<-'out'
            depends[inds[nn],14]<-NA
            depends[depends==steps[inds[nn]]]<-NA
            hold<-hold+1
          }
        } else {
          if(depends[inds[nn],13]=='wait'){
            # is one available?
            workers<-depends[which(!is.na(depends[,14])),14]
            if((length(workers)+hold)<numWorkers){
              depends[inds[nn],13]<-'processing'
              depends[inds[nn],14]<-1
              depends[inds[nn],15]<-timed+60+match(steps[inds[nn]],myLetters)-1
              if(timed == depends[inds[nn],15]){
                ord<-c(ord,steps[inds[nn]])
                depends[inds[nn],13]<-'out'
                depends[inds[nn],14]<-NA
                depends[depends==steps[inds[nn]]]<-NA
                hold<-hold+1
              }
            }
          }
        }
      } else{
        workers<-depends[which(!is.na(depends[,14])),14]
        if((length(workers)+hold)<numWorkers){
          depends[inds[nn],13]<-'processing'
          depends[inds[nn],14]<-1
          depends[inds[nn],15]<-timed+60+match(steps[inds[nn]],myLetters)-1
          if(timed == depends[inds[nn],15]){
            ord<-c(ord,steps[inds[nn]])
            depends[inds[nn],13]<-'out'
            depends[inds[nn],14]<-NA
            depends[depends==steps[inds[nn]]]<-NA
            hold<-hold+1
          }
        } else {depends[inds[nn],13]<-'wait'}          
      }
    } #for
    
    #ord<-c(ord,lowestLett)
    #depends[hereLow,1]<-'out'
    #depends[depends==lowestLett]<-NA
  }
  print(length(which(depends[,13]!='out')))
  if(length(which(depends[,13]!='out'))==0){go<-FALSE}
  timed<-timed+1
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
