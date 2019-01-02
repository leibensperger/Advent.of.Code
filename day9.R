# This is terribly inefficient and clearly a brute force method
# There must be a better way, but this does indeed work.
input.file<-'/Users/eleib003/Google.Drive/Advent.of.Code.2018/input9.txt'
# Open connection to read file
con<-file(input.file,open='r')
oneLine<-readLines(con,n=1)
splitUp<-strsplit(oneLine,split=' ')[[1]]
nPlayers<-as.numeric(splitUp[1])
maxVal<-as.numeric(splitUp[7])*100
scores<-vector(mode='numeric',length=nPlayers)
close(con)

# starting point
marbles<-c(0,1)
val<-2
current<-2
player<-3
while(val <= maxVal){
  print(val/maxVal * 100)
  if(val %% 23 !=0){
    nmarbles<-length(marbles)
    newCurrent<-current+2
    if(newCurrent>(nmarbles+1)){
      newCurrent<-newCurrent - nmarbles
      marbles<-c(marbles[1:(newCurrent-1)],
                 val,
                 marbles[(newCurrent):nmarbles])
    } else{if(newCurrent==(nmarbles+1)){
      marbles<-c(marbles,val)
    } else{marbles<-c(marbles[1:(newCurrent-1)],val,
                      marbles[(newCurrent):nmarbles])
    }}
  
    current<-newCurrent
  } else {
    #if(val==92){stop()}
    scores[player]<-scores[player]+val
    #also add 7 to left
    left<-current-7
    nmarbles<-length(marbles)
    if(left<0){
      spot<-nmarbles+left
      if(spot==nmarbles){marbles<-c(marbles[1:(spot-1)])}else{
      marbles<-c(marbles[1:(spot-1)],marbles[(spot+1):nmarbles])}
      scores[player]<-scores[player]+marbles[spot]
      current<-spot
    } else{
      scores[player]<-scores[player]+marbles[left]
      if(left==1){marbles<-marbles[2:nmarbles]}else{
      marbles<-c(marbles[1:(left-1)],marbles[(left+1):nmarbles])}
      current<-left
    }
  }
  #if(sum(is.na(scores))>0){stop()}
  val<-val+1
  player<-player+1
  if(player>nPlayers){player<-1} 
  #if(length(marbles)==20){stop()}
}
max(scores)