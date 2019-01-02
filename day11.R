power<-matrix(ncol=300,nrow=300,NA)

sn<-8561
for(x in 1:300){
  for (y in 1:300){
    ID<-x+10
    pow<-ID*y
    pow<-pow+sn
    dig<-as.integer(pow*ID/100)%%10
    power[x,y]<-dig-5
  }
}

# FOR PART 1
maxCell<-0
for(x in 2:299){
  for (y in 2:299){
    cell<-sum(power[(x-1):(x+1),(y-1):(y+1)])
    if(cell > maxCell){maxCell<-cell;xSave<-x;ySave<-y}
  }
}

# To get top left index
xAns<-xSave-1
yAns<-ySave-1

# FOR PART 2
maxCell<-0
for(size in 1:300){
  print(size)
  xmin <- 1 
  xmax <- 300-size+1 
  ymin <- 1
  ymax <- 300-size+1
  for(x in xmin:xmax){
    for (y in ymin:ymax){
      #print(c(x,y,size))
      cell<-sum(power[x:(x+size-1),y:(y+size-1)])
      if(cell > maxCell){
        print('New max cell:')
        print(c(x,y,size,maxCell))
        maxCell<-cell;xSave<-x;ySave<-y;sizeSave<-size}
    }
  }
}

# To get top left index
xAns<-xSave
yAns<-ySave
