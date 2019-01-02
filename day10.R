input.file<-'/Users/eleib003/Google.Drive/Advent.of.Code.2018/input10.txt'
# Open connection to read file
con<-file(input.file,open='r')
oneLine<-readLines(con,n=1)
x<-as.numeric(substr(oneLine,11,16))
y<-as.numeric(substr(oneLine,18,24))
u<-as.numeric(substr(oneLine,37,38))
v<-as.numeric(substr(oneLine,40,42))

while(length(oneLine<-readLines(con,n=1))>0){
  x<-c(x,as.numeric(substr(oneLine,11,16)))
  y<-c(y,as.numeric(substr(oneLine,18,24)))
  u<-c(u,as.numeric(substr(oneLine,37,38)))
  v<-c(v,as.numeric(substr(oneLine,40,42)))
}
y<- -y
v<- -v
close(con)
plot(x,y,typ='p',main='0')
go<-TRUE
last<-1000000
t<-1
# Keep going until the y-dimension is minimized. Assumes
# that most compact is the answer. This turns out to work.
while(go){
  x <- x + u
  y <- y + v
  dif<-max(y)-min(y)
  if(dif > last){go<-FALSE;x<-x-u;y<-y-v;t<-t-1}else{last<-dif;t<-t+1}
}

plot(x,y,typ='p',main=as.character(t))
