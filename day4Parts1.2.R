#--- Day 4: Repose Record ---
# You've sneaked into another supply closet - this time,
# it's across from the prototype suit manufacturing lab.
# You need to sneak inside and fix the issues with the suit,
# but there's a guard stationed outside the lab, so this is
# as close as you can safely get.

# As you search the closet for anything that might help,
# you discover that you're not the first person to want to
# sneak in. Covering the walls, someone has spent an hour
# starting every midnight for the past few months secretly
# observing this guard post! They've been writing down the
# ID of the one guard on duty that night - the Elves seem
# to have decided that one guard was enough for the overnight
# shift - as well as when they fall asleep or wake up while
# at their post (your puzzle input).

# For example, consider the following records, which have
# already been organized into chronological order:

#[1518-11-01 00:00] Guard #10 begins shift
#[1518-11-01 00:05] falls asleep
#[1518-11-01 00:25] wakes up
#[1518-11-01 00:30] falls asleep
#[1518-11-01 00:55] wakes up
#[1518-11-01 23:58] Guard #99 begins shift
#[1518-11-02 00:40] falls asleep
#[1518-11-02 00:50] wakes up
#[1518-11-03 00:05] Guard #10 begins shift
#[1518-11-03 00:24] falls asleep
#[1518-11-03 00:29] wakes up
#[1518-11-04 00:02] Guard #99 begins shift
#[1518-11-04 00:36] falls asleep
#[1518-11-04 00:46] wakes up
#[1518-11-05 00:03] Guard #99 begins shift
#[1518-11-05 00:45] falls asleep
#[1518-11-05 00:55] wakes up

# Timestamps are written using year-month-day hour:minute
# format. The guard falling asleep or waking up is always the
# one whose shift most recently started. Because all
# asleep/awake times are during the midnight hour (00:00 -
# 00:59), only the minute portion (00 - 59) is relevant for
# those events.

# Visually, these records show that the guards are asleep
# at these times:

# Date   ID   Minute
#             000000000011111111112222222222333333333344444444445555555555
#             012345678901234567890123456789012345678901234567890123456789
# 11-01  #10  .....####################.....#########################.....
# 11-02  #99  ........................................##########..........
# 11-03  #10  ........................#####...............................
# 11-04  #99  ....................................##########..............
# 11-05  #99  .............................................##########.....

# The columns are Date, which shows the month-day portion
# of the relevant day; ID, which shows the guard on duty
# that day; and Minute, which shows the minutes during
# which the guard was asleep within the midnight hour.
# (The Minute column's header shows the minute's ten's digit
# in the first row and the one's digit in the second row.)
# Awake is shown as ., and asleep is shown as #.

# Note that guards count as asleep on the minute they fall
# asleep, and they count as awake on the minute they wake up.
# For example, because Guard #10 wakes up at 00:25 on
# 1518-11-01, minute 25 is marked as awake.

# If you can figure out the guard most likely to be asleep
# at a specific time, you might be able to trick that guard
# into working tonight so you can have the best chance of
# sneaking in. You have two strategies for choosing the
# best guard/minute combination.

# Strategy 1: Find the guard that has the most minutes asleep.
# What minute does that guard spend asleep the most?

# In the example above, Guard #10 spent the most minutes
# asleep, a total of 50 minutes (20+25+5), while Guard #99
# only slept for a total of 30 minutes (10+10+10). Guard #10
# was asleep most during minute 24 (on two days, whereas any
# other minute the guard was asleep was only seen on one day).

# While this example listed the entries in chronological
# order, your entries are in the order you found them.
# You'll need to organize them before they can be analyzed.

# What is the ID of the guard you chose multiplied by the
# minute you chose? (In the above example, the answer would
# be 10 * 24 = 240.)

# To begin, get your puzzle input.
input.file<-'/Users/eleib003/Google.Drive/Advent.of.Code.2018/input4.txt'
# Open connection to read file
con<-file(input.file,open='r')

# Read first line and set up date/message 
oneLine<-readLines(con,n=1)
dateTime<-as.POSIXct(substr(oneLine,2,17),format='%Y-%m-%d %H:%M')
message<-substr(oneLine,20,100)
# Keep reading and appending until end of file
while(length(oneLine<-readLines(con,n=1))>0){
  dateTime<-c(dateTime,as.POSIXct(substr(oneLine,2,17),format='%Y-%m-%d %H:%M'))
  message<-c(message,substr(oneLine,20,100))
}
# close connection
close(con)

# Order the dates/times
ord<-order(dateTime)
# Apply ordering to date time and messages
dateTime<-dateTime[ord]
message<-message[ord]

nRec<-length(message)
first<-TRUE
# dummy matrix to store guard sleep data
# ID then 60 minutes, will tally minutes asleep
recs<-matrix(ncol=61,nrow=1,0)
n<-1
while(n < nRec){
  print(n)
  # find out what we're doing here
  what<-substr(message[n],1,5)
  print(what)
  
  # If guard, update ID
  if(what == "Guard") {
    ID <- as.numeric(substr(message[n],8,11))
    #day<-format(dateTime[n],'%m-%d')
  } else {
    # if falling asleep, find start time, end time
    if(what == "falls"){
      minStart<-as.numeric(format(dateTime[n],'%M'))
      n<-n+1
      what2<-substr(message[n],1,5)
      if(what2 != 'wakes') {stop()}
      minEnd <- as.numeric(format(dateTime[n],'%M'))
      mins<-(1:60)*0; 
      mins[(minStart+1):(minEnd)]<-1
      # If first time through, just add records to first row
      if(first){
        first<-FALSE; 
        recs[1,]<-c(ID,mins)
      } else {
        # Find if this ID has been used before
        here<-which(recs[,1]==ID)
        # If it hasn't, add a new row
        if(length(here)==0){recs<-rbind(recs,c(ID,mins))} else{
          # if it hasn't, add times
          recs[here,2:61]<-recs[here,2:61]+mins
        }
      } # end else
    } # end if falls
  } # end else
  n<-n+1
} # end while

# Tally up each guards total
guards<-integer()
for (n in 1:dim(recs)[1]){
  guards<-c(guards,sum(recs[n,2:61]))
  print(n)
  print(max(recs[n,2:61]))
}

# Multiply guard id number x minute of max sleep (note-1 to  get to actual minute)
recs[which.max(guards),1]*(which.max(recs[which.max(guards),2:61])-1)

# Part 2
# Find max sleep for given minute:
inds<-which(recs[,2:61]==max(recs[,2:61]),arr.ind=T)
recs[inds[1]]*(inds[2]-1)
