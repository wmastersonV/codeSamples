#Dallam Masterson 5-11-2015

#This script is modularized allowing the user to customize these input parameters:
# 1) Number of people to include in graphs
#     1.a) user can pass in any number of people to be graphed
#     1.b) default is set to 9 users ordered by emails sent
# 2) unix time is converted to datetime and bucketed by X buckets
#     2.a) bucket size can be adjusted for performance, less buckets = faster runtime but less accurate graphs
#     2.b) optimal buckets = 85 (for the top 9 people ordered by sent emails)


#initialize inputs
numPeopleToGraph = 9
bucketInput = 85

# grab input name from linux command line
arg <- commandArgs(trailingOnly = TRUE)
inputFileName <- arg[1] 

#Extra Credit: I could pass in extra parameters for dynamic graphing 
# > Rscript --vanilla summarize-enron.R enron-event-history-all.csv 9 85
# numPeopleToGraph <- arg[2] 
# numBuckets <- arg[3] 

#check install packages
pkgs = c("ggplot2","sqldf","RSQLite","methods","gsubfn","proto","DBI","RSQLite.extfuns","stats")
isPkgInstal = pkgs %in% rownames(installed.packages())
for(k in 1 : length(isPkgInstal) ){
  if(!isPkgInstal[k]){
    install.packages(pkgs[k], dependencies = TRUE, repos="http://cran.rstudio.com/")
  }
}
library(ggplot2)
library(sqldf)


input = read.csv(inputFileName, header = FALSE,stringsAsFactors = FALSE)
input = input[,c(1,3,4)] 
names(input) = c("time","sender","recipients")

# create output file ------------------------------------------------------

#count number of emails sent by sender
senderNameCount = data.frame(table(input$sender))
names(senderNameCount) = c("person","sent")

#find recipients: split recipient names on "|", use Perl Regular Expressions for split

#vectorize operation, lapply faster that for loops
recipients = unlist(lapply(input$recipients, function(x) strsplit(x,"(?=[\\|\\E])", perl=TRUE)))

#remove "|" recipient name (artifact from Reg Ex split)
recipients = recipients[recipients != "|"]

#count of number of recieves by recipient
recipientNameCount = data.frame(table(recipients))
names(recipientNameCount) = c("person","received")

#remove "|" recipient name (artifact from Reg Ex split)
recipientNameCount = recipientNameCount[recipientNameCount$person != "|",]

#join recipient number to senders
output = merge(x = senderNameCount, y = recipientNameCount, by.x = "person", by.y = "person",
               all.x = TRUE, all.y = TRUE)

#order by number of emails sent
output = output[with(output, order(-sent)), ]

#replace NA's with 0
output[is.na(output)] <- 0


# viz 1: Emails sent by user over time -----------------------------------------------------------------

#creates buckets for timestamps, appends bucket number to input dataframe
addTimeBucket = function(numBuckets, input){
  maxT = max(input$time)
  minT = min(input$time)
  step = (maxT-minT) / numBuckets
  timeBucket = minT + step*(0:numBuckets)
  timeBucket[numBuckets] = maxT
  intTime = cbind(data.frame(1:(numBuckets + 1)), 
                  as.POSIXlt(timeBucket/1000,origin="1970-01-01"))
  names(intTime) = c("timeBucket","date")
  input$timeBucket = findInterval(input$time, sort(timeBucket) )
  input = merge(input, intTime, by.x = "timeBucket", by.y = "timeBucket", all.x = TRUE)
  return(input)
}

#builds Viz1 plot: emails over time for each person
sentEmailPlot = function(datIn, enName, bucket){ 
  
  #generate time buckets
  datIn = addTimeBucket(bucket, datIn)
  
  #subset on input sender names
  datIn = datIn[datIn$sender %in% enName,]
  
  #create column for number of sent emails per time bucket
  datIn1 = sqldf("select sender, count(sender) as numEmailSent, date 
                 from datIn 
                 group by date, sender")
  
  #plot total number of emails sent
  p1 = ggplot(datIn1, aes(date, numEmailSent)) + geom_area(aes(colour = sender)) + 
    facet_wrap(~sender) + theme(axis.text.x= element_text(angle = 45, hjust = 1)) + 
    labs(title = "Total Number of Sent Emails") + ylab("Outgoing Emails")
  
  return(p1)
}


# viz 2: unique number of contacts by time ---------------------------------------

#Calculate number of UNIQUE recipients per top senders in each time period
uniqueRespPlot = function(datIn, enName, bucket = NA){ 
  
  #generate time buckets
  datIn = addTimeBucket(bucket, datIn)
  
  #subset on input sender names
  datIn = datIn[datIn$sender %in% enName,]
  
  # loop over sender and time-buckets, count number of unique recipients by: 
  # sender, date, number of unique recipients 
  #store results in vectors, R doesnt handle lists or dataframes well especially in for loops
  l_sender = vector()
  l_date = vector()
  l_unique = vector()
  t1 = proc.time()
  bucketIndex = unique(datIn$timeBucket)
  senderIndex = unique(datIn$sender)
  dateIndex = unique(datIn$date)
  
  #loop over senders
  for(i in senderIndex){
    #loop over buckets
    for(j in 1:length(bucketIndex)){
      #select subset on sender and bucket
      datDummy = datIn[with(datIn, timeBucket == bucketIndex[j] & sender == i), ]
      
      #count of unique recipients by sender by time bucket
      uniqRec = length( unique( unlist( 
        strsplit( datDummy$recipients,"(?=[\\|\\E])", perl=TRUE))))
      l_sender = c(l_sender, i)
      l_date = c(l_date, dateIndex[j])
      l_unique = c(l_unique, uniqRec)      
    } 
  }
  #convert bucket number to date for graph
  datOut = data.frame(as.POSIXlt(l_date,origin="1970-01-01"),l_sender,l_unique)
  names(datOut)[1] = "l_date"
  
  #remove "|" due to regex split
  datOut[datOut$l_unique > 1, ]$l_unique = datOut[datOut$l_unique > 1, ]$l_unique - 1
  
  #create plot
  p1 = ggplot(datOut, aes(l_date, l_unique)) + geom_area(aes(colour = l_sender)) + 
    facet_wrap(~l_sender) + theme(axis.text.x= element_text(angle = 45, hjust = 1)) + 
    labs(title = "Unique Incoming Email Addresses") + ylab("Unique addresses received ") + xlab("Date")
  
  return(p1)
}

#pass in inputed number of people in graph and bucket size
enName = output$person[1:numPeopleToGraph]
outPlot1 = sentEmailPlot(input, enName, bucket = bucketInput)
outPlot2 = uniqueRespPlot(input, enName, bucket = bucketInput)

#save files in current directory
print("saving out1_masterson.csv, Plot1_masterson.png, and Plot2_masterson.png to current directory")
write.csv(output, file = "out1_masterson.csv", row.names = FALSE)
ggsave(outPlot1, file = paste("Plot1_masterson.png"))
ggsave(outPlot2, file = paste("Plot2_masterson.png"))
