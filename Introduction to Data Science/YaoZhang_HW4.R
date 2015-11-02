library(plyr)
#1.The following code finds the summary (sum) for every group. There is also a way to clock the time needed to perform the operations.
# Start the clock!
ptm <- proc.time()
#Split
pieces <- split(baseball[,6:9], baseball$year)
# Apply
results <- vector('list', length(pieces))
names <- names(pieces)
for(i in seq(1, length(pieces))){
  piece <- pieces[[i]]
  results[[i]] <- sum(piece)}
# Combine
result <- do.call('rbind', results)
# Stop the clock
proc.time() - ptm
#(a)The group is the statistcs of g(number of games),ab(number of times at bat),r(number of runs) and h(hits) from 1871 and 2007
#(b)"user time"is 0.060, "system time" is 0.003

#(c)
# Start the clock!
ptm <- proc.time()
#Split
pieces <- split(baseball[,6:9], baseball$year)
# Apply
results2 <- vector('list', length(pieces))
names <- names(pieces)
for(i in seq(1, length(pieces))){
  piece <- data.matrix(pieces[[i]])
  results2[[i]] <- mean(piece)}
# Combine
result2 <- do.call('rbind', results2)
# Stop the clock
proc.time() - ptm

#2.An equivalent way to find the previous summary is
# Start the clock!
ptm <- proc.time()
newsum<- ddply(baseball, .(year), function
               (df) sum(df[,6:9]))
# Stop the clock
proc.time() - ptm
#(a)Yes, the data was split by year.
#(b)"ddply" is used to split data frame, apply function, and the return results in a data frame; "aaply"is used to split array, apply function, and the return results in an array;"apply" is used to apply functions over array margins.The current data we have is a data fram, not an array. If we want to use "aaply" or "apply", the data frame needs to be transformed to array first.
#(c)"user time" is 0.072, "system time" is 0.011", which is longer than the previous methods.
#(d)
# Start the clock!
ptm <- proc.time()
newsum2<- daply(baseball[, 6:9], .(baseball$year), colwise(mean))
# Stop the clock
proc.time() - ptm

#3. The following calculates the career year for Babe Ruth
baberuth <- subset(baseball, id =='ruthba01')
baberuth <- transform(baberuth, cyear = year - min(year) + 1)
#What does the following code calculate?
baseball <- ddply(baseball, .(id), transform, cyear = year - min(year) + 1)
#this code caculates the career year for all players in this dataset

#4. The following calculates the total number of teams in the data frame. 
summarise(baseball, nteams = length(unique(team)))
#Can you create asummary (using the “summarise”) of
#(a) career length
careerlength <- ddply(baseball, "id", summarise, duration = max(year) - min(year),nteams = length(unique(team)))
#(b) total number of games played
gamenumbers <- ddply(baseball, "id", summarise, ngames = sum(g), nteams = length(unique(team)))

#5. Create a new variable for career batting average (cba) in the data frame. Submit the code
cba <- ddply(baseball, "id", summarise, cba = sum(h)/sum(ab), nteams = length(unique(team)))
newbaseball <- merge(baseball,cba,by.x = "id",by.y = "id",all = TRUE)
#6. Explain what the following code does
model <- function(df){
  lm(h/ab ~ cyear, data = df)
}
baseball <- subset(baseball,ab>=25)
bmodels <- dlply(baseball, .(id), model)
bcoefs <- ldply(bmodels, function(x) coef(x))
names(bcoefs)[2:3] <- c('intercept', 'slope')
#these codes fit linear models relating to baseball players' career duration and their batting average for players who have had no less than 25 times at bat.