# Blog post at: https://davidabelman.wordpress.com/2015/01/13/analysing-parkrun-data-using-r/
paste('Running script. See https://davidabelman.wordpress.com/2015/01/13/analysing-parkrun-data-using-r/ for blog post.')
library(ggplot2)

# Functions used ######################
CorrectTimeFormat <- function(s) {
  mins <- substr(s, 1, 2)
  mins <- as.numeric(mins)
  secs <- substr(s, 4, 5)
  secsInMins <- as.numeric(secs)/60
  return (mins+secsInMins)
}

LoadAndCleanParkrunResults <- function(filename) {
  # Load CSV to dataframe
  df <- read.csv(filename)
  df <- na.omit(df)
  # Convert Grade to numbers
  gradeNoPC <- gsub(" %", "", df$Grade)
  grade <- as.numeric(gradeNoPC)
  df$Grade <- grade
  # Convert time factor to minutes (numeric)
  df$Time <- CorrectTimeFormat(df$Time)
  return(df)
}

# Analysing my own time ######################
paste("Analysing my own data:")

# Load my own data
david <- read.csv('input/Parkrun - David_stats.csv')

# Only select runs where I was running alone! (i.e. my speed...)
david <- david[c(1,2,4,5,7,8,9),]

# Convert run dates into correct date format
david$Run.Date <- as.Date(david$Run.Date, '%d/%m/%Y')

# Convert times into numerics (in minutes)
david$Time <- CorrectTimeFormat(david$Time)

# Plot run date against time
paste("Creating plot (david_scatter.png)")
png("output/david_scatter.png",height=400,width=600)
ggplot(david, aes(x=Run.Date, y=Time, color=Event)) +
  geom_point(shape=20, size=7) + 
  labs(x="Run date", y="Run time (minutes)") + 
  ggtitle("5km Parkrun results") + 
  ylim(c(22, 24)) + 
  theme(legend.position = "bottom", text=element_text(size=12))
dev.off()

# Split into Finsbury and Southwark datasets of times only
finsbury.times <- david[david$Event=='Finsbury parkrun',]$Time
southwark.times <- david[david$Event=='Southwark parkrun',]$Time

# Do a t-test
paste("T-test to determine if my run time has improved")
t.test(finsbury.times,
       southwark.times,
       var.equal=TRUE, paired=FALSE)



# Comparing Southwark and Finsbury Park ######################
paste("Now comparing Southwark and Finsbury Park fields on most recent week of data")
finsbury.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - Fns_2015_01_10.csv')
southwark.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - Sth_2015_01_10.csv')

# Find summary statistics
paste("Summary stats:")
summary(finsbury.results$Time)
summary(southwark.results$Time)

# Plot gender splits
paste("Creating chart (male_female_split.png)")
require(gridExtra)
png("output/male_female_split.png",height=400,width=800)
title = 'Finsbury Park'
plot1 <- ggplot(finsbury.results, aes(x=Time, fill=X)) + 
  geom_density(alpha=0.5) +
  labs(x="Run time (minutes)", y="Density") + 
  scale_fill_discrete(name='Gender', labels=c('Female', 'Male')) + 
  ggtitle(title) + 
  theme(legend.position = "bottom", text=element_text(size=12)) +
  ylim(c(0, 0.165))
title = 'Southwark'
plot2 <- ggplot(southwark.results, aes(x=Time, fill=X)) + 
  geom_density(alpha=0.5) +
  labs(x="Run time (minutes)", y="Density") + 
  scale_fill_discrete(name='Gender', labels=c('Female', 'Male')) + 
  ggtitle(title) + 
  theme(legend.position = "bottom", text=element_text(size=12)) +
  ylim(c(0, 0.165))
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# Take males only
finsbury.results.male <- finsbury.results[finsbury.results$X=='M',]
southwark.results.male <- southwark.results[southwark.results$X=='M',]

# Plot age splits
# Sum up by age category
f_cat <- data.frame ( summary(finsbury.results.male$Cat) )
s_cat <- data.frame ( summary(southwark.results.male$Cat) )
# Add in course name
f_cat$Course = 'Finsbury Park'
s_cat$Course = 'Southwark'
# Add in age as a column
f_cat$age = rownames(f_cat)
s_cat$age = rownames(s_cat)
# Make sure both have same col names
colnames(f_cat) <- c('count', 'Course', 'age')
colnames(s_cat) <- c('count', 'Course', 'age')
# Convert to percentages
f_cat$count <- round(prop.table(f_cat[,1]),3)
s_cat$count <- round(prop.table(s_cat[,1]),3)
# Make sure both have same col names (again!)
colnames(f_cat) <- c('count', 'Course', 'age')
colnames(s_cat) <- c('count', 'Course', 'age')
# Remove any rows with 0
s_cat <- s_cat[s_cat$count!=0,]
f_cat <- f_cat[f_cat$count!=0,]
# Merge vertically
age.splits <- rbind(f_cat, s_cat)

paste("Creating chart (age_split_of_males.png)")
png("output/age_split_of_males.png",height=500,width=800)
ggplot(age.splits, aes(x=factor(age), 
                    y=count,
                    fill=Course)) + 
  geom_bar(stat='identity',
           position='dodge',
           width=0.8) +
  labs(x="Age category", y="Proportion of runners") + 
  ggtitle("Age split of male runners at both Finsbury Park and Southwark (by %)") + 
  theme(legend.position = "bottom",
        text=element_text(size=16),
        axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# Do a t-test
paste("T-test to compare males at F & S, determining if 1 course is easier")
t.test(finsbury.results.male$Time,
       southwark.results.male$Time,
       var.equal=TRUE,
       paired=FALSE)

# Analysing pre and post Christmas results ######################
paste("Finally, we analyse pre and post Xmas performance")
# Load in Finsbury Park between subjects data
f256.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - F256.csv') #20th
f260.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - F260.csv') #3rd

# Load in Southwark between subjects data
s68.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - S68.csv') #20th Dec
s71.results <- LoadAndCleanParkrunResults(filename = 'input/Parkrun - S71.csv') #3rd Jan

# Total number of runners in 20th Dec / 3rd Jan races
runner.total <- ( dim(s68.results) + dim(s71.results) + dim(f256.results) + dim(f260.results) )[1]
paste("Total runners over 4 races:", runner.total)

# Running both races
f256.results$Course = 'Finsbury Park'
f260.results$Course = 'Finsbury Park'
s68.results$Course = 'Southwark'
s71.results$Course = 'Southwark'
dec20.results <- rbind(f256.results, s68.results)
jan3.results <- rbind(f260.results, s71.results)
joinedByRunner <- merge(dec20.results, jan3.results, by=c('parkrunner','Course'))
joinedByRunner <- joinedByRunner[,c('Course','Time.x', 'Time.y', 'X.x')]
colnames(joinedByRunner) <- c('Course', 'Pre.Christmas', 'Post.Christmas', 'Gender')
paste("Total runners running both races:", dim(joinedByRunner[1]))

# Plot chart of correlation
paste("Creating chart (prepost_xmas.png)")
png("output/prepost_xmas.png",height=500,width=500)
ggplot(joinedByRunner, aes(x=Pre.Christmas, y=Post.Christmas)) +
  geom_point(shape=20, size=7) + 
  labs(x="Pre Christmas time in minutes (20th Dec)", y="Post Christmas time in minutes (3rd Jan)") + 
  ggtitle("Pre and Post Christmas run timings for 5km") + 
  theme(legend.position = "bottom", text=element_text(size=12)) +
  geom_abline(intercept = 0)
dev.off()

# Plot chart of residuals
joinedByRunner$Improvement <- joinedByRunner$Pre.Christmas - joinedByRunner$Post.Christmas 

paste("Creating chart (prepost_residuals.png)")
png("output/prepost_residuals.png",height=500,width=500)
ggplot(joinedByRunner, aes(x=Pre.Christmas, y=Improvement)) +
  geom_point(shape=20, size=7) + 
  labs(x="Pre Christmas time (20th Dec)", y="Improvement in time") + 
  ggtitle("Improvement (or worsening) of run time over Christmas period") + 
  theme(legend.position = "bottom", text=element_text(size=12)) +
  geom_abline(intercept = 0, slope=0) 
dev.off()

paste("Creating chart (prepost_residuals_coursecolor.png)")
png("output/prepost_residuals_coursecolor.png",height=550,width=500)
ggplot(joinedByRunner, aes(x=Pre.Christmas, y=Improvement, color=Course)) +
  geom_point(shape=20, size=7) + 
  labs(x="Pre Christmas time (20th Dec)", y="Improvement in time") + 
  ggtitle("Improvement (or worsening) of run time over Christmas period") + 
  theme(legend.position = "bottom", text=element_text(size=12)) +
  geom_abline(intercept = 0)
dev.off()

# Paired t-test (everyone)
paste("Paired t-test using Southwark and F.Park")
t.test(joinedByRunner$Pre.Christmas,
       joinedByRunner$Post.Christmas,
       var.equal=TRUE,
       paired=TRUE)

# Paired t-test (Finsbury only)
paste("Paired t-test using F.Park only")
t.test(joinedByRunner[joinedByRunner$Course=='Finsbury Park',]$Pre.Christmas,
       joinedByRunner[joinedByRunner$Course=='Finsbury Park',]$Post.Christmas,
       var.equal=TRUE,
       paired=TRUE)

# Paired t-test (Southwark)
paste("Paired t-test using Southwark only")
t.test(joinedByRunner[joinedByRunner$Course=='Southwark',]$Pre.Christmas,
       joinedByRunner[joinedByRunner$Course=='Southwark',]$Post.Christmas,
       var.equal=TRUE,
       paired=TRUE)