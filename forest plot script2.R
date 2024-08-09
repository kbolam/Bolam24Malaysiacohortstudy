#This is a call for a 2 panel forest plot#

# set working directory
setwd("C:/Users/katieb/Documents/Final Forest Plot/Final Forest Plot")
# read source code from files
source("OxForestPlot0 - lltoul.r")


# Define the plot; nb: commented out lines are defaults (you can change if you want to)
datafile <- "C:/Users/katieb/Documents/Final Forest Plot/Final Forest Plot/Forestplot data Chinese.csv"
datafile2 <- "C:/Users/katieb/Documents/Final Forest Plot/Final Forest Plot/Forestplot data indian.csv"
MainTitle <- "Odds ratios for self-reported diagnosis of selected NCDs among Chinese and Indian compared to Malay."
EventsLabel <- "Events"
EstimateLabel <- "OR(CI)"
AxisLabel <- "Odds ratio"
PLabel="p-value"


AxisTicks <- c(0.15, 0.5, 1.0, 1.5, 2.0)
IsLog <- FALSE
AxisTicks2 <- c(0.15, 0.5, 1.0, 1.5, 2.0)
IsLog <- FALSE
#AxisTicks <- c(-0.5, 0, 0.5, 1, 1.5, 2)
#IsLog <- FALSE
#XBlock=0
#XRowName=2
#XEvents=25
#XAxisLow=26
#XAxisHigh=70
#XHR=71
#XP=90
#FontMult=0.8

# example of sending multiple graphs to a file, with an overall title and a 
# footnote

# height and width are in inches, and are the size of A4 landscape
#outfile <- "two-plots2.pdf" #name the output
#pdf(outfile, height=8.26, width=11.69) #define as pdf and set the size
install.packages('devEMF')
library(devEMF)
emf(file = "plot.emf", width = 11, height = 7)

# split the page into 2 columns and allow outer margins for title and footnote
# the margin order is bottom, left, top, right
par(mfcol=c(1, 2), oma=c(3, 0, 4, 0))
# call the first graph; I need to move things about to make them fit
OxForestPlot0(datafile, "Chinese", EventsLabel, EstimateLabel, 
   PLabel, AxisLabel, AxisTicks, IsLog=TRUE, XEvents=NA, XBlock=0, XRowName = 22, XAxisLow=46, 
   XAxisHigh=91, XHR=92, XP=117)
# call the second graph
OxForestPlot0(datafile2, "Indian", EventsLabel, EstimateLabel, 
   PLabel, AxisLabel, AxisTicks2, IsLog=TRUE, XEvents=NA, XAxisLow=20, 
   XAxisHigh=65, XHR=66, XP= 91, XBlock=NA, XRowName = NA) ##the 'NA' removes redundant columns


# footnote, small, left adjusted, helps you find a graph you've made
mtext(side=1, line=1, outer=TRUE, adj=0, cex=0.75, paste(date(), outfile))
# side: 1=bottom, same as the margins
# line: distance from plot.  I allowed 3 lines at the bottom in the par() call
# outer: use the outer margins from the par() call
# adj: Adjustment: 0 for left, 0.5 for centre, 1 for right
# cex: font size multiplier

# main title: big, bold, centred
mtext(side=3, line=1, outer=TRUE, font=0.75, cex=1.25, "Odds ratios for self-reported diagnosis of selected NCDs among Chinese and Indian compared to Malay.")
graphics.off()





