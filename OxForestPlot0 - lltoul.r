# code to set up the forest plot functions

# insert this into your program by using
# source("...\\OxForestPlot0.r")
# where ... is replaced with the path to this file

# draw square boxes of precise size, with known locations of sides
SquareBox<-function(xlocs, ylocs, sizes, collist, SizeByX=TRUE)
    {

#   xlocs: vector of x-locations
#   ylocs: vector of y-locations
#   sizes: length of the box sides, on either the X or the Y scales
#   SizeByX: TRUE if sizes are on the X scale, FALSE if the sizes are on 
#      the Y scale

    # function to plot _square_ boxes of the given size

    # plot sizes in paper and user coordinates
    paperx<-par("pin")[1]
    papery<-par("pin")[2]
    userx<-par("usr")[2]-par("usr")[1]
    usery<-par("usr")[4]-par("usr")[3]

    # When plotted, y-coordinate size of box is to be sizes*boxparmy
    #    similarly x-coordinate size is sizes*boxparmx
    if (SizeByX==TRUE)
       {
       boxparmx <- 1
       boxparmy <- (paperx/papery)*(usery/userx)
       } else
       {
       boxparmy <- 1
       boxparmx <- (papery/paperx)*(userx/usery)
       }

    xleft <- xlocs - (0.5*sizes*boxparmx)
    xright <- xlocs + (0.5*sizes*boxparmx)
    ytop <- ylocs + (0.5*sizes*boxparmy)
    ybottom <- ylocs - (0.5*sizes*boxparmy)
    
    # draw the squares with proper sharp corners, using 'mitre' joins
    # then reset joins to what they were
    oldjoin<-par("ljoin")
    par(ljoin="mitre")
    rect(xleft, ybottom, xright, ytop, col=collist)
    par(ljoin=oldjoin)

    # make a data frame of the box sides, then return them
    boxedges<-data.frame(left=xleft, right=xright, bottom=ybottom, top=ytop)
    return(boxedges)
    }

# p-value formatting - no brackets, no 'p'
PFormat <- function(pvals)
   {
   # pval is the numeric p-value
   # ptext is the formatted text
   # help(prettyNum) will show you what prettyNum is doing
   # paste(..., sep="") puts the bits together with nothing in between them
   # (in the end, I could have done this more simply, but it is done now)

   plabs <- character(length(pvals))
   for (ip in seq(1, length(pvals)))
      {
      pval <- pvals[ip]
      if (pval>=0.1) ptext<-prettyNum(pval, format="fg", digits=1)
      if (pval<0.1&&pval>=0.01) ptext<-prettyNum(pval, format="fg", digits=1)
      if (pval<0.01&&pval>=0.001) ptext<-prettyNum(pval, format="fg", digits=1)
      if (pval<0.001) ptext<-"<0.001"
      plabs[ip] <- ptext
      }
   return(plabs)
   }

# p-value formatting (with brackets, not currently used)
PFormatOld <- function(pvals)
   {
   # pval is the numeric p-value
   # ptext is the formatted text
   # help(prettyNum) will show you what prettyNum is doing
   # paste(..., sep="") puts the bits together with nothing in between them

   plabs <- character(length(pvals))
   for (ip in seq(1, length(pvals)))
      {
      pval <- pvals[ip]
      if (pval>=0.1) ptext<-paste("(p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
      if (pval<0.1&&pval>=0.01) ptext<-paste("(p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
      if (pval<0.01&&pval>=0.001) ptext<-paste("(p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
      if (pval<0.001) ptext<-"(p<0.001)"
      plabs[ip] <- ptext
      }
   return(plabs)
   }

# Function to draw forest plot diamonds
Diamonds<-function(left, right, at, yloc, height, border=NULL, col=NULL)
   {
   for (i in 1:length(left)) 
      {
      x<-c(left[i], at[i], right[i], at[i])
      y<-c(yloc[i], yloc[i]+(height[i]/2), yloc[i], yloc[i]-(height[i]/2))
      polygon(x, y, border=border[i], col=col[i])
      }
   }

# The following lines, commented out, are my test settings for the function
#datafile <- "K:\\vep\\PSC\\Paul\\for jennifer\\forest plot function\\forest plot example data.csv"
#MainTitle <- "Silly things"
#EventsLabel <- "Events"
#EstimateLabel <- "Odds ratio"
#AxisLabel <- "Odds ratio"
##AxisTicks <- c(0.5, 1, 2, 4, 8)
##IsLog <- TRUE
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
#PLabel="p\255value"

#OxForestPlot0(datafile, MainTitle, EventsLabel, EstimateLabel, PLabel, AxisLabel, AxisTicks, IsLog=FALSE)

OxForestPlot0 <- function(datafile, MainTitle, EventsLabel, EstimateLabel, PLabel, AxisLabel, AxisTicks, 
   IsLog=TRUE, FontMult=0.8, 
   XBlock=0, XRowName=2, XEvents=25, XAxisLow=26, XAxisHigh=70, XHR=71, XP=90)
   {
   # datafile:                full path and file name for the data file
   # MainTitle:               main title to display.  Separate lines with \n in the text ("First line\nSecond line")
   # EventsLabel:             Label above the number of events
   # EstimateLabel:           Label above the estimates
   # AxisLabel:               x-axis label
   # AxisTicks:               tick mark locations on the x-axis.  The lowest and highest define the range of the axis
   # IsLog:                   TRUE for a log scale, FALSE for a linear scale.  Displayed values will be exponentials
   #                             of the supplied estimates for a log scale.  
   #                             The default is TRUE.  To supply false, add "..., IsLog=FALSE, ..." to the function call
   # FontMult:                Font size multiplier, default 0.8

   # positioning: The variables from XBlock to XP give the horizontal position of the columns on a 0-100 scale.  
   # they have defaults, and to change them, put, for example "..., XAxisHigh=55, ..." in the function call
   # XBlock:                  block name location, left justified
   # XRowName:                row name location, left justified
   # XEvents:                 location for number of events, _right_ justified
   # XAxisLow:                location for left hand end of axis
   # XAxisHigh:               location for right hand end of axis
   # XHR:                     location for estimate and confidence interval, left-justified
   # XP:                      location for P-values, left justified

#  about the values in datfile:
#  block:                     The block name to display.  The program leaves extra space before a block.
#                             If the block name is "Overall", the point will be a diamond
#  rowname:                   Name of this row
#  estimate:                  The value to be shown
#  se:                        Standard error of estimate.  Defines the confidence interval and the
#                             (non-diamond) box sizes
#  p:                         The numerical p-value


#  read the data file
   indata <- read.csv(datafile)

   # first I need to work out the y-locations of everything
   rows_between_blocks <- 2

   # strip out spaces from blocks to ensure that I do not count blocks of " " as being
   # meaningful
   block_no_spaces <- gsub(" ", "", indata$block, fixed = TRUE)
   newblock <- nchar(block_no_spaces)>0

#  y-locations
#  lowest point is at 1
#  each row is 1 higher than the last, but
#  if there is a block name in a row, the row above is an extra rows_between_blocks higher still
#  ... but not by as much for the "Overall" row, if any
   this_y <- 1
   ylocs <- numeric(nrow(indata))
   for (ipt in seq(nrow(indata), 1, -1))
      {
      ylocs[ipt] <- this_y
      this_y <- this_y + 1
      if (newblock[ipt]) this_y <- this_y + rows_between_blocks
      if (indata$block[ipt]=="Overall") this_y <- this_y - 1  
      }

#  reset left and right margins to 0, set font multiplier
   omars <- par("mar")
   ocex <- par("cex")
   newmars <- c(omars[1], 0, omars[3], 0)
   newcex <- FontMult
   par(mar=newmars, cex=newcex)

#  set the "paper" coordinate system
#  top of the graph is 1 above the highest row location
   plot.new()
   plot.window(xlim=c(0, 100), ylim=c(0, max(ylocs)+1))

#  x-locations for the columns are defined by the user, but I need to calculate 
#  x-locations for the points on their own scale, log or non-log

#  conversion factor of distance from given information to paper location
   if (IsLog) xaxisvalues <- log(AxisTicks) else
      xaxisvalues <- AxisTicks
   xf <- (XAxisHigh-XAxisLow)/(max(xaxisvalues)-min(xaxisvalues))
#  location of 1(log scale) or 0 (linear scale) on the axis
   xax <- XAxisLow - xf*min(xaxisvalues)

#  plotting locations
   xlocs <- xax + xf*indata$estimate
   xlow <- xax + xf*(indata$estimate - 1.96*indata$se)
   xhigh <- xax + xf*(indata$estimate + 1.96*indata$se)

#  point sizes (largest square point 0.8 of the distance between rows)
   squares <- indata$block!="Overall"
   sizes <- 0.8*min(indata$se[squares])/indata$se[squares]

#  from the nominal top and left...
#  title
   title(main=MainTitle, cex.main=1.8)
#  block names, one higher than their nominal row, unless they are "Overall"
   blocky <- ylocs+1
   blocky[!squares] <- ylocs[!squares]
   text(XBlock, blocky, indata$block, adj=0, font=2)
#  row names
   text(XRowName, ylocs, indata$rowname, adj=0)
#  n-events (gap every three digits)
   evlabs <- formatC(indata$events, digits=0, big.mark=" ")
   text(XEvents-0.5*max(strwidth(evlabs)), max(ylocs)+1, EventsLabel, font=2)
   text(XEvents, ylocs, evlabs, adj=1)

#  now the plot itself, starting with the axis
   axis(side=1, at=xax+xf*xaxisvalues, labels=AxisTicks, font.axis=2, pos=0)
#  line at 0/1, if appropriate
   if (XAxisLow <= xax && XAxisHigh >=xax) segments(xax, 0, xax, max(ylocs)+0.5)
#  draw points
   boxsides <- SquareBox(xlocs[squares], ylocs[squares], sizes, "black", SizeByX=FALSE)
#  CI
   CIcol <- ifelse(xlow[squares]<boxsides$left, "black", "white")
   segments(xlow[squares], ylocs[squares], xhigh[squares], ylocs[squares], col=CIcol)

#  diamonds, if any
   if (any(!squares))
      {
      Diamonds(xlow[!squares], xhigh[!squares], xlocs[!squares], ylocs[!squares], 
         0.8)
      segments(xlocs[!squares], ylocs[!squares]+0.4, xlocs[!squares], max(ylocs)+0.5, 
         lty="dotted")
      }

#  x-axis label
   mtext(side=1, at=0.5*(XAxisLow+XAxisHigh), AxisLabel, font=2, line=2)

#  display estimate and CI
   if (IsLog) vallabs <- paste0(formatC(exp(indata$estimate), format="f", digits=2), " (", 
         formatC(indata$ll, format="f", digits=2), " - ", 
         formatC(indata$ul, format="f", digits=2), ")") else
      vallabs <- paste0(formatC(indata$estimate, format="f", digits=2), " (", 
         formatC(indata$ll, format="f", digits=2), " - ", 
         formatC(indata$ul, format="f", digits=2), ")")
   text(XHR, ylocs, vallabs, adj=0)
   text(XHR+0.5*max(strwidth(vallabs)), max(ylocs)+1, EstimateLabel, font=2)


#  display formatted p-value
   plabs <- PFormat(indata$p)
   text(XP, ylocs, plabs, adj=0)
   text(XP+0.5*max(strwidth(plabs)), max(ylocs)+1, PLabel, font=2)

#  reset margins and font size to previous value
   par(mar=omars, cex=ocex)
}

# graphics devices
# pdf()
# win.metafile()  # (i.e. emf)
# png()
# tiff()



    
