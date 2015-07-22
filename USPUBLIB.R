#######################################################################################
#    United States Public Library Survey Data - Statistical Analysis 
#    
#    used data from 1999 to 2013
#######################################################################################


# install required R packages for the analysis
install.packages("plyr")
install.packages("dplyr")
install.packages("waffle")


library(dplyr)
library(waffle)
library(plyr)
install.packages("stringi")
install.packages("devtools")
library("devtools")
install_github("ropensci/plotly")
library(plotly)
set_credentials_file("KODISHALA", "dlsejh64f9")


# Load data into R studio from the .csv files, first set working directory.

setwd("/Users/hadoop/Documents/Data Analytics/US Public Libraries/Datasets")

# we are interested in only the following columns of the data from various years 

ColNames_99T02 <-  c("STABR","FSCSKEY","GEOCODE",	"POPU_UND","CENTLIB",	"BRANLIB","BKMOB","TOTSTAFF",	"TOTINCM","ELMATEXP",	"TOTEXPCO",
                    "TOTOPEXP",	"CAPITAL",	"BKVOL","AUDIO", "VIDEO",	"ELMATS",	"SUBSCRIP",	"HRS_OPEN",	"VISITS",	"REFERENC",	
                    "TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")


ColNames_03T09 <-  c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB",	"BRANLIB","BKMOB","TOTSTAFF",	"TOTINCM","PRMATEXP", "ELMATEXP",	"TOTEXPCO",
                   "TOTOPEXP",	"CAP_REV", "CAPITAL",	"BKVOL","EBOOK", "AUDIO", "VIDEO",	 "DATABASE",	"SUBSCRIP",	"HRS_OPEN",	"VISITS",	"REFERENC",	
                   "TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")

ColNames_10T12 <-  c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB",  "BRANLIB","BKMOB","TOTSTAFF",	"TOTINCM","PRMATEXP", "ELMATEXP",	"TOTEXPCO",
                     "TOTOPEXP",	"CAP_REV", "CAPITAL",	"BKVOL","EBOOK", "AUDIO_PH","AUDIO_DL" ,"VIDEO_PH",	"VIDEO_DL","DATABASE",	"SUBSCRIP",	"HRS_OPEN",	
                     "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")

ColNames_Final <- c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB",  "BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP",	"TOTEXPCO",
                    "TOTOPEXP",	"CAP_REV", "CAPITAL",	"BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",	"SUBSCRIP",	"HRS_OPEN",	
                    "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")

###############################################################################################
# load each year data and cleanse it for analysis
###############################################################################################

# year 1999 data
raw_yr1999 <- read.csv("Pupldf99.csv",header = TRUE)
sel_yr1999 <- raw_yr1999[,ColNames_99T02]
#str(sel_yr1999)

# following columns are not in the datasets from 1999 to 2002, so adding these and initializing the values to 0

sel_yr1999$PRMATEXP <- 0
sel_yr1999$CAP_REV  <- 0
sel_yr1999$EBOOK    <- 0
sel_yr1999$PRMATEXP <- as.double(sel_yr1999$PRMATEXP)
sel_yr1999$CAP_REV <- as.double(sel_yr1999$CAP_REV)
sel_yr1999$EBOOK  <- as.double(sel_yr1999$EBOOK)

# now re order the columns in the data frame in required order

sel_yr1999 <- sel_yr1999[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","ELMATS",	"SUBSCRIP",	"HRS_OPEN",	
                           "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]

# Rename following column names.. so column names will be same across years

colnames(sel_yr1999)[20] <- "DATABASE"

################
# year 2000 data
################
raw_yr2000 <- read.csv("Pupldf00.csv",header = TRUE)
sel_yr2000 <- raw_yr2000[,ColNames_99T02]
#str(sel_yr2000)

# following columns are not in the datasets from 1999 to 2002, so adding these and initializing the values to 0
sel_yr2000$PRMATEXP <- 0
sel_yr2000$CAP_REV  <- 0
sel_yr2000$EBOOK    <- 0

sel_yr2000$PRMATEXP <- as.double(sel_yr2000$PRMATEXP)
sel_yr2000$CAP_REV <- as.double(sel_yr2000$CAP_REV)
sel_yr2000$EBOOK  <- as.double(sel_yr2000$EBOOK)

# now re order the columns in the data frame in required order

sel_yr2000 <- sel_yr2000[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","ELMATS",  "SUBSCRIP",	"HRS_OPEN",	
                           "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]

# Rename following column names.. so column names will be same across years

colnames(sel_yr2000)[20] <- "DATABASE"

########################
# year 2001 data
#######################
raw_yr2001 <- read.csv("Pupld01b.csv",header = TRUE)
sel_yr2001 <- raw_yr2001[,ColNames_99T02]
#str(sel_yr2001)

sel_yr2001$PRMATEXP <- 0
sel_yr2001$CAP_REV  <- 0
sel_yr2001$EBOOK    <- 0

sel_yr2001$PRMATEXP <- as.double(sel_yr2001$PRMATEXP)
sel_yr2001$CAP_REV <- as.double(sel_yr2001$CAP_REV)
sel_yr2001$EBOOK  <- as.double(sel_yr2001$EBOOK)


# now re order the columns in the data frame in required order
sel_yr2001 <- sel_yr2001[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","ELMATS",  "SUBSCRIP",  "HRS_OPEN",	
                           "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]

# Rename following column names.. so column names will be same across years

colnames(sel_yr2001)[20] <- "DATABASE"


###########################
# year 2002 data
##########################
raw_yr2002 <- read.csv("Pupld02b.csv",header = TRUE)
sel_yr2002 <- raw_yr2002[,ColNames_99T02]
#str(sel_yr2002)


sel_yr2002$PRMATEXP <- 0
sel_yr2002$CAP_REV  <- 0
sel_yr2002$EBOOK    <- 0
sel_yr2002$PRMATEXP <- as.double(sel_yr2002$PRMATEXP)
sel_yr2002$CAP_REV <- as.double(sel_yr2002$CAP_REV)
sel_yr2002$EBOOK  <- as.double(sel_yr2002$EBOOK)


# now re order the columns in the data frame in required order
sel_yr2002 <- sel_yr2002[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","ELMATS",  "SUBSCRIP",  "HRS_OPEN",  
                           "VISITS",	"REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]


# Rename following column names.. so column names will be same across years

colnames(sel_yr2002)[20] <- "DATABASE"

#################################
# year 2003 data
#################################
raw_yr2003 <- read.csv("Pupld03a.csv",header = TRUE)
sel_yr2003 <- raw_yr2003[,ColNames_03T09]
#str(sel_yr2003)

#################################
# year 2004 data
#################################
raw_yr2004 <- read.csv("Pupld04a.csv",header = TRUE)
sel_yr2004 = raw_yr2004[,ColNames_03T09]
#str(sel_yr2004)

#################################
# year 2005 data
#################################
raw_yr2005 <- read.csv("Pupld05a.csv",header = TRUE)
sel_yr2005 <- raw_yr2005[,ColNames_03T09]
#str(sel_yr2005)

#################################
# year 2006 data
#################################
raw_yr2006 <- read.csv("Pupld06a.csv",header = TRUE)
sel_yr2006 <- raw_yr2006[,ColNames_03T09]
#str(sel_yr2006)

################################
# year 2007 data
################################
raw_yr2007 <- read.csv("Pupld07.csv",header = TRUE)
sel_yr2007 <- raw_yr2007[,ColNames_03T09]
#str(sel_yr2007)

################################
# year 2008 data
################################
raw_yr2008 <- read.csv("Pupld08a.csv",header = TRUE)
sel_yr2008 <- raw_yr2008[,ColNames_03T09]
#str(sel_yr2008)

################################
# year 2009 data
################################
raw_yr2009 <- read.csv("Pupld09a.csv",header = TRUE)
sel_yr2009 <- raw_yr2009[,ColNames_03T09]
#str(sel_yr2009)

################################
# year 2010 data
################################
raw_yr2010 <- read.csv("Pupld10a.csv",header = TRUE)
sel_yr2010 <- raw_yr2010[,ColNames_10T12]
#str(sel_yr2010)

# merge following columns into one column, in oder to keep these in sync with previous years
sel_yr2010$AUDIO <- sel_yr2010$AUDIO_PH + sel_yr2010$AUDIO_DL
sel_yr2010$VIDEO <- sel_yr2010$VIDEO_PH + sel_yr2010$VIDEO_DL


# now re order the columns in the data frame in required order
sel_yr2010 <- sel_yr2010[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                           "VISITS",  "REFERENC",	"TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]

################################
# year 2011 data
################################
raw_yr2011 <- read.csv("Pupld11b.csv",header = TRUE)
sel_yr2011 <- raw_yr2011[,ColNames_10T12]
#str(sel_yr2011)

# merge following columns into one column, in oder to keep these in sync with previous years
sel_yr2011$AUDIO <- sel_yr2011$AUDIO_PH + sel_yr2011$AUDIO_DL
sel_yr2011$VIDEO <- sel_yr2011$VIDEO_PH + sel_yr2011$VIDEO_DL

# now re order the columns in the data frame in required order
sel_yr2011 <- sel_yr2011[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                           "VISITS",  "REFERENC",  "TOTCIR",	"KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]

#######################################
# year 2012 data
#######################################
raw_yr2012 <- read.csv("Pupld12a.csv",header = TRUE)
sel_yr2012 <- raw_yr2012[,ColNames_10T12]
#str(sel_yr2012)

# merge following columns into one column, in oder to keep these in sync with previous years
sel_yr2012$AUDIO <- sel_yr2012$AUDIO_PH + sel_yr2012$AUDIO_DL
sel_yr2012$VIDEO <- sel_yr2012$VIDEO_PH + sel_yr2012$VIDEO_DL

# now re order the columns in the data frame in required order
sel_yr2012 <- sel_yr2012[c("STABR","FSCSKEY","GEOCODE",  "POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                           "VISITS",  "REFERENC",  "TOTCIR",  "KIDCIRCL",	"LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS",	"YR_SUB")]


####################################
# Now megre all years data into single data frame for the analysis
####################################

Input_Data <- rbind(sel_yr1999, sel_yr2000, sel_yr2001, sel_yr2002, sel_yr2003, sel_yr2004, sel_yr2005, sel_yr2006,
                    sel_yr2007, sel_yr2008, sel_yr2009, sel_yr2010, sel_yr2011, sel_yr2012)



# We need the data of the libraries which submitted the survery for all the years.. so we try the common library IDs across all the years data

BrList_yr1999 <- sel_yr1999[,2]
BrList_yr2000 <- sel_yr2000[,2]
BrList_yr2001 <- sel_yr2001[,2]
BrList_yr2002 <- sel_yr2002[,2]
BrList_yr2003 <- sel_yr2003[,2]
BrList_yr2004 <- sel_yr2004[,2]
BrList_yr2005 <- sel_yr2005[,2]
BrList_yr2006 <- sel_yr2006[,2]
BrList_yr2007 <- sel_yr2007[,2]
BrList_yr2008 <- sel_yr2008[,2]
BrList_yr2009 <- sel_yr2009[,2]
BrList_yr2010 <- sel_yr2010[,2]
BrList_yr2011 <- sel_yr2011[,2]
BrList_yr2012 <- sel_yr2012[,2]

Comm_BrList <- Reduce(intersect, list(BrList_yr1999,BrList_yr2000,BrList_yr2001,BrList_yr2002,
                                      BrList_yr2003,BrList_yr2004,BrList_yr2005,BrList_yr2006,
                                      BrList_yr2007,BrList_yr2008,BrList_yr2009,BrList_yr2010,
                                      BrList_yr2011,BrList_yr2012))


Input_Data <- subset(Input_Data, Input_Data$FSCSKEY %in% Comm_BrList)
#str(Input_Data)
###################################
# Remove all the rows which have one or other fields missing values
###################################
Input_Data_Cleans <- Input_Data[which(Input_Data$POPU_UND>=0 & Input_Data$CENTLIB>=0 & Input_Data$BRANLIB>=0 & Input_Data$BKMOB>=0
                      & Input_Data$TOTSTAFF>=0 & Input_Data$TOTINCM>=0 & Input_Data$PRMATEXP>=0 & Input_Data$ELMATEXP>=0
                      & Input_Data$TOTEXPCO>=0 & Input_Data$TOTOPEXP>=0 & Input_Data$CAP_REV>=0 & Input_Data$CAPITAL>=0
                      & Input_Data$BKVOL>=0 & Input_Data$EBOOK>=0 & Input_Data$AUDIO>=0 & Input_Data$VIDEO>=0
                      & Input_Data$DATABASE>=0 & Input_Data$SUBSCRIP>=0 & Input_Data$HRS_OPEN>=0 & Input_Data$VISITS>=0
                      & Input_Data$REFERENC>=0 & Input_Data$TOTCIR>=0 & Input_Data$KIDCIRCL>=0 & Input_Data$LOANTO>=0
                      & Input_Data$LOANFM>=0 & Input_Data$KIDATTEN>=0 & Input_Data$GPTERMS>=0),]

#str(Input_Data_Cleans)
#summary(Input_Data_Cleans)

###############################
# Aggregates of data for each year
###############################
Input_Yearly <- Input_Data_Cleans[c("POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                           "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                           "VISITS", "REFERENC", "TOTCIR", "KIDCIRCL", "LOANTO",	"LOANFM",	"KIDATTEN",	"GPTERMS")]


Input_Yearly$POPU_UND <- as.double(Input_Yearly$POPU_UND)
Input_Yearly$TOTINCM  <- as.double(Input_Yearly$TOTINCM)
Input_Yearly$PRMATEXP  <- as.double(Input_Yearly$PRMATEXP)
Input_Yearly$ELMATEXP  <- as.double(Input_Yearly$ELMATEXP)
Input_Yearly$TOTEXPCO  <- as.double(Input_Yearly$TOTEXPCO)
Input_Yearly$TOTOPEXP  <- as.double(Input_Yearly$TOTOPEXP)
Input_Yearly$CAP_REV  <- as.double(Input_Yearly$CAP_REV)
Input_Yearly$CAPITAL  <- as.double(Input_Yearly$CAPITAL)
Input_Yearly$BKVOL  <- as.double(Input_Yearly$BKVOL)
Input_Yearly$EBOOK  <- as.double(Input_Yearly$EBOOK)
Input_Yearly$AUDIO  <- as.double(Input_Yearly$AUDIO)
Input_Yearly$VIDEO  <- as.double(Input_Yearly$VIDEO)
Input_Yearly$DATABASE  <- as.double(Input_Yearly$DATABASE)
Input_Yearly$SUBSCRIP  <- as.double(Input_Yearly$SUBSCRIP)
Input_Yearly$HRS_OPEN  <- as.double(Input_Yearly$HRS_OPEN)
Input_Yearly$VISITS  <- as.double(Input_Yearly$VISITS)
Input_Yearly$REFERENC  <- as.double(Input_Yearly$REFERENC)
Input_Yearly$TOTCIR  <- as.double(Input_Yearly$TOTCIR)
Input_Yearly$KIDCIRCL  <- as.double(Input_Yearly$KIDCIRCL)
Input_Yearly$LOANTO  <- as.double(Input_Yearly$LOANTO)
Input_Yearly$LOANFM  <- as.double(Input_Yearly$LOANFM)
Input_Yearly$KIDATTEN  <- as.double(Input_Yearly$KIDATTEN)
Input_Yearly$GPTERMS  <- as.double(Input_Yearly$GPTERMS)

InData_Agg_Yearly <- aggregate(Input_Yearly, by=list(Year=Input_Data_Cleans$YR_SUB),FUN=sum)

################################
# Now check the data to see how it is spread over
################################
summary(InData_Agg_Yearly)         


####################################################################################
# Descriptive analysis to see the trends and patterns in the data
###################################################################################



#############################
# Now using external tool Plotly to draw line charts to see trends in
# Print Materials Expenses
# Electronci Materials Expenses
#############################
py <- plotly(username='KODISHALA', key='dlsejh64f9')

Yearly_PrMatExp <- InData_Agg_Yearly$PRMATEXP/1000000
Yearly_ElMatExp <- InData_Agg_Yearly$ELMATEXP/1000000


trace1 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_PrMatExp,
  type = "scatter"
)
trace2 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_ElMatExp,
  type = "scatter"
)


data <- list(trace1,trace2)
layout <- list(
  showlegend = TRUE,
  legend = list(
    x = 100,
    y = 1
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="Collection-Expenses", fileopt="overwrite"))
response$url

# plot is edited in the plotly, use following URL to view the plot
# https://plot.ly/~KODISHALA/121/us-public-libraries-expenses-on-collection/?share_key=mYYrk4rwLqQPbLVepMQWtc


#############################
# Now find out top 10 libraries in print material usage for 2012
# ###########################

PrMatElMat_yr2012 <- sel_yr2012[c("FSCSKEY","PRMATEXP", "ELMATEXP")]
PrMatElMat_yr2012_PMDesc <- PrMatElMat_yr2012[order(-PrMatElMat_yr2012$PRMATEXP),] 
PrMatElMat_yr2012_PMDesc_Top10 <- PrMatElMat_yr2012_PMDesc[1:10,1:2]

head(PrMatElMat_yr2012_PMDesc_Top10)
LibNames_yr2012 <- raw_yr2012[c("FSCSKEY","LIBNAME")]

PrMat_yr2012_Top10 <- merge(PrMatElMat_yr2012_PMDesc_Top10, LibNames_yr2012, by="FSCSKEY")


library(plotly)

  trace1 <- list(
    x = PrMat_yr2012_Top10$LIBNAME,
    y = PrMat_yr2012_Top10$PRMATEXP,
    marker = list(color = "rgb(142, 124, 195)"),
    type = "bar"
  )

  
data <- list(trace1)
  
layout <- list(
  title = "Print Material Expenses - Top-10 libraries of year 2012 ",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(tickangle = -45),
  yaxis = list(
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="bar-with-hover-text", fileopt="overwrite"))
url <- response$url
# this chart is available in the url https://plot.ly/~KODISHALA/124/print-material-expenses-top-10-libraries-of-year-2012/

##################################
# Now top 10 libaries in Electronic materials expenses - 2012
##################################

PrMatElMat_yr2012_EMDesc <- PrMatElMat_yr2012[order(-PrMatElMat_yr2012$ELMATEXP),] 
PrMatElMat_yr2012_EMDesc_Top10 <- PrMatElMat_yr2012_EMDesc[1:10,c(1,3)]

str(PrMatElMat_yr2012_EMDesc_Top10)
LibNames_yr2012 <- raw_yr2012[c("FSCSKEY","LIBNAME")]

ElMat_yr2012_Top10 <- merge(PrMatElMat_yr2012_EMDesc_Top10, LibNames_yr2012, by="FSCSKEY")


library(plotly)

trace1 <- list(
  x = ElMat_yr2012_Top10$LIBNAME,
  y = ElMat_yr2012_Top10$ELMATEXP,
  marker = list(color = "rgb(100, 124, 195)"),
  type = "bar"
)


data <- list(trace1)

layout <- list(
  title = "Electronic Material Expenses - Top-10 libraries of year 2012 ",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(tickangle = -45),
  yaxis = list(
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="Ele-with-hover-text", fileopt="overwrite"))
url <- response$url
url
# this chart is available in the url  https://plot.ly/~KODISHALA/129/electronic-material-expenses-top-10-libraries-of-year-2012/



################################################################################
#  Now using external tool Plotly to draw line chart to see the trend in following
#  Hours Open
#  Visits
#  References
#  Circulation
#  Kids Circulation
#  Kids Attendance
################################################################################
Yearly_HrOpen <-  InData_Agg_Yearly$HRS_OPEN/1000000
Yearly_Visits <-  InData_Agg_Yearly$VISITS/1000000
Yearly_Ref <-  InData_Agg_Yearly$REFERENC/1000000
Yearly_Circ <-  InData_Agg_Yearly$TOTCIR/1000000
Yearly_KidsCirc <-  InData_Agg_Yearly$KIDCIRCL/1000000
Yearly_KidsAttn <-  InData_Agg_Yearly$KIDATTEN/1000000

trace1 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_HrOpen,
  type = "scatter"
)
trace2 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Ref,
  type = "scatter"
)
trace3 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_KidsCirc,
  type = "scatter"
)
trace4 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_KidsAttn,
  type = "scatter"
)

data <- list(trace1,trace2,trace3,trace4)
layout <- list(
  showlegend = TRUE,
  legend = list(
    x = 100,
    y = 1
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="HrsOpen-Ref", fileopt="overwrite"))
response$url
# the chart is edited and it is avaialble in https://plot.ly/~KODISHALA/55/united-states-public-libraries-1999-2013/

################################################################################
#  Now using external tool Plotly to draw line chart to see the trend in following
#  Visits
#  Circulation
################################################################################
trace1 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Visits,
  type = "scatter"
)
trace2 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Circ,
  type = "scatter"
)

data <- list(trace1,trace2)
layout <- list(
  showlegend = TRUE,
  legend = list(
    x = 100,
    y = 1
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="Vists-Circ", fileopt="overwrite"))
response$url
# chart is edited and saved at https://plot.ly/~KODISHALA/61/united-states-public-libraries-visits-and-circulation/

#############################
# Now check how the following have changed over the years
# Book volume
# E-books
# Audios
# Videos
#############################
Yearly_BookVol <-  InData_Agg_Yearly$BKVOL/1000000
Yearly_Ebook <-  InData_Agg_Yearly$EBOOK/1000000
Yearly_Audio <-  InData_Agg_Yearly$AUDIO/1000000
Yearly_Video <-  InData_Agg_Yearly$VIDEO/1000000
Yearly_DB <-  InData_Agg_Yearly$DATABASE/1000000


trace1 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Ebook,
  type = "scatter"
)
trace2 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Audio,
  type = "scatter"
)
trace3 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_Video,
  type = "scatter"
)

trace4 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_DB,
  type = "scatter"
)
data <- list(trace1,trace2,trace3,trace4)
layout <- list(
  showlegend = TRUE,
  legend = list(
    x = 100,
    y = 1
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="Collection", fileopt="overwrite"))
response$url

# url saved in https://plot.ly/~KODISHALA/153/numbers-in-millions-vs-year/?share_key=rxozKpBlDsVZBhLnce8LJy
##############################################################
# Book volume 
##############################################################
trace1 <- list(
  x = InData_Agg_Yearly$Year,
  y = Yearly_BookVol,
  type = "bar"
)

data <- list(trace1)
layout <- list(
  showlegend = TRUE,
  legend = list(
    x = 100,
    y = 1
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="Databases", fileopt="overwrite"))
response$url

####################################################################################
##                  Create waffle chart to represent the collection (2012) 
####################################################################################

InData_Agg_Yearly_Yr2012 <- InData_Agg_Yearly[which(InData_Agg_Yearly$Year==2013),]

BKVOL_Yr2012 <- as.integer(InData_Agg_Yearly_Yr2012$BKVOL/1000000)
EBOOK_Yr2012 <- as.integer(InData_Agg_Yearly_Yr2012$EBOOK/1000000)
AUDIO_Yr2012 <- as.integer(InData_Agg_Yearly_Yr2012$AUDIO/1000000)
VIDEO_Yr2012 <- as.integer(InData_Agg_Yearly_Yr2012$VIDEO/1000000)
DB_Yr2012 <- as.integer(InData_Agg_Yearly_Yr2012$DATABASE/1000000)


library(waffle)
Collection <- c(`Books (767.75)`= BKVOL_Yr2012, `E-books (84.03)`= EBOOK_Yr2012, 
             `Audios (74.27)`= AUDIO_Yr2012, `Videos (57.10)`=VIDEO_Yr2012, `Databases (0.45)`=1)
waffle(Collection, rows=20, size=0.5, 
       colors=c("#fbec5d", "#9d7824", "#a4c639", "#c293f9","#ff0000"), 
       title="United States Public Libraries - collection  - 2012", 
       xlab="1 square == 1 million")


###### check per capita Print Materials, Electronic Materials etc.. compare with region per captia, national per capita


######################################################################################################
#                                                    Predictive Analysis
#
######################################################################################################

####################################################################################
##                   United States Public Libraries - Correlation matrix for 2012
####################################################################################
                     
##############################################################
# Aggregate data for year 2012 by geo code, this is required for co-relation
#
#############################################################

library(plyr)
InData_Year2012 <- Input_Data_Cleans[which(Input_Data_Cleans$YR_SUB == '2013'),]
Input_Data2012  <- InData_Year2012[c("GEOCODE","POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                                     "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                                     "VISITS", "REFERENC", "TOTCIR", "KIDCIRCL", "LOANTO",  "LOANFM",  "KIDATTEN",	"GPTERMS")]



Input_Data2012$GEOCODE <- revalue(Input_Data2012$GEOCODE, c("CI1"="1","CI2"="2","CO1"="3","CO2"="4","MA1"="5","MA2"="6","MC1"="7","MC2"="8","SD1"="9","SD2"="10","OTH"="11"))

Input_Data2012$GEOCODE <- as.integer(Input_Data2012$GEOCODE)

install.packages("reshape2")
library(reshape2)

require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)

corrmatrix<-cor(Input_Data2012)
corrdata=as.data.frame(corrmatrix)
corrdata$Variable1=names(corrdata)
corrdatamelt=melt(corrdata,id="Variable1")
names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
corrmatplot$addParams(height = 450, width=1000)
corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
corrmatplot$guides(x = list(numticks = 3))
corrmatplot$addParams(staggerLabels=TRUE)
corrmatplot$save("corrmatplotstate.html",cdn=T)
corrmatplot

###############################################################################
# Now generate heatmap of different variables for the year 2012 for each state
###############################################################################
install.packages("RPMG")
library(plyr)
Input_St_Data2012  <- InData_Year2012[c("POPU_UND","CENTLIB","BRANLIB","BKMOB","TOTSTAFF",  "TOTINCM","PRMATEXP", "ELMATEXP","TOTEXPCO",
                                     "TOTOPEXP","CAP_REV","CAPITAL","BKVOL","EBOOK", "AUDIO" ,"VIDEO","DATABASE",  "SUBSCRIP",  "HRS_OPEN",  
                                     "VISITS", "REFERENC", "TOTCIR", "KIDCIRCL", "LOANTO",  "LOANFM",  "KIDATTEN",  "GPTERMS")]


InData_StAgg_Yr2012 <- aggregate(Input_St_Data2012, by=list(State=InData_Year2012$STABR),FUN=sum)


InData_StAgg_Yr2012$State <- revalue(InData_StAgg_Yr2012$State, c("AL"="Alabama","AK"="Alaska","AZ"="Arizona",
                                                                  "AR"="Arkansas","CA"="California",
                                                                  "CO"="Colorado","CT"="Connecticut",
                                                                   "DE"="Delaware","DC"="District of Columbia",
                                                                   "FL"="Florida","GA"="Georgia","HI"="Hawaii",
                                                                   "ID"="Idaho","IL"="Illinois","IN"="Indiana",
                                                                   "IA"="Iowa","KS"="Kansas","KY"="Kentucky",
                                                                   "LA"="Louisiana","ME"="Maine","MD"="Maryland",
                                                                   "MA"="Massachusetts","MI"="Michigan",
                                                                   "MN"="Minnesota","MS"="Mississippi",
                                                                    "MO"="Missouri","MT"="Montana","NE"="Nebraska",
                                                                    "NV"="Nevada","NH"="New Hampshire",
                                                                    "NJ"="New Jersey","NM"="New Mexico",
                                                                    "NY"="New York","NC"="North Carolina",
                                                                    "ND"="North Dakota","OH"="Ohio","OK"="Oklahoma",
                                                                    "OR"="Oregon","PA"="Pennsylvania",
                                                                    "RI"="Rhode Island","SC"="South Carolina",
                                                                    "SD"="South Dakota","TN"="Tennessee",
                                                                    "TX"="Texas","UT"="Utah","VT"="Vermont",
                                                                    "VA"="Virginia","WA"="Washington",
                                                                    "WV"="West Virginia","WI"="Wisconsin",
                                                                     "WY"="Wyoming",
                                                                     "GU"="Guam","MP"="Northern Mariana Islands",
                                                                     "PR"="Puerto Rico","VI"="Virgin Islands"))
str(InData_StAgg_Yr2012)

install.packages("sclaes")
library(scales)

# heatmap of variables and States
statemelt=ddply(melt(InData_StAgg_Yr2012),.(variable),transform,rescale=rescale(value))
names(statemelt)=c("State","Params","value","rescale")
hmap <- rPlot(State ~ Params, color = 'rescale', data = statemelt, type = 'tile')
hmap$addParams(height = 600, width=1000)
hmap$guides(reduceXTicks = FALSE)
hmap$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
hmap$guides(y = list(numticks = length(unique(statemelt$State))))
hmap$guides(x = list(numticks = 3))
hmap$save("heatmapstate.html",cdn=T)
hmap


#######################################################################################
# Applying Clustering technique on the Statewise aggregate data to cluster these in to 5 clusters.
#######################################################################################
install.packages("rjson")
library(rjson)
set.seed(123)

summary(InData_StAgg_Yr2012)

# From the above summary, it is evident some of the colums have wider difference in their values than the other columns. So now \
# now we will normalize the data

# from the summary we see that some of the attributes have wide range of values, so normalizing the data
InData_StAgg_Yr2012_norm <- as.data.frame(cbind(InData_StAgg_Yr2012[ ,1], scale(InData_StAgg_Yr2012[ ,2:28]))) 
summary(InData_StAgg_Yr2012_norm)

str(InData_StAgg_Yr2012_norm)
kmeansdata=kmeans(InData_StAgg_Yr2012_norm[c(-1)],5)  # Decided on 5 for interpretation
# get cluster means 
meanvars=aggregate(InData_StAgg_Yr2012_norm[c(-1)],by=list(kmeansdata$cluster),FUN=mean)
# append cluster assignment
InData_StAgg_Yr2012_clust <- data.frame(InData_StAgg_Yr2012, kmeansdata$cluster)

# plotting states/uts by cluster number
stategpplot=dPlot(x="State", y="kmeansdata.cluster",groups="kmeansdata.cluster",data=InData_StAgg_Yr2012_clust,
                  type="bar",height=475,width=700,bounds = list(x=50, y=10, width=600, height=300))
stategpplot$yAxis(type="addCategoryAxis")
stategpplot$xAxis(type="addCategoryAxis",orderRule="kmeansdata.cluster")
stategpplot$save("stategpplot.html",cdn=T)
stategpplot


str(meanvars)

############## Parallel Plot#############
#names(meanvars)=c("Group","Pop","CLib","BLib","Book_Mobi",
#                  "Staff","Income","PrMatExp","ElecMatExp",
#                   "TotColExp","TotOpExp","CapRev","Captial","Books",
#                   "E_books","Audio","Video","Database","Subscrip","HrsOpen","Visits","References",
#                    "TotCirc","KidsCir","LoansTo","LoansFrom","KidsAttn","CompTerm")

names(meanvars)=c("Group","Population","CentLib","BranLib","BookMob",
                  "Staff","Income","PrMatExp","ElecMatExp",
                  "TotColExp","TotOpExp","CapRev","Capital","Books",
                  "Ebooks","Audio","Video","Databases","Subscrip","HrsOpen","Visits","Refer",
                  "TotCirc","KidsCir","LoansTo","LoansFrom","KidsAttn","CompTeminals")



parrstate <- rCharts$new()
parrstate$field('lib', 'parcoords')
parrstate$set(padding = list(top = 25, left = 5, bottom = 10, right = 0), width=1380, height=700)
parrstate$set(data = toJSONArray(meanvars, json = F), 
                colorby = 'Group', 
                range = range(meanvars$Group),
                colors = c('red','green')
)
parrstate$setLib("/Users/hadoop/Documents/Data Analytics/US Public Libraries/Datasets/parcoords")
parrstateut$save("parallelplotstate.html", cdn=T)
parrstate
#  It was tricky to get the parallel coordinates to save. Using the previous command, 
# generate a viz of the plot, copy the source code (view html source) of the page generated, 
# and paste it into a new text editor and save as html file. That'll take care of it. 
#It has some links to the js libs in the parcoords folder... they have to be in place for it to work. 











