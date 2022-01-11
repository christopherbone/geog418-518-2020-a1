#######
## For this lab you will need the following libraries:
## terra, lubridate, e1071, gridExtra, gtable, grid, ggplot2, dplyr, bcmaps, tmap, and sf 
#####
#Install Libraries
install.packages("terra")
install.packages("lubridate")
install.packages("e1071")
install.packages("gridExtra")
install.packages("gtable")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages("tmap")
install.packages("sf")


#####
#Load Libraries
library("terra")

#####
#Set working directory
dir <- "YOUR WORKING DIRECTORY"
setwd(dir)
getwd()

#####
#Read in data and extract attribute table
shp <- vect("./H_FIRE_PNT_point.shp") #read in shp file from current (".") working directory
shp

df <- as.data.frame(shp) #Extract the attribute table as a dataframe, store in variable
class(df) #ensure new variable is a dataframe

#####
#Inspect data
names(df) #see column names
head(df) #see first 6 rows of data

typeof(df$VARIABLEFORYEAR) #what is the data type of year?
range(df$VARIABLEFORYEAR) #How many years of data is there?

#We will need to subset the data into an given year
df <- subset(df, df$FIRE_YEAR == YOURASSIGNEDYEAR)

#We will also need to know the date and the month of each fire
df$IGN_DATE <- as.Date(df$IGN_DATE, "%Y%m%d") #Convert to date string
df$IGN_Day <- yday(df$DATEVAR) #Make new column with day of year for the ignition date
df$IGN_Month <- month(df$DATEVAR, label = TRUE, abbr = TRUE) #create new column with month of ignition

#Check the range of day and month
range(df$DAYVAR) #Everything seem ok?
unique(df$MONTHVAR) #Any months seem odd?

#Remove values with an NA date
df <- subset(df, !is.na(df$DATEVAR))

#Check the range of IGN Date
range(df$DATEVAR) #Anything odd?

#Calculate new year column from date and check range
df$IGN_Year <- year(df$DATEVAR)
range(df$YEARVAR)



#####
#Clean data to appropriate year and calculate descriptive statistics
df_year <- df[which(df$VARIABLEFORYEAR == YOURASSIGNEDYEAR),] #subset only your year

#Mean
meanPop <- mean(df_year$SIZE) #This could produce a wrong value (NA) due to a single NA value in data
meanPop <- mean(df_year$SIZE, na.rm = TRUE) #Use na.rm = TRUE to ignore NA values in calculation

#Check this website for the correct day numbers of your year: https://miniwebtool.com/day-of-the-year-calculator/
meanSummer <- mean(subset(df_year, IGN_Day >= DDD & IGN_Day <= DDD)$FIRESIZE) #Calculate the mean fire size between July 1 and Aug 31

#Standard Deviation
sdPop <- sd(df_year$Size, na.rm = TRUE) #Calculate the SD, ignoring NA values
sdSummer <- sd(subset(df_year, IGN_Day >= DDD & IGN_Day <= DDD)$FIRESIZE) #Calculate the SD, ignoring NA values only for the summer months

#Mode
modePop <- as.numeric(names(sort(table(df_year$SIZE), decreasing = TRUE))[1]) #make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)

#It will be cleaner if we use a new variable for the summer data
df_Summer <- subset(df_year, IGN_Day >= DDD & IGN_Day <= DDD) #Make new variable to hold summer data
#make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)
modeSummer <- as.numeric(names(sort(table(df_Summer$SIZE), decreasing = TRUE))[1])

#Median
medPop <- median(df_year$SizeOfFire, na.rm = TRUE)
medSummer <- median(df_Summer$SizeOfFire, na.rm = TRUE)

#Skewness
skewPop <- skewness(df_year$SizeOfFire, na.rm = TRUE)[1]
skewSummer <- skewness(df_Summer$SizeOfFire, na.rm = TRUE)[1]

#Kurtosis
kurtPop <- kurtosis(df_year$SizeOfFire, na.rm = TRUE)[1]
kurtSummer <- kurtosis(df_Summer$SizeOfFire, na.rm = TRUE)[1]

#CoV
CoVPop <- (sdPop / meanPop) * 100
CoVSummer <- (sdSummer / meanSummer) * 100

#Normal distribution test
normPop_PVAL <- shapiro.test(df_year$SizeOfFire)$p.value
normSummer_PVAL <- shapiro.test(df_Summer$SizeOfFire)$p.value

#####
#Create a table of descriptive stats

samples = c("Population", "Summer") #Create an object for the labels
means = c(meanPop, meanSummer) #Create an object for the means
sd = c(sdPop, sdSummer) #Create an object for the standard deviations
median = c(medPop, medSummer) #Create an object for the medians
mode <- c(modePop, modeSummer) #Create an object for the modes
skewness <- c(skewPop, skewSummer) #Create an object for the skewness
kurtosis <- c(kurtPop, kurtSummer) #Create an object for the kurtosis
CoV <- c(CoVPop, CoVSummer) #Create an object for the CoV
normality <- c(normPop_PVAL, normSummer_PVAL) #Create an object for the normality PVALUE

##Check table values for sigfigs?

data.for.table1 = data.frame(ADD VARIABLES FOR YOUR TABLE)
data.for.table2 = data.frame(ADD VARIABLES FOR YOUR TABLE)

#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: CAPTION FOR YOUR TABLE", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: CAPTION FOR YOUR TABLE", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)



grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Output_Table1.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

png("Output_Table2.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()

#####
#Create and Print a histogram
png("Output_Histogram.png")
hist(df_year$SizeOfFire, breaks = 30, main = "TITLE OF YOUR HISTOGRAM", xlab = "LABEL FOR AXIS") #Base R style
dev.off()

#LOOK FOR AND CORRECT AN ERROR IN THE CODE BELOW
histogram <- ggplot(df_year, aes(x = SizeOfFire)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "TITLE OF HISTOGRAM", x = "AXIS TITLE", y = "AXIS TITLE", caption = "Figure X: MAKE CAPTION FOR FIGURE") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 700, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100

png("Output_Histogram_ggplot.png")
histogram
dev.off()

#####
#Creating bar graph
#LOOK FOR AND CORRECT AN ERROR IN THE CODE BELOW
sumMar = sum(subset(df_year, IGN_Month == "Mar")$FIRESIZE, na.rm = TRUE) #create new object for March
sumApr = sum(subset(df_year, IGN_Month == "Apr")$FIRESIZE, na.rm = TRUE) #create new object for April
sumMay = sum(subset(df_year, IGN_Month == "May")$FIRESIZE, na.rm = TRUE) #create new object for May
sumJun = sum(subset(df_year, IGN_Month == "Jun")$FIRESIZE, na.rm = TRUE) #create new object for June
sumJul = sum(subset(df_year, IGN_Month == "Jul")$FIRESIZE, na.rm = TRUE) #create new object for July
sumAug = sum(subset(df_year, IGN_Month == "Aug")$FIRESIZE, na.rm = TRUE) #create new object for August
sumSep = sum(subset(df_year, IGN_Month == "Sep")$FIRESIZE, na.rm = TRUE) #create new object for September
months = c("Mar","Apr","May","Jun","Jul", "Aug", "Sep")  #Create labels for the bar graph

png("Output_BarGraph.png") #Create an object to print the bar graph 
barplot(c(sumMar,sumApr,sumMay, sumJun, sumJul, sumAug, sumSep), names.arg = months, 
        main = "TITLE FOR BAR GRAPH", ylab = "AXIS TITLE", xlab = "AXIS TITLE") #Create the bar graph
dev.off() #Print bar graph

#Total Size by Month GGPLOT
#LOOK FOR AND CORRECT ERRORS IN THE CODE BELOW
barGraph <- df_year %>% #store graph in bargraph variable and pass data frame as first argument in next line
  group_by(IGN_Month) %>% #use data frame and group by month and pass to first argument in next line
  summarise(sumSize = sum(SizeOfFire, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  ggplot(aes(x = IGN_Month, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity") + #make bar chart with the Y values from the data (identity)
  labs(title = "TITLE FOR BAR GRAPH", x = "AXIS TITLE", y = "AXIS TITLE", caption = "Figure X: CAPTION FOR YOUR FIGURE") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) #set title to center and bold
barGraph

png("Output_BarGraph_GG.png")
barGraph
dev.off()



#####
#Creating maps
bc <- vect(bc_neighbours()) #Get shp of BC bounds
crs(bc)
bc <- spTransform(bc, crs("+init=epsg:4326")) #project to WGS84 geographic (Lat/Long)

bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province

#####
#Making Maps with tm package
#Make spatial object out of data
#LOOK FOR AND CORRECT ERRORS IN THE CODE BELOW
coords <- cbind(df_year$LONG, df_year$LAT) #Store coordinates in new object
crs <- crs("+init=epsg:4326") #store the coordinate system in a new object

firePoints <- vect(coords, atts = df_year, crs = crs) #Make new spatial Points object using coodinates, data, and projection

map_TM <- tm_shape(sf::st_as_sf(bc)) + #make the main shape
  tm_fill(col = "gray50") +  #fill polygons
  tm_shape(sf::st_as_sf(firePoints)) +
  tm_symbols(col = "red", alpha = 0.3) +
  tm_layout(title = "Title of Map", title.position = c("LEFT", "BOTTOM"))

map_TM

png("TmMap.png")
map_TM
dev.off()

#How would you calculate the mean center point location and add it to a map?
