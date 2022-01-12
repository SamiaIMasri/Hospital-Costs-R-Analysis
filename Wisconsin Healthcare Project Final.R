
#================================================================================================================================
#   Data: City of Wisconsin, patients aged 0 -17 years, Inpatient Data Hospital Records
#   Objective: Analyze the data to research Healthcare Costs and Utilization
#================================================================================================================================
rm(list = ls())             #Clear/Initialize Environment
#set working directory                           
setwd("/Users/samiai.masri/Documents/MastersProgram/4_Data Science with R/Project/Projects for Submission/Healthcare/Healthcare")
getwd()

library(pacman)             #Load packages
p_load(DataExplorer,dplyr,ggplot2,magrittr,plyr,plyr,sqldf,scales,tidyr,extrafont,grid, rvg,tidyverse,here,glue,officer,
       gt,rio,flextable,ggpubr)
#================================================================================================================================
#My Functions
my_percent <- function(x, digits = 1, format = "f", ...) 
{paste0(formatC(100 * x, format = format, digits = digits, ...), "%")}  

ks <- function (x) {number_format(accuracy = 1,scale = 1/1000,suffix = "K", prefix = "$")(x)}  
#===============================================================================================================================
HospitalData <- read.csv("HospitalCosts.csv")   #Read in the data file
str(HospitalData)                               #view structure of the data
summary(HospitalData)                           #view summary of the data set

# summary(HospitalData)                         #view summary of the data set
# AGE             FEMALE           LOS              RACE           TOTCHG          APRDRG     
# Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :1.000   Min.   :  532   Min.   : 21.0  
# 1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 2.000   1st Qu.:1.000   1st Qu.: 1216   1st Qu.:640.0  
# Median : 0.000   Median :1.000   Median : 2.000   Median :1.000   Median : 1536   Median :640.0  
# Mean   : 5.086   Mean   :0.512   Mean   : 2.828   Mean   :1.078   Mean   : 2774   Mean   :616.4  
# 3rd Qu.:13.000   3rd Qu.:1.000   3rd Qu.: 3.000   3rd Qu.:1.000   3rd Qu.: 2530   3rd Qu.:751.0  
# Max.   :17.000   Max.   :1.000   Max.   :41.000   Max.   :6.000   Max.   :48388   Max.   :952.0  
#                                                   NA's   :1                    

#Note from Summary we can see RACE has one NA (missing data)
HospitalData$RACE[is.na(HospitalData$RACE)]<-median(HospitalData$RACE,na.rm=TRUE) #Take median value, update missing 

#===============================================================================================================================


# 1. To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and
# has the maximum expenditure.

#create an age group column 'AGEGRP'
HospitalData <- mutate(HospitalData, AGEGRP = case_when (AGE >= 0 & AGE <= 2  ~ "00-02",
                                                         AGE >= 3 & AGE <= 5  ~ "03-05",
                                                         AGE >= 6 & AGE <= 8  ~ "06-08",
                                                         AGE >= 9 & AGE <= 11 ~ "09-11",
                                                         AGE >= 12 &AGE <= 14 ~ "12-14",
                                                         AGE >= 15 &AGE <= 17 ~ "15-17",
                                                         TRUE ~ "NA"),  
                       Sex  = case_when (FEMALE == 0 ~ "Sex 0",
                                         FEMALE == 1 ~ "Sex 1",
                                         TRUE ~"NA")) #TRUE is the equivalent of ELSE STATEMENT)
head(HospitalData)
summary(HospitalData)
str(HospitalData)
HospitalData$AGEGRP <- as.factor(HospitalData$AGEGRP) 
HospitalData$Sex <- as.factor(HospitalData$Sex) 
summary(HospitalData)


h_Visits<- (ggplot(data=HospitalData, aes(AGE)) + 
  geom_histogram(binwidth = 1, fill = "darkslategrey") +
  ggtitle("Hospital Visits")+
  ylab("Visits") +
  xlab("Age Group (yrs)") +
  scale_y_continuous(limits=c(0,350))+
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.4,"cm"),
        text = element_text(size = 10,family="Times New Roman"),
        axis.text=element_text(size=10),
        legend.position=c(.8,.90),
        axis.title.y = element_text(margin = margin(r = 5), face="bold"),
        axis.title.x = element_text(margin = margin(t = 10), face="bold"),
        plot.title = element_text(hjust=.5, family = "Times New Roman", size = 12, face ="bold", margin=margin(t=40,b=-30))) +
  stat_bin(binwidth=1, geom="text", colour="black", vjust = -.5,size =3.0 , 
           aes(label=..count..),  family="Times New Roman")); h_Visits


h_Visits_dml <- rvg::dml(ggobj = h_Visits)  #Print plot to pptx
officer::read_pptx() %>%  # initialize PowerPoint slide ----
officer::add_slide() %>%    # add slide ----
officer::ph_with(h_Visits_dml, ph_location()) %>%  # specify object and location of object ----
base::print(target = here::here("_posts","h_Visits.pptx")) #export slide    




# View summary of the AGE and AGEGRP fields
summary(as.factor(HospitalData$AGE))
# > summary(HospitalData$AGE)
# 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 307  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38 

summary(as.factor(HospitalData$AGEGRP))

# summary(HospitalData$AGEGRP)
# 00-02 03-05 06-08 09-11 12-14 15-17 
# 318     7     7    14    58    96 




#summarize the data for plotting adding specific details to organize by Gender

HospitalData_Visits <- 
  sqldf("SELECT 
           AGEGRP,
           Sex,
           COUNT(Sex) AS Visits
        FROM HospitalData 
        GROUP By 1,2")
HospitalData_Visits <- arrange(HospitalData_Visits, AGEGRP, Sex)
HospitalData_Visits


#Plot the data with some aesthetic detail
h_Visits2 <- (HospitalData_Visits %>%
  gather("Type", "Value",-AGEGRP,-Sex) %>%    #use gather function to make wide data long
  ggplot(aes(x=AGEGRP, y=Value, fill = Sex, group= Sex)) +
  geom_bar(position = "stack",stat = "identity")  +
  ggtitle("Hospital Stays")+
  ylab("Frequency") +
  xlab("Age Group (yrs)") +
  scale_y_continuous(limits=c(0,350))+
  scale_fill_manual(values = c("darkslategray3", "darkslategrey"),labels = c("Sex 1", "Sex 0")) + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.4,"cm"),
        text = element_text(size = 10,family="Times New Roman"),
        axis.text=element_text(size=10),
        legend.position=c(.8,.80),
        axis.title.y = element_text(margin = margin(r = 5), face = "bold"),
        axis.title.x = element_text(margin = margin(t = 5), face = "bold"),
        plot.title = element_text(family = "Times New Roman", size = 12, margin=margin(0,0,10,0)),) +
  stat_summary(fun = sum, aes(label = ..y.., group = AGEGRP), geom = "text",  vjust = -.2, size =3.0 , 
               family="Times New Roman")); h_Visits2

#Print plot to pptx
h_Visits2_dml <- rvg::dml(ggobj = h_Visits2)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(h_Visits2_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = here::here(
    "_posts",
    "h_Visits2.pptx"
  )
)




# Repeat Summary above for Cost Data
#summarize the data for plotting adding specific details to organize by Gender

HospitalData_Costs <- 
  sqldf("SELECT 
           AGEGRP,
           Sex,
           sum(TOTCHG) AS Costs
        FROM HospitalData 
        GROUP By 1,2")
HospitalData_Costs <- arrange(HospitalData_Costs, AGEGRP, Sex)
HospitalData_Costs

h_Costs <- (HospitalData_Costs %>%
  gather("Type", "Value",-AGEGRP,-Sex) %>%   #use gather function to make wide data long
  ggplot(aes(x=AGEGRP, y=Value, fill = Sex, group= Sex)) +
  geom_bar(position = "stack", stat = "identity")  +
  ggtitle("Hospital Costs")+
  ylab("$(Thousands)") +
  xlab("Age Group (yrs)") +
  scale_y_continuous(labels = function(l) {l = l / 1000; paste0(l, "K")}, limits = c(0,750000)) +
  scale_fill_manual(values = c("darkslategray3", "darkslategrey"),labels = c("Sex 1", "Sex 0")) + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.4,"cm"),
          text = element_text(size = 10,family="Times New Roman"),
          axis.text=element_text(size=10),
          legend.position=c(.8,.80),
          axis.title.y = element_text(margin = margin(r = 5), face = "bold"),
          axis.title.x = element_text(margin = margin(t = 5), face = "bold"),
          plot.title = element_text(family = "Times New Roman", size = 12, margin=margin(0,0,10,0)),) +
  stat_summary(fun = sum, aes(label = ks(..y..), group = AGEGRP), geom = "text", vjust = -.4, size = 3,
               family="Times New Roman"))

h_Costs



#Print plot to pptx
h_Costs_dml <- rvg::dml(ggobj = h_Costs)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(h_Costs_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = here::here(
    "_posts",
    "h_Costs.pptx"
  )
)





# Compare % of total costs and visits among age groups to determine group with most expenditure and visits
Hsp_Summary <-
  sqldf("SELECT
               AGEGRP,          
               SUM(a.TOTCHG) AS Costs,  
               COUNT(a.Sex)  AS Visits,
               b.Total_Cost,
               b.Total_Visits
        FROM HospitalData a
                    CROSS JOIN (SELECT SUM(TOTCHG) Total_Cost, COUNT(Sex) Total_Visits
                          FROM HospitalData) b 
        GROUP By 1")


# Add Percentages to the Data
Hsp_Summary1 <- mutate(Hsp_Summary, Cost_Pct = (round(((Costs/Total_Cost)),3)),Visit_Pct =round(((Visits/Total_Visits)),3))
Hsp_Summary2 <- subset(Hsp_Summary1, select= c(AGEGRP, as.numeric(Cost_Pct), Visit_Pct))
Hsp_Summary2

# Hsp_Summary2
# AGEGRP Cost_Pct Visit_Pct
# 1  00-02    0.521     0.636
# 2  03-05    0.047     0.014
# 3  06-08    0.024     0.014
# 4  09-11    0.043     0.028
# 5  12-14    0.109     0.116
# 6  15-17    0.256     0.192

#Plot the data
h_pcts <- (Hsp_Summary2 %>%
            gather("Type", "Value",-AGEGRP) %>%
  ggplot(aes(AGEGRP, Value, fill = Type, )) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = my_percent(Value)),  size = 3.2,  vjust = -.7, position = position_dodge(.9) ,family="Times New Roman" )+
  ggtitle("Hospital Costs and Utilization")+
  ylab("Percent (%)") +
  xlab("Age Group (yrs)") +
  scale_y_continuous(limits=c(0,.80), labels=percent) +
  scale_fill_manual(values = c("darkseagreen", "darkslategray"),labels = c("Hospital Costs", "Hospital Stays")) + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.4,"cm"),
          text = element_text(size = 10,family="Times New Roman"),
          axis.text=element_text(size=10),
          legend.position=c(.8,.80),
          axis.title.y = element_text(margin = margin(r = 5), face = "bold"),
          axis.title.x = element_text(margin = margin(t = 5), face = "bold"),
          plot.title = element_text(family = "Times New Roman", size = 12, margin=margin(0,0,10,0))))

h_pcts

#Print plot to pptx
h_pcts_dml <- rvg::dml(ggobj = h_pcts)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(h_pcts_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = here::here(
    "_posts",
    "h_pcts.pptx"
  )
)






Hsp_Summary3 <- subset(Hsp_Summary1, select= c(AGEGRP,Costs, Visits, as.numeric(Cost_Pct), Visit_Pct))
str(Hsp_Summary3)
colnames(Hsp_Summary3) <- c('Age Group','Costs','Visits','% of Total Costs', '% of Total Visits')
Hsp_Summary3




# format the column Costs as currency, and Visit% and Cost% as percentages
library(DT)
h_stats<- datatable(Hsp_Summary3) %>% formatCurrency(c('Costs')) %>% formatPercentage(c('% of Total Costs', '% of Total Visits'), 1)
library(xlsx)
write.xlsx(Hsp_Summary3, "/Users/samiai.masri/Documents/MastersProgram/4_Data Science with R/Project/Projects for Submission/Healthcare/Healthcare/_posts/h_stats.xlsx")



#***************************************************************************************************************************
#Select the Top Diagnosis with Highest Cost Percentage (eg. APRDRG with greater than 1.5% of total costs for the population)
DRG_Summary <-
  sqldf("SELECT
               APRDRG AS Diagnosis, 
              
               COUNT(a.Sex)                                             AS Visits,
               SUM(CAST(a.TOTCHG AS float))                             AS Cost,
               round((SUM(CAST(a.TOTCHG AS float))/COUNT(a.Sex)),0)     AS Avg_Cost_Per_Visit,
               round((SUM(CAST(a.TOTCHG AS float))/b.Total_Cost),3)     AS Cost_Pct,
               CAST(COUNT(a.Sex) AS float)/b.Total_Visits               AS Visit_Pct
        FROM HospitalData a
                    CROSS JOIN (SELECT CAST(SUM(TOTCHG) AS float) Total_Cost, COUNT(Sex) Total_Visits
                          FROM HospitalData) b 
        GROUP By 1
        ORDER BY Cost_Pct desc")

DRG_Summary
str(DRG_Summary)

#Plot the Data 

h_drg <- (DRG_Summary[c(-2,-3,-4)] %>%  #(Exclude the column 2-4 from data frame - will need in next report)
  filter(Cost_Pct > .015) %>%   #Filter the data for those APRDRG records where Cost_Pct > .015 or 1.5% of total costs
  gather("Type", "Value",-Diagnosis) %>%  #use gather function to make wide data long
  ggplot(aes(x= reorder(as.factor(Diagnosis), -Value), Value, fill = Type)) + #reorder function sorts the bars in graph
  geom_bar(position = "dodge", stat = "identity", width = .8) +
  geom_text(aes(label = my_percent(Value)),  size = 3.0, vjust=-.8,hjust=-.2, angle =60,
            position = position_dodge(1) ,family="Times New Roman", fontface="bold" )+
  ggtitle("*Healthcare Data By Diagnosis")+
  ylab("Percent (%)") +
  xlab("Diagnosis (APRDRG)") +
  scale_fill_manual(values = c( "darkseagreen", "darkslategray"),
                    labels = c( "Hospital Costs", "Hospital Stays")) + 
  scale_y_continuous(limits=c(0,.65), labels=percent)+
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.5,"cm"),
        text = element_text(size = 7,family="Times New Roman"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position=c(.8,.8),
        plot.title = element_text(family = "Times New Roman", size = 14, margin=margin(0,0,10,0)),
        legend.text = element_text( size=10))+
  annotate("rect", xmin=9, xmax=15, ymin=.30 , ymax=.40, 
           alpha=0.2, fill="lightsteelblue4") +
  annotate('text', 
           x = 12.2, y = .37, 
           label = '*Diagnosis that make-up     ', color='black',
           family="Times New Roman",
           size=4.5) +
  annotate('text', 
           x = 12.2, y = .33, 
           label = '1.5% or more of total costs.', color='black',
           family="Times New Roman",
           size=4.5) )

h_drg

#Print plot to pptx

h_drg_dml <- rvg::dml(ggobj = h_drg)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(h_drg_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = here::here(
    "_posts",
    "h_drg.pptx"
  )
)

# We can see that the diagnosis associated with APRDRG = 640 is the most costly, making up ~ 53.4 % of total costs.
# APRDRG 640 also accounts for the diagnosis with the most hospital visits; ~32 % of all visits.

#However, to find the most expensive treatments we need to look at Average Cost Per Diagnosis


costly_drg <- (DRG_Summary[c(-2,-3,-5,-6)] %>%  #(Exclude columns from data not part of analysis)
  filter(Avg_Cost_Per_Visit > 10000) %>%  # Filter for those APRDRG that Average over $10,000 per visit
  gather("Type", "Value",-Diagnosis) %>%
  ggplot(aes(x= reorder(as.factor(Diagnosis), Value), Value, fill = Type)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = dollar(Value)),  size = 4, hjust = -.1, family="Times New Roman", fontface="bold")+
  ggtitle("Diagnosis with Highest Average Cost Per Hospital Visit")+
  ylab("$'s") +
  xlab("Diagnosis (APRDRG)") +
  scale_fill_manual(values = c("darkseagreen"),
                    labels = c("Average Cost Per Visit ($)")) + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(0.4,"cm"),
        text = element_text(size = 6,family="Times New Roman"),
        axis.text=element_text(size=13),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        legend.position=c(.75,.74),
        plot.title = element_text(family = "Times New Roman", size = 15, margin=margin(0,0,5,0)),
        legend.text = element_text( size=10))+
  scale_y_continuous(labels=scales::dollar_format(),limits = c(0,60000))+
  annotate("rect", xmin=7.0, xmax=10.5, ymin=35000 , ymax=57000, 
           alpha=0.2, fill="lightsteelblue4") +
  annotate('text', 
           x = 9.5, y = 45500, 
           label = 'Top costing Diagnosis make up 23.8%', color='black',
           family="Times New Roman",
           size=3.7) +
  annotate('text', 
           x = 8.7, y = 45500, 
           label = 'of total costs($330.7K of the $1.39M)', color='black',
           family="Times New Roman",
           size=3.7)+
  annotate('text', 
           x = 7.9, y = 45000, 
           label = 'and only 4.0% of visits (20 of 500).  ', color='black',
           family="Times New Roman",
           size=3.7)+
  coord_flip())

costly_drg_summary <- DRG_Summary %>%  #(Exclude columns from data not part of analysis)
                          filter(Avg_Cost_Per_Visit > 10000) 

DRG_Summary
costly_drg
costly_drg_summary

#Print plot to pptx

costly_drg_dml <- rvg::dml(ggobj = costly_drg )

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(costly_drg_dml, ph_location()) %>%
  # export slide -----
base::print(
  target = here::here(
    "_posts",
    "costly_drg.pptx"
  )
)

# **** Conclusion *******
#Conclusion: The Top costing Diagnosis make up 23.8% of total costs($330.7K of the $1.39M), 
# and 4.0% of visits (20 of 500).  Diagnosis associated with APRDRG of 911 is the most expensive
# Diagnosis with an Ave Cost Per Hospital Visit of ~ $48K. 


#***************************************************************************************************************************



#*  #3: Malpractice Risks: To make sure that there is no malpractice, the agency needs to analyze 
#*  if the race of the patient is related to the hospitalization costs. 
#*
#*
#*


# rm(list = ls())                                               #clear the environment
# HospitalData <- read.csv("HospitalCosts.csv",header = TRUE )  #Import Data File
# str(HospitalData)                                             #review structure of data variables

# 'data.frame':	500 obs. of  6 variables:
# $ AGE   : int  17 17 17 17 17 17 17 16 16 17 ...
# $ FEMALE: int  1 0 1 1 1 0 1 1 1 1 ...
# $ LOS   : int  2 2 7 1 1 0 4 2 1 2 ...
# $ RACE  : int  1 1 1 1 1 1 1 1 1 1 ...
# $ TOTCHG: int  2660 1689 20060 736 1194 3305 2205 1167 532 1363 ...
# $ APRDRG: int  560 753 930 758 754 347 754 754 753 758 ...

# plot_missing(HospitalData)                            #check for missing records
# HospitalData$RACE[is.na(HospitalData$RACE)]<-median(HospitalData$RACE,na.rm=TRUE) #Take median value, update missing 
summary(HospitalData)
HospitalData$RACE <- as.factor(HospitalData$RACE)     #convert RACE from int to a factor variable
table(HospitalData$RACE)                              #determine how many Race groups there are for our Null Hypothesis

#   1   2   3   4   5   6 
# 485   6   1   3   3   2 

str(HospitalData)
# ANOVA tests whether any of the group means are different from the overall mean of the data by 
# checking the variance of each individual group against the overall variance of the data. 
# If one or more groups falls outside the range of variation predicted by
# the null hypothesis (that all group means are equal), then the test is statistically significant.

race_anova <- aov(TOTCHG ~ RACE, HospitalData)
race_anova

# > race_anova
# Call:
#   aov(formula = TOTCHG ~ RACE, data = HospitalData)
# 
# Terms:
#   RACE  Residuals
# Sum of Squares    18609476 7526126736
# Deg. of Freedom          5        494
# 
# Residual standard error: 3903.213
# Estimated effects may be unbalanced
# > 


summary(race_anova)
# > summary(race_anova)
# Df    Sum Sq  Mean Sq F value Pr(>F)
# RACE          5 1.861e+07  3721895   0.244  0.943
# Residuals   494 7.526e+09 15235074               
# > 

# The F-value column is the test statistic from the F test. This is the mean square of each independent 
# variable (RACE) divided by the mean square of the residuals. The larger the F value, the more likely 
# it is that the variation caused by the independent variable is real and not due to chance.

# The Pr(>F) column is the p-value of the F-statistic. This shows how likely it is that the 
# F-value calculated from the test would have occurred if the null hypothesis of 
# no difference among group means were true.

# **** Conclusion *******

# The null hypothesis states that the mean cost values of the six different race groups are equal. 
# Because the p-value is 0.943, which is more than the significance level of 0.05,
# we can conclude that there is not enough evidence to reject the Null Hypothesis that all means are equal. 
# In other words, our does not indicate RACE as being a factor that drives hospital treatment costs.

# 
# 
# #create histogram of residuals
# res_anova<-(ggplot(data = race_anova, aes(x = race_anova$residuals))  +
#               geom_histogram(fill = 'steelblue', color = 'black')     +
#               labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') + 
#               theme_bw())
# 
# res_anova
# 
# res_anova_dml <- rvg::dml(ggobj = res_anova) 			              #print plot to pptx
# officer::read_pptx() %>%					                              #initialize PowerPoint slide
# officer::add_slide() %>%					                              #add slide
# officer::ph_with(res_anova_dml, ph_location()) %>% 		          #specify object and location of object 
# base::print(target = here::here("_posts", "res_anova.pptx")) 		#export slide
#   

#Q-Q Plot data to visually inspect distribution of the data

res_anova2<-(ggqqplot(race_anova$residuals)  +
              labs(title = 'Q-Q Plot of Residuals', x = 'Theoretical', y = 'Sample') + 
              theme_bw())

res_anova2

res_anova2_dml <- rvg::dml(ggobj = res_anova2) 			            #print plot to pptx
officer::read_pptx() %>%					                              #initialize PowerPoint slide
officer::add_slide() %>%					                              #add slide
officer::ph_with(res_anova2_dml, ph_location()) %>% 		        #specify object and location of object 
base::print(target = here::here("_posts", "res_anova2.pptx")) 	#export slide


#Run Shaprio-Wilk test to compliment the visual observation for normality
shapiro.test(race_anova$residuals)

# > shapiro.test(race_anova$residuals)
# 
# Shapiro-Wilk normality test
# data:  race_anova$residuals
# W = 0.47558, p-value < 2.2e-16
# >

#Run non-parametric test which does not require any assumption on the distribution of the data
kruskal.test(HospitalData$TOTCHG ~ HospitalData$RACE)

# > kruskal.test(HospitalData$TOTCHG ~ HospitalData$RACE)
# 
# Kruskal-Wallis rank sum test
# 
# data:  HospitalData$TOTCHG by HospitalData$RACE
# Kruskal-Wallis chi-squared = 3.2751, df = 5, p-value = 0.6576
# 
# >


#===========================================================================================================

# Resource Allocation: To properly utilize the costs, the agency has to analyze the severity of the 
# hospital costs by age and gender for proper allocation of resources. 


#What is the equation for costs as a function of age and gender?
# plot data, visualize if there is a linear relationship between response variable (costs) and each predicator variable (age, gender)
# run correlation
# run regression

# Predicting Costs using Age and Gender by Regression in R. Train a linear regression model in 
# R which will be able to predict Costs using age and gender for patients that are not present in our data set.

# Testing the Null Hypothesis:
#   H0: a linear relationship does not exist between hospital costs and age and gender of a patient
#   H1: a linear relationship does exist between hospital costs and age and gender of a patient



# We will create a linear regression model to help us understand what, if any,  influence/relationship do Age and Gender(independent variables) 
# of a patient have on Hospital Costs (dependent variable).  For our null hypothesis we will assert that age and gender do not influence
# costs.

rm(list = ls())                                               #clear the environment
HospitalData <- read.csv("HospitalCosts.csv",header = TRUE )  #Import Data File
summary(HospitalData)
str(HospitalData)                                             #review structure of data variables
plot_missing(HospitalData)                                    #check for missing records
HospitalData$RACE[is.na(HospitalData$RACE)]<-median(HospitalData$RACE,na.rm=TRUE) #Take median value, update missing 
summary(HospitalData)

# Equation of the regression line in our dataset.
# 
# Y = B0 + B1X1 + B2X2 + E


#Use cor() function to support the initial analysis for how the independent and dependent variables correlate 
cor(HospitalData$TOTCHG,HospitalData$AGE)
#[1] 0.1316797    #some correlation (weak positive correlation)
cor(HospitalData$TOTCHG,HospitalData$FEMALE)
#[1] -0.06019504  #slightly less correlation (weak negative correlation)

#Split the data into Training and Test data.  We will train our model using the Training data set 
#and test the model using the Test data set.

summary(HospitalData)
set.seed(2)                                                 # set seed so samples can be recreated in the future
library(caTools)
split <- sample.split(HospitalData, SplitRatio = 0.7)
split


train <- subset(HospitalData, split=="TRUE")
test  <- subset(HospitalData, split=="FALSE")


#Train - Train the Model 
cost_model_1 <- lm(TOTCHG ~ AGE+as.factor(FEMALE), data = train)   # assign FEMALE as a factor
print(cost_model_1)

#Call:
#  lm(formula = TOTCHG ~ AGE + as.factor(FEMALE), data = train)

# Coefficients:
#   (Intercept)               AGE                as.factor(FEMALE)1  
#       2737.9               101.0              -866.8  


#Interpret Results
# our beta coefficients:
# y - intercept - 2737.9
#           age -  101.0
#        gender -  866.8
# ==> Cost = 2737.9 + 101.0(age) -866.8(gender)  # our formula for predicting costs when age and gender are known.
# Is this formula significant?  If so, how significant?

summary(cost_model_1)

# Call:
#   lm(formula = TOTCHG ~ AGE + as.factor(FEMALE), data = train)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3646  -1464   -788   -153  44800 
# 
# Coefficients:
#                       Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept)           2737.93     330.50        8.284   2.99e-15 ***
#   AGE                  101.01      32.15        3.142   0.00183 ** 
#   as.factor(FEMALE)1  -866.78     454.77        -1.906  0.05752 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4007 on 331 degrees of freedom
# Multiple R-squared:  0.03248,	Adjusted R-squared:  0.02664 
# F-statistic: 5.557 on 2 and 331 DF,  p-value: 0.004231
# 
# > 




# ==> Model's p-value = 0.004231.  It is less than the 0.05 significance level.

#In Linear Regression, the Null Hypothesis (H0) is that the beta coefficients associated with the variables is equal to zero.
# The alternate hypothesis (H1) is that the coefficients are not equal to zero. 
# (i.e. there exists a relationship between the independent variable in question and the dependent variable).

# Pr(>|t|) or p-value is the probability that you get a t-value as high or higher than the observed value when the Null Hypothesis (the ? coefficient is equal to zero or that there is no relationship) is true.
# 
# So if the Pr(>|t|) is low, the coefficients are significant (significantly different from zero). If the Pr(>|t|) is high, the coefficients are not significant.
#When p Value is less than significance level (< 0.05), you can safely reject the null hypothesis that the co-efficient ? of the predictor is zero.

# with a p-value 0.004231 <.05 the test is statistically significant and we can reject the null hypothesis that 
# the there is no linear association between Cost, Age and Gender.

# When testing the null hypothesis that there is no linear association between Costs and age and after adjusting for gender, 
# we reject the null hypothesis. For a one-unit change in age, on average, the Costs increase by 101.0, after adjusting for gender.

# When testing the null hypothesis that there is no linear association between Costs and gender and after adjusting for age, 
# we reject the null hypothesis. For factor(FEMALE)1 gender, Costs decreases by -866.78 units, after adjusting for age.

#What R-Squared tells us is the proportion of variation in the dependent (response) variable that has been explained by this model.
# However, the model where the predictor variables are age+gender accounts for only ~ 2.66% (Adjusted R-squared) of the variation in costs.
# We should continue to research the data for other variables that may better predict hospital costs. 


AIC(race_anova)
AIC(cost_model_1)


# Calculate prediction accuracy and error rates
# A simple correlation between the actual and predicted values can be used as a form of accuracy measure.
# 
# A higher correlation accuracy implies that the actual and predicted values have similar directional movement, 
# i.e. when the actual values increase the predicted values also increase and vice-versa.


#Prediction - Test the Model
pred <- predict(cost_model_1, data = test)
pred
summary(pred)


library(modelr)
predications1 <- add_predictions(test, cost_model_1, var = "pred_Cost", type = NULL)
correlation_accuracy1 <- cor(predications1$TOTCHG, predications1$pred_Cost)
correlation_accuracy1 #(low correlation)

#================================================================================

#Length of Stay (LOS): Can a patients age, gender, and race predict Hospital Cost?

#=======================================================================



scatter.smooth(x=train$AGE, y=train$LOS, main="LOS ~ AGE")
scatter.smooth(x=train$FEMALE, y=train$LOS, main="LOS ~ FEMALE")
scatter.smooth(x=train$RACE, y=train$LOS, main="LOS ~ RACE")


summary(train)
#Use cor() function to support the initial analysis for how the independent and dependent variables correlate 
cor(train$LOS,train$AGE)
#[1] -0.05729162   #some correlation (weak negative correlation)
cor(train$LOS,train$FEMALE)
#[1] -0.005936428    #some correlation (weak negative correlation)
cor(train$LOS,train$RACE)
#[1] -0.02543265   #some correlation (weak negative correlation)

#Train - Train the Model 
los_model_1 <- lm(LOS ~ AGE + FEMALE + RACE, data = train)   
print(los_model_1)

summary(los_model_1)
# Call:
#   lm(formula = LOS ~ AGE + FEMALE + RACE, data = train)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.058 -1.058 -0.996  0.004 36.004 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.14025    0.46072   6.816 4.46e-11 ***
#   AGE         -0.02722    0.02638  -1.032    0.303    
# FEMALE       0.06274    0.37276   0.168    0.866    
# RACE        -0.14467    0.34721  -0.417    0.677    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.284 on 330 degrees of freedom
# Multiple R-squared:  0.003898,	Adjusted R-squared:  -0.005158 
# F-statistic: 0.4304 on 3 and 330 DF,  p-value: 0.7313
# 
# >




#Prediction - Test the Model
pred_los <- predict(los_model_1, data = test)
pred_los
summary(pred_los)


library(modelr)
predications_los <- add_predictions(test, los_model_1, var = "pred_los", type = NULL)
correlation_accuracy_los <- cor(predications_los$LOS, predications_los$pred_los)
correlation_accuracy_los #(low correlation)








#===========================
cor(train$TOTCHG,train$AGE)
cor(train$TOTCHG,train$RACE)
cor(train$TOTCHG,train$FEMALE)
cor(train$TOTCHG,train$LOS)
cor(train$TOTCHG,train$APRDRG)



cost_model_2 <- lm(TOTCHG ~ ., data = train) 
print(cost_model_2)

# Call:
#   lm(formula = TOTCHG ~ ., data = train)
# 
# Coefficients:
#   (Intercept)          AGE       FEMALE          LOS         RACE       APRDRG  
# 4622.023      138.084     -435.721      746.371     -380.035       -6.444 

summary(cost_model_2)

# Call:
#   lm(formula = TOTCHG ~ ., data = train)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5701   -860   -228    100  42881 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4622.0226   745.4904   6.200 1.70e-09 ***
#   AGE          138.0839    24.2695   5.690 2.82e-08 ***
#   FEMALE      -435.7209   348.6652  -1.250    0.212    
# LOS          746.3712    50.3208  14.832  < 2e-16 ***
#   RACE        -380.0355   318.5602  -1.193    0.234    
# APRDRG        -6.4444     0.9874  -6.527 2.56e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3002 on 328 degrees of freedom
# Multiple R-squared:  0.4619,	Adjusted R-squared:  0.4537 
# F-statistic: 56.31 on 5 and 328 DF,  p-value: < 2.2e-16







#Prediction - Test the Model
pred2 <- predict(cost_model_2, data = test)
summary(pred2)


library(modelr)
predications2 <- add_predictions(test, cost_model_2, var = "pred_Cost", type = NULL)
correlation_accuracy2 <- cor(predications2$TOTCHG, predications2$pred_Cost)
correlation_accuracy2 


library(QuantPsyc) #use the lm.beta function to standardize the coefficients coming out of the linear regression model

lm.beta 

# > lm.beta 
# function (MOD) 
# {
#   b <- summary(MOD)$coef[-1, 1]
#   sx <- sapply(MOD$model[-1], sd)
#   sy <- sapply(MOD$model[1], sd)
#   beta <- b * sx/sy
#   return(beta)
# }
# <bytecode: 0x7f8419c0a9c8>
#   <environment: namespace:QuantPsyc>
#   > 

lm.beta(cost_model_2)
# > lm.beta(cost_model_2)
# AGE      FEMALE         LOS        RACE      APRDRG 
# 0.24078243 -0.05371391  0.60195763 -0.04854809 -0.27472808 
# >

























