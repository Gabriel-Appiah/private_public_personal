pkg <- c("tidyverse","wordcloud2","RColorBrewer","gmodels","modelr","nnet","tidyr","tm",'ggplot2','gridExtra',
         'ggsci','quanteda',"patchwork","grid","gridExtra")

new.packages <- pkg[!(pkg %in% installed.packages()[,"Package"])]

if(length(new.packages))install.packages(new.packages)

invisible(lapply((pkg),library, character.only = T))

################################################################################

fs <- read.csv("FinalprocesEdited.csv")

# Calculate proportions for data sources

summary <- fs |>
  group_by(Data_S)|>
  summarize(count = n())|>
  mutate(proportion = count/sum(count) * 100)

#Chisquare test of independence using Crosstable function from gmodels

CrossTable(fs$Data_S, fs$F_Y_N,
           expected = T, prop.r = F, prop.c = F,
           prop.t=F, prop.chisq = F)

#Selecting sample papers who used private sector data
#checking how they treated some of the issues we
#highlighted in the discussion section

privatedata <- fs%>%
  filter(Data_S == "Private")

#first selected 100 papers using private data source
set.seed(3)

pvsapp <- privatedata[sample(nrow(privatedata),size=100),]


sprivate <- read.csv("Sprivatedata.csv")
titles <- sprivate$X

newdata <- privatedata |>
  subset(!(X %in% titles))|>
  filter(Data_S == "Private")

nrow(newdata)

set.seed(4)

extrapvsapp <- newdata[sample(nrow(newdata), size =2),]

##########################################################################################
#Calculating average percent change in papers using government data
govdata <- fs |>
  group_by(Pub_Y, Data_S)|>
  arrange(Pub_Y,.by_group = TRUE)|>
  summarise(cot=n())|>
  pivot_wider(names_from = Data_S, values_from = cot)


#ncol <- tibble(diff(govdata$Government)/head(govdata$Government,-1))

ncol <- tibble (percgov = (govdata$Government - lag(govdata$Government))/lag(govdata$Government)*100)

govdata<-govdata|>
  cbind(ncol)

mean(govdata$percgov,na.rm = TRUE)

#################################################################

df <- df|>
  group_by(J_Name,PublishedData)|>
  summarise(co = n())|>
  pivot_wider(names_from =PublishedData, values_from = co)

datproviders <- df|>
  group_by(J_Name,Thank.Data.provider)|>
  summarise(co = n())|>
  pivot_wider(names_from =Thank.Data.provider, values_from = co)|>
  mutate(prop = Y /(Y+N)*100)

datacknowledge <- df|>
  group_by(Thank.Data.provider)|>
  summarize(co=n())|>
  mutate(prop = co/sum(co)*100)
View(datacknowledge)


publishedata<- df|>
  group_by(PublishedData)|>
  summarize(co = n())

###############################################################################
#Selecting Articles for detail Review

pvt_select <- filter(finalproc, Data_S == 'Private')

set.seed(3)

pvsapp <- pvt_select[sample(nrow(pvt_select),size=1),]

View(pvsapp)

write.csv(pvsapp,'pvsappadditionaonlyone.csv')

set.seed(20)
gvt_select <- filter(finalproc, Data_S == 'Government')

gvsapp <- gvt_select[sample(nrow(gvt_select),size=1),]

View(gvsapp)

#write.csv(gvsapp,'gvsappADDITIONA.csv')


set.seed(10)
twomore_select <- filter(finalproc, Data_S == 'Two or More Sources')

twoapp <- twomore_select[sample(nrow(twomore_select),size=1),]

View(twoapp)

write.csv(twoapp,'twoappaddition1only1.csv')

set.seed(1)
fs_select <- filter(finalproc, Data_S == 'Field Survey')

fsapp <- fs_select[sample(nrow(fs_select),size=1),]

View(fsapp)
write.csv(fsapp,'fsappadditional.csv')