pkg <- c("tidyverse","wordcloud2","RColorBrewer","gmodels","modelr","nnet","tidyr","tm",'ggplot2','gridExtra',
         'ggsci','quanteda',"patchwork","grid","gridExtra")

new.packages <- pkg[!(pkg %in% installed.packages()[,"Package"])]

if(length(new.packages))install.packages(new.packages)

invisible(lapply((pkg),library, character.only = T))


setwd("D:\\Master\\PhD Files\\Research\\__ResearchCollaboration\\Col_2\\arch_shifting_patterns\\arch_shifting patterns\\arch_shifting patterns\\CEUS")

################################################################################
pltfunction <- function(data,label){
  plt<-ggplot(data)+aes(x=reorder(str_wrap(activity,2),Freq),y=Freq,fill=Freq)+
    geom_bar(position="dodge",stat="identity",show.legend = FALSE,
             alpha = 0.9)+
    scale_fill_gradientn(colors = c("#6C5B7B","#C06C84","#F67280","#F8B195"))+
    theme_minimal()+
    theme(axis.text.y=element_text(size = 5),axis.title.x = element_text(size = 5),
          axis.title = element_text(size=4)) +xlab('Keywords')+ylab('Word Frequency')+
    geom_text(aes(x=reorder(str_wrap(activity,2),Freq),
                  y=Freq + 0.8,label = Freq), vjust = 0, 
              position = position_dodge(.0),size = 3,color = 'gray12')+
    ggplot2::annotate("text",x=6.2,y=0.1,label=label,size=4,
                      vjust=1.1,hjust = -0.1)+
  coord_flip()
  return(plt)
}

textcleaning <- function(df,clutno,clutno2){
  cleandaframe <-df|>
    filter(cluster == clutno|cluster==clutno2)|>
    select(new_keywords)|>
    as.vector()|>
    unlist()|>
    iconv(to = "UTF-8",sub="")|>
    strsplit(";")|>
    unlist()|>
    #str_trim()|>
    trimws()|>
    str_to_lower()|>
    corpus()
  return(cleandaframe)
}

selectdata <- function(df,nvalue){
  ndata <- df|>
    select(activity)|>
    mutate(across(everything(),~na_if(.,"")))|>
    table()|> #add frequency to the text
    as.data.frame()|> #convert it back to a dataframe
    filter(Freq >= nvalue)|>
    mutate(label=paste(activity,Freq,sep=" "))|>
    slice_max(Freq, n=5,with_ties = TRUE)
  return(ndata)
}



################################################################################
data <- read.csv("mapsmin10001.csv")%>%
  separate(url, into= c('HTPP','DOI'),sep ='https://doi.org/')

finalproc <- read.csv("FinalprocesEdited.csv")%>%
  mutate(doi = str_to_lower(DOI), MCategory = ifelse(M_Category == "Introductory","Group 1",M_Category),
         MCategory = ifelse(M_Category == "Moderate", "Group 2",MCategory),
         MCategory = ifelse(M_Category == "Advanced","Group 3",MCategory))%>%
  select(-DOI)%>%
  select(-M_Category)%>%
  rename(DOI = doi)

#write.csv(finalproc, "FinalDataSet.csv")

data_proces <- data %>%
  left_join(finalproc, by = 'DOI')%>%
  mutate(new_keywords = ifelse(is.na(Author.Keywords),Index.Keywords,Author.Keywords))

removwords <- c('united states','^gis$','africa','arizona','asia',
                'atlantic coast [north america]','atlantic coast [united states]',
                'atlantic ocean','atlas of living australia','australasia','australia',
                'china','north america','geographic information system*|geographic information science*','switzerland','texas',
                'canada','idaho','india','native americans','oregon','rocky mountains',
                'russian federation','sierra nevada','spain','thailand',
                'united kingdom','alberta','europe','eurasia','germany','western europe','france',
                'netherlands','new yok [united states]','north carolina','atlanta',
                'beijing','brazil','california','central europe','east africa','ethiopia',
                'indonesia','italy','kenya','london [england]','louisiana','new york [new york (stt)]',
                'norway','pennsylvania','philadelphia','sub-saharan africa','west africa','wuhan','african savannas','amazonia','england','guangdong','beijing [china]',
                'guangzhou','shanghai','beijing [beijing (ads)]','shenzhen','singapore [southeast asia]',
                'florida [united states]','georgia','greece','new zealand','western europe','iran','hubei',
                'mississippi delta','portugal','colorado','denver','new york city','latinos','boston',
                'benelux','wales','massachusetts','los angeles','north holland','sweden','toronto','far east',
                'utrecht','memphis','shanghai','taiwan','tennessee','western europe','internet gis','software',
                'world wide web')%>%
  str_c(collapse = "|")
################################################################################


s_datanewclut1<- textcleaning(data_proces,1,0)


land<- c('land use','land cover','land-cover', 'land-use','land-use and land-cover change')%>%
  str_c(collapse = "|")

s_datanewclut1_ <- as.data.frame(s_datanewclut1)|>
  mutate(activity = ifelse(str_detect(s_datanewclut1,pattern = '^climate'),'climate change',s_datanewclut1),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^accuracy'),'accuracy assessment',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^cluster|clustering$'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^digital|grid digital elevation models')&
                             !str_detect(s_datanewclut1,'digital mapping'),'digital elevation model',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^dem$|dem accuracy|
                                      dem error|dem reliability|dem resolution'),'digital elevation model',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^error'),'error analysis',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^fuzzy'),'fuzzy mathematics',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^glmm|poisson generalized linear mixed model (glmm)'),'geostatistics',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^hydro|hydrology$|hydrograph \\(giuh\\)$'),'hydrological modeling',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^image'),'image analysis',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^kriging|kriging$|interpolation$'),'interpolation',activity),
         activity = ifelse(str_detect(s_datanewclut1,land),'land cover analysis',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^map|maps$|mapping$|mapping|map$'),'mapping methods',activity),
         activity = ifelse(str_detect(s_datanewclut1,pattern = '^uncertainty'),'uncertainty analysis',activity),
         activity = ifelse(str_detect(s_datanewclut1,removwords),'',activity))|>
  selectdata(6)

#plt1 <- pltfunction(s_datanewclut1_,0.1,63,label="A")
plt1 <- pltfunction(s_datanewclut1_,label="A")
plt1 
################################################################################
s_datanewclut2<- textcleaning(data_proces,2,0)

regression <- c('best linear unbiased estimator (blue)',
                'generalized additive model','hedonic models',
                'least squares method','logistic regression model','logistics',
                'multivariate spatial regression','ols assumptions','ordinary least squares','^multivariate','regression analysis')%>%
  str_c(collapse = "|")

gwm <- c('gwmodel','multiscale geographically weighted regression',
         'geographically weighted regression')%>%
  str_c(collapse = '|')

statistics <- c('covariance analysis','information theory','statistic',
                '^correlation','^stochast','akaike','statistical analysis',
                'geostatistics','spatial statistics','moran',
                'generalized likelihood ratio statistic','likelihood ratio statistic',
                'spatial statistics','circular statistics','local moran and getis & ord statistics',
                'scan statistics','spatial scan statistics','statistical approach','statistical classifier',
                'statistics','conditional autoregressive model','maximum likelihood')%>%
  str_c(collapse = '|')

ml <- c('em algorithm','extreme learning machine','gaussian method','principal component analysis (pca)',
        'principal component analysis','markov','deep learning')%>%
  str_c(collapse = "|")

s_datanewclut2_<-as.data.frame(s_datanewclut2)|>
  mutate(activity = ifelse(str_detect(s_datanewclut2,pattern = 'autocorrelation'),'spatial auto  correlation',s_datanewclut2),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^cluster'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^spatio'),'spatiotemporal analysis',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^bayesian'),'bayesian analysis',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = 'neural network'),'artificial neural network',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^gps'),'gps data',activity),
         activity = ifelse(str_detect(s_datanewclut2,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^kernel'),'kernel density estimation',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = '^space'),'space time analysis',activity),
         activity = ifelse(str_detect(s_datanewclut2,regression)&!str_detect(s_datanewclut2,gwm),'regression analysis',activity),
         activity = ifelse(str_detect(s_datanewclut2,statistics),'spatial statistics',activity),
         activity = ifelse(str_detect(s_datanewclut2,ml),'machine learning',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = 'interpolation|^kriging'),'interpolation',activity),
         activity = ifelse(str_detect(s_datanewclut2,pattern = 'econometrics|^spatial econometrics'),'spatial econometrics',activity),
         activity = ifelse(str_detect(s_datanewclut2,gwm),'geographical weighted regression',activity))|>
  selectdata(8)

#plt2 <- pltfunction(s_datanewclut2_,0.1,53,label="B")
plt2 <- pltfunction(s_datanewclut2_,label="B")
plt2

################################################################################
s_datanewclut3<- textcleaning(data_proces,4,0)

data_min <- c('spatial data mining')%>%
  str_c(collapse = "|")

social.media <- c('social network','social networks', 'location-based social networks','social sensing',
                  'crowdsensing','volunteered geographic information','poi recommendation',
                  'points-of-interest','crowd flux estimation','crowdsourced tracking data',
                  'crowdsourced transportation','crowdsourcing','crowdsourced data','geolocated social media',
                  'geosensor networks','geosocial','geosocial networks','geotagged images','geotagged tweets',
                  'location-based social network (lbsn)','social media check-ins','vgi','mobile phone location data',
                  'mobile phone data sets','mobile phone traces','mobile phone tracking')%>%
  str_c(collapse = "|")

ml <- c('convolutional neural network','artificial neural network','deep learning','deep neural network',
        'ensemble learning method','functional principal component analysis','graph convolutional neural networks',
        'logistics big data','machine learning techniques','unsupervised algorithm')%>%
  str_c(collapse = "|")

gps <- c('global positioning systems','gnss traces','gnss and gps')%>%
  str_c(collapse = "|")

s_datanewclut3_ <- as.data.frame(s_datanewclut3)|>
  mutate(activity = ifelse(str_detect(s_datanewclut3,pattern = '^gps'),'gps data',s_datanewclut3),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^movement'),'movement analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^cluster'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = 'markov*'),'machine learning',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^trajectory'),'trajectory analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^bayesian'),'bayesian analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^space'),'space time analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^spatio'),'spatio temporal analysis',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^time'),'time geography',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = 'twitter'),'twitter',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^urban planning'),'urban planning',activity),
         activity = ifelse(str_detect(s_datanewclut3,data_min),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^remote'),'remote sensing',activity),
         activity = ifelse(str_detect(s_datanewclut3,social.media),'volunteered geographic information',activity),
         activity = ifelse(str_detect(s_datanewclut3,ml),'machine learning',activity),
         activity = ifelse(str_detect(s_datanewclut3,gps),'gps data',activity),
         activity = ifelse(str_detect(s_datanewclut3,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut3,pattern = '^big'),'big data',activity))|>
  selectdata(7)

#plt3 <- pltfunction(s_datanewclut3_,0.1,42,label="C")
plt3 <- pltfunction(s_datanewclut3_,label="C")
plt3

################################################################################
s_datanewclut4<- textcleaning(data_proces,5,0)

plan <- c('urban area','polycentricity','planning practice',
          'planning system','policy implementation','gentrification',
          'infrastructure planning','local planning','plan-making processes','planner',"planners' perceptions",
          'planning history','polycentric spatial structure','regional planning',
          'regional policy','rural planning','site planning','town planning','zoning','zoning policy','urban growth policy',
          'regional development','placemaking','urban planning')%>%
  str_c(collapse = "|")

plannP <-c('^planning$','planning process','planning processes','planning support systems')%>%
  str_c(collapse = "|")

plannmethod <- c('planning method','planning theory')%>%
  str_c(collapse = "|")

land <- c('land-use planning tools','land cover','land surface')%>%
  str_c(collapse = "|")

urban <- c('urban development','urban design','urban growth','urban system','design',
           'design method','design methods')%>%
  str_c(collapse = "|")

s_datanewclut4_ <- as.data.frame(s_datanewclut4)|>
  mutate(activity = ifelse(str_detect(s_datanewclut4,pattern = 'land use*'),'land use planning', s_datanewclut4),
         activity = ifelse(str_detect(s_datanewclut4,pattern = 'environmental*'),'environmental planning', activity),
         activity = ifelse(str_detect(s_datanewclut4,pattern = 'sprawl*'),'sprawling', activity),
         activity = ifelse(str_detect(s_datanewclut4,pattern = '^gps'),'gps data',activity),
         activity = ifelse(str_detect(s_datanewclut4,pattern = 'economic*'),'economic development', activity),
         activity = ifelse(str_detect(s_datanewclut4,pattern = '^data|spatial data mining'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut4,pattern = 'complex*'),'complex network analysis',activity),
         activity = ifelse(str_detect(s_datanewclut4,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut4,plan),'urban planning',activity),
         activity = ifelse(str_detect(s_datanewclut4,land),'land use planning',activity),
         activity = ifelse(str_detect(s_datanewclut4,urban),'urban design',activity),
         activity = ifelse(str_detect(s_datanewclut4,plannP),'planning process',activity),
         activity = ifelse(str_detect(s_datanewclut4,plannmethod),'planning methods',activity))|>
  selectdata(7)

#plt4 <- pltfunction(s_datanewclut4_,0.1,55,label="D")
plt4 <- pltfunction(s_datanewclut4_,label="D")
plt4

################################################################################
s_datanewclut5<- textcleaning(data_proces,6,0)

cellular <- c('ca-based flus model','^ca$')%>%
  str_c(collapse = '|')

land <- c('land use planning','land-use planning')%>%
  str_c(collapse = "|")


landusechange <- c('land use change','land cover','land-use change','land cover change',
                   'landscape change','land-cover change trajectories',
                   'land-use allocation','land-use and land-cover change','land-use interactions',
                   'land-use model','land-use optimization','land-use/land-cover changes',
                   '^[land change]$','land change modeling','land conversion','land use changes',
                   'land use classification','land use land cover','land use land cover change',
                   'land use modeling','land use patterns','land use/land cover (lulc) change',
                   'multiple land use','multiple land use change modelling','urban land-use change',
                   '^[urban land expansion]$','^[urban land use]$')%>%
  str_c(collapse = "|")

lanuse <- c('^land use$','^land-use$','^land uses$')%>%
  str_c(collapse = "|")

ml <- c('machine learning','markov chain','artificial neural network','artificial intelligence',
        'artificial neural networks','deep learning','ensemble','ensemble kalman filter',
        'markov transition probabilities matrices','monte carlo','monte carlo methods','monte carlo simulation',
        'random forest','random forest (rf) classification and regression')%>%
  str_c(collapse = "|")

landsat <-c('landsat thematic mapper','^modis$','landsat imagery','landsat images','satellite data','vhr imagery')%>%
  str_c(collapse = "|")

urba <- c('urban development modelling','urban growth boundary','urban emergence simulation','urban expansion types',
          'urban extension','urban growth boundary model','urban growth forecasting',
          'urban growth model','urban growth modelling','urban growth pattern','urban growth simulation',
          'urban simulation','urban growth','^sleuth$')%>%
  str_c(collapse = "|")

s_datanewclut5_<- as.data.frame(s_datanewclut5)|>
  mutate(activity = ifelse(str_detect(s_datanewclut5,pattern = 'cellular*'),'cellular automata', s_datanewclut5),
         activity = ifelse(str_detect(s_datanewclut5,pattern = 'agent-based*|agent based*'),'agent-based model', activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = 'remote*'),'remote sensing', activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = '^fuzzy'),'fuzzy mathematics',activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = 'economic*'),'economic development', activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = '^data'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = '^image'),'image classification',activity),
         activity = ifelse(str_detect(s_datanewclut5,pattern = '^lidar'),'lidar data',activity),
         activity = ifelse(str_detect(s_datanewclut5,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut5,cellular),'cellular automata',activity),
         activity = ifelse(str_detect(s_datanewclut5,land),'land use planning',activity),
         activity = ifelse(str_detect(s_datanewclut5,landsat),'landsat imagery',activity),
         activity = ifelse(str_detect(s_datanewclut5,ml),'machine learning',activity),
         activity = ifelse(str_detect(s_datanewclut5,urba),'urban growth simulation',activity),
         activity = ifelse(str_detect(s_datanewclut5,landusechange),'land cover change',activity),
         activity = ifelse(str_detect(s_datanewclut5,lanuse),'land use',activity))|>
  selectdata(6)

#plt5 <- pltfunction(s_datanewclut5_,0.1,66,label="E")
plt5 <- pltfunction(s_datanewclut5_,label="E")
plt5

################################################################################
s_datanewclut6<- textcleaning(data_proces,7,0)

inter <- c('geographical weighting','weighted minimum edit distance')%>%
  str_c(collapse = "|")


s_datanewclut6_ <- as.data.frame(s_datanewclut6)|>
  mutate(activity = ifelse(str_detect(s_datanewclut6,pattern = '^census|population census'),'census data', s_datanewclut6),
         activity = ifelse(str_detect(s_datanewclut6,pattern = 'remote*'),'remote sensing', activity),
         activity = ifelse(str_detect(s_datanewclut6,inter),'areal interpolation', activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^big'),'big data',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = 'interpolation*'),'areal interpolation', activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^data'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^kriging'),'areal interpolation',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^clustering'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut6,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^dasymetric'),'areal interpolation',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = 'bayesian'),'bayesian analysis',activity),
         activity = ifelse(str_detect(s_datanewclut6,pattern = '^social segregation$'),'segregation',activity))|>
  selectdata(5)
#plt6 <- pltfunction(s_datanewclut6_,0.1,26,label="F")
plt6 <- pltfunction(s_datanewclut6_,label="F")
plt6

###############################################################################
s_datanewclut7<- textcleaning(data_proces,8,0)

s_datanewclut7_<- as.data.frame(s_datanewclut7)|>
  mutate(activity = ifelse(str_detect(s_datanewclut7,pattern = 'transportation*'),'transportation planning', s_datanewclut7),
         activity = ifelse(str_detect(s_datanewclut7,pattern = 'agent-based*|agent based*|agents*'),'agent-based model', activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^big'),'big data', activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^walking|^walkability'),'walkability',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^discrete choice|discrete model|^choice'),'discrete choice model', activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^data mining'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^travel'),'travel behavior',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^human'),'human activities',activity),
         activity = ifelse(str_detect(s_datanewclut7,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = 'micro-simulation|microsimulation'),'microsimulation',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^pedestrian'),'pedestrian activities',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = '^traffic'),'traffic analysis',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = 'accessibility*'),'accessibility',activity),
         activity = ifelse(str_detect(s_datanewclut7,pattern = 'logit*'),'logit model',activity))|>
  selectdata(7)|>
  group_by(Freq)|>
  slice(1)|>
  ungroup()


#plt7 <- pltfunction(s_datanewclut7_,0.1,32,label="G")
plt7 <- pltfunction(s_datanewclut7_,label="G")
plt7

###############################################################################
s_datanewclut8<- textcleaning(data_proces,9,0)

s_datanewclut8_ <- as.data.frame(s_datanewclut8)|>
  mutate(activity = ifelse(str_detect(s_datanewclut8,pattern = 'optimization*|^optimal'),'spatial optimization', s_datanewclut8),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'agent-based*|agent based*|agent*'),'agent-based model', activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^algorithms$'),'algorithm', activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^big'),'big data', activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'cluster*'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'simulation'),'computer simulation', activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'data mining*'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'decision*'),'decision analysis',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'genetic*'),'genetic algorithm',activity),
         activity = ifelse(str_detect(s_datanewclut8,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = 'cellular'),'cellular automata',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^parallel'),'parallel algorithm',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^land-use planning$'),'land use planning',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^fuzzy'),'fuzzy mathematics',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^gps$|global positioning system* | ^gps'),'gps data',activity),
         activity = ifelse(str_detect(s_datanewclut8,pattern = '^transportation'),'transportation planning',activity))|>
  selectdata(5)

#plt8 <- pltfunction(s_datanewclut8_,0.1,47,label="H")
plt8 <- pltfunction(s_datanewclut8_,label="H")
plt8

################################################################################

s_datanewclut9<- textcleaning(data_proces,10,3)

ml <- c('artificial neural network','machine learning','deep learning',
        'deep neural network','convolutional neural network*')%>%
  str_c(collapse = '|')

s_datanewclut9_ <- as.data.frame(s_datanewclut9)|>
  mutate(activity = ifelse(str_detect(s_datanewclut9,pattern = 'geovisualization|visual analysis|visualisation|^virtual|^visual|geovisual|visualization'),'visualization', s_datanewclut9),
         activity = ifelse(str_detect(s_datanewclut9,pattern = '^choropleth mapping$|choropleth maps|crime mapping|digital mapping|^map*|mapping$'),'mapping', activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = '^big'),'big data', activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'cluster*'),'cluster analysis',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'data mining*'),'data mining',activity),
         activity = ifelse(str_detect(s_datanewclut9,removwords),'',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'decision*'),'decision analysis',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'internet*'),'internet gis',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'cartograph*'),'cartography',activity),
         activity = ifelse(str_detect(s_datanewclut9,ml),'',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = '3d city models|3d indoor navigation|3d modeling|
                                      3d subdivision|3d urban model'),'3d city models',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = 'accuracy$|^accuracy'),'accuracy assessment',activity),
         activity = ifelse(str_detect(s_datanewclut9,pattern = '^database$|^data set$'),'database',activity))|>
  selectdata(8)|>
  group_by(Freq)|>
  slice(1)|>
  ungroup()

#plt9 <- pltfunction(s_datanewclut9_,0.1,79,label="I")
plt9 <- pltfunction(s_datanewclut9_,label="I")
plt9 

################################################################################

combined_plot <- grid.arrange(plt1,plt2,plt3,plt4,plt5,plt6,plt7,plt8,plt9, ncol=2)

ggsave("AppiahFigureA1.tiff", combined_plot, width = 7, height = 9.5,dpi = 300)
