pkg <- c("tidyverse","wordcloud2","RColorBrewer","gmodels","modelr","nnet","tidyr","tm",'ggplot2','gridExtra',
         'ggsci','quanteda',"patchwork","grid","gridExtra")

new.packages <- pkg[!(pkg %in% installed.packages()[,"Package"])]

if(length(new.packages))install.packages(new.packages)

invisible(lapply((pkg),library, character.only = T))

setwd()
###################################################################
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
######################################################################################
#figure4

disp_data <- data_proces%>%
  mutate(clus = ifelse(cluster == 1,'Digital Elevation Model',''),
         clus = ifelse(cluster == 2,'Spatial Statistics',clus),
         clus = ifelse(cluster == 4,'Volunteered Geographic Information',clus),
         clus = ifelse(cluster == 5,'Urban Planning',clus),
         clus = ifelse(cluster == 6,'Cellular Automata',clus),
         clus = ifelse(cluster == 7,'Areal Interpolation',clus),
         clus = ifelse(cluster == 8,'Accessibility',clus),
         clus = ifelse(cluster == 9,'Spatial Optimization',clus),
         clus = ifelse(cluster == 3|cluster == 10, 'Mapping/
                       visualization',clus),
         Data_S = as.factor(Data_S))

disp_data_<-disp_data%>%
  count(clus, Data_S,name = "count")%>%
  ggplot()+aes(x=reorder(str_wrap(clus,3),count), y=count,fill=Data_S)+
  geom_bar(position="dodge", stat="identity",alpha = .9) + 
  theme(axis.line = element_line(colour = "gray87",
                                 linetype = "solid"), panel.background = element_rect(fill = NA,
                                                                                      linetype = "solid"), plot.background = element_rect(linetype = "solid")) +
  labs(x = "Themes", y = "Num. of Publications",
       fill = "Data Sources")+ #title = "Figure 3: Analytical Techniques by Data Sources"
  #scale_fill_viridis(discrete = TRUE)+
  #scale_fill_brewer(palette = 'Dark2',labels = c("Field Survey","Government", "Private","Two or More Sources"))+
  scale_fill_simpsons(labels = c("Field Survey","Government", "Private","Two or More Sources"))+
  geom_text(aes(label = count), 
            vjust = -0.2, position = position_dodge(.9),size = 3,color='gray12',fontface = 'bold') + 
  theme_minimal()+
  #theme(legend.position = c(0.2, 0.8)) + 
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 8),axis.title.x=element_text(size=6),
        #axis.title.y = element_text(size=6))+
  #scale_x_discrete(labels = datsour) + 
  theme(legend.position = c(0.2, 0.85))#,legend.key.size = unit(0.5,'cm'),
        #legend.text = element_text(size=7))

ggsave("AppiahFigure4.tiff", disp_data_,width=9.5, height=7,dpi = 300)

###################################################################################
fs <- read.csv("FinalprocesEdited.csv")

datsour <- c("Private","Field Survey", "Two or More Sources","Government")
plot4 <- fs%>%
  count(F_Y_N,Data_S, name = 'count')%>%
  ggplot() + aes(x=reorder(Data_S,count), y=count, fill = F_Y_N)+
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.line = element_line(colour = "gray87",
                                 linetype = "solid"), panel.background = element_rect(fill = NA,
                                                                                      linetype = "solid"), plot.background = element_rect(linetype = "solid")) +
  labs(x = "Data Sources", y = "Num. of Publications",
       fill = "Funding Status")+
  #scale_fill_viridis(discrete = TRUE)+
  theme_minimal()+
  scale_fill_simpsons(labels = c("Funded","No Funding"))+
  geom_text(aes(label = count), 
            vjust = -0.2, position = position_dodge(.9),size = 3,fontface = 'bold') + 
  #theme(legend.position = c(0.2, 0.8)) + 
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 8),legend.text = element_text(size=6))+
  scale_x_discrete(labels = datsour) + theme(legend.position = c(0.3, 0.67))#,axis.text.x = element_text(size=20),
                                             #axis.text.y = element_text(size=20),axis.title = element_text(size=30))
  

plot4

ggsave("AppiahFigure7.tiff", plot4,width=7, height=5, dpi = 300)

#################################################################################

plot4b <- fs%>%
  count(F_Y_N, Pub_Y, name = 'count')%>%
  mutate(`Funding Status`=ifelse(F_Y_N=='Funded','Funding',F_Y_N),
         `Funding Status`=ifelse(F_Y_N=='Not Funded', 'No Funding',`Funding Status`))%>%
  ggplot() + aes(Pub_Y, count,col=`Funding Status`, linetype = `Funding Status`, shape = `Funding Status`)+
  geom_point(size = 1.5)+
  geom_line(linewidth = 0.5)+
  scale_shape_discrete(solid = T)+
  labs(x = "Year of Publication", y = "Num. of Publications")+#, title = "Figure 4: Funding Status by Year of Publication"
  #scale_color_discrete(labels = c("Funded", "No Funding"),name="Funding Stats") +
  theme(axis.line = element_line(colour = "gray74",
                                 size = 0.9, linetype = "solid"), axis.ticks = element_line(size = 0.8),
        panel.background = element_rect(fill = NA,
                                        linetype = "solid"), legend.background = element_rect(fill = NA)) +
  theme(legend.key = element_rect(fill = NA))+
  theme_minimal()+
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 30),legend.text = element_text(size=30))+
  theme(legend.position = c(0.2, 0.8))+#,axis.text = element_text(size = 20),axis.title = element_text(size=30))+
  geom_text(aes(label = count), 
            vjust = -.5,size = 3, show.legend = F)


plot4b

ggsave("AppiahFigure6.tiff", plot4b,width=7, height=5, dpi = 300)

#################################################################################
plot1b <- fs%>%
  count(Data_S, Pub_Y, name = 'count')%>%
  rename(`Data Sources` = 'Data_S')%>%
  ggplot() + aes(Pub_Y, count, col=`Data Sources`, linetype = `Data Sources`, shape=`Data Sources`)+
  geom_point(size = 1.5)+
  geom_line(linewidth = 0.5)+
  scale_shape_discrete(solid = T)+
  labs(x = "Year of Publication", y = "Num. of Publications")+ #title = "Figure 2: Data Sources by Year of Publication"
  scale_color_discrete(labels = c("Field Survey", "Government","Private","Two or More Sources")) + theme(axis.line = element_line(colour = "gray74",
                                                                                                                                  size = 0.9, linetype = "solid"), axis.ticks = element_line(size = 0.8),
                                                                                                         panel.background = element_rect(fill = NA,
                                                                                                                                         linetype = "solid"), legend.background = element_rect(fill = NA)) +
  theme(legend.key = element_rect(fill = NA))+
  theme_minimal()+
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 30),legend.text = element_text(size=30))+
  theme(legend.position = c(0.2, 0.8))+#,axis.text = element_text(size = 20),axis.title = element_text(size=30))+
  geom_text(aes(label = count), 
            vjust = -.5,size = 2, show.legend = F)+
  scale_linetype_manual(values=c('solid','twodash','dotdash','longdash'))

plot1b

ggsave("AppiahFigure2.tiff", plot1b,width=7, height=5, dpi = 300)

################################################################################

pub_t = read.csv("Pub_t.csv")%>%
  pivot_longer(c("AAAG","CEUS","EPB","IJGIS","TGIS","GA"),names_to = "Journals",values_to = "count")

#Creating line graphs with ggplot

pub_trend <- pub_t%>%
  ggplot() + aes(Year, count, col=Journals, linetype = Journals, shape = Journals)+
  geom_point(size = 1.5)+
  geom_line(linewidth = 0.5)+
  scale_shape_discrete(solid = T)+
  labs(x = "Year of Publication", y = "Num. of Publications")+ #title = "Figure 1: Journals by Year of Publication"
  #scale_color_discrete(labels = c("AAAG","CEUS","EPB","IJGIS","TGIS","GA")) + 
  theme(axis.line = element_line(colour = "gray74", size = 0.9, linetype = "solid"), 
        axis.ticks = element_line(size = 0.8), panel.background = element_rect(fill = NA,
                                                                               linetype = "solid"), legend.background = element_rect(fill = NA)) +
  theme(legend.key = element_rect(fill = NA))+
  theme_minimal()+
  #theme(plot.title = element_text(size = 16),legend.text = element_text(size=30)) + 
  #theme(legend.title = element_text(size = 30),axis.text = element_text(size = 20),axis.title = element_text(size=30))+
  theme(legend.position = c(0.2, 0.8))+
  scale_linetype_manual(values=c('solid','dashed','twodash','dotted','dotdash','longdash'))
#geom_text(aes(label = count),vjust = -.5,size = 4, show.legend = F)

pub_trend
ggsave("AppiahFigure1.tiff", pub_trend,width=7, height=5, dpi = 300)

################################################################################
stackedbar <- fs %>%
  select(J_Name,Data_S)%>%
  group_by(J_Name,Data_S)%>%
  summarise(count=n())

figure <- ggplot(stackedbar, aes(fill=Data_S, y = count, x= reorder(J_Name,count)))+
  geom_bar(position="stack",stat = "identity")+
  theme(axis.line = element_line(colour = "gray87",
                                 linetype = "solid"), panel.background = element_rect(fill = NA,
                                                                                      linetype = "solid"), plot.background = element_rect(linetype = "solid")) +
  labs(x = "Journals", y = "Num. of Publications",
       fill = "Data Sources")+
  #scale_fill_viridis(discrete = TRUE)+
  theme_minimal()+
  scale_fill_simpsons()+
  geom_text(aes(label = count),position = position_stack(vjust= 0.5),size = 2,fontface = 'bold') + 
  #theme(legend.position = c(0.2, 0.8)) + 
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 30),legend.text = element_text(size=30))+
  theme(legend.position = c(0.3, 0.67))#,axis.text.x = element_text(size=20),
        #axis.text.y = element_text(size=20),axis.title = element_text(size=30))
figure
ggsave("AppiahFigure3.tiff", figure,width=7, height=5, dpi = 300)

#################################################################################
df <- read.csv("Sprivatedata_.csv")

df_ <- df|>
  group_by(J_Name,PublishedData)|>
  summarise(co = n())|>
  ggplot()+aes(x=reorder(J_Name,co), y=co, fill=PublishedData)+
  geom_bar(position="stack",stat = "identity")+
  theme(axis.line = element_line(colour = "gray87",
                                 linetype = "solid"), panel.background = element_rect(fill = NA,
                                                                                      linetype = "solid"), plot.background = element_rect(linetype = "solid")) +
  labs(x = "Journals", y = "Num. of Publications",
       fill = "Data used for analysis published \nalongside articles")+
  #scale_fill_viridis(discrete = TRUE)+
  theme_minimal()+
  scale_fill_simpsons()+
  geom_text(aes(label = co),position = position_stack(vjust= 0.5),size = 3,fontface = 'bold') + 
  #theme(legend.position = c(0.2, 0.8)) + 
  #theme(plot.title = element_text(size = 16)) + 
  #theme(legend.title = element_text(size = 30),legend.text = element_text(size=30))+
  theme(legend.position = c(0.3, 0.67))#,axis.text.x = element_text(size=20),
        #axis.text.y = element_text(size=20),axis.title = element_text(size=30))

ggsave("AppiahFigure5.tiff", df_,width=7, height=5, dpi = 300)

