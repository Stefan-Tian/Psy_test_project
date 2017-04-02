happiness_1<-read_excel("online+data+chapter+2+whr+2017.xlsx", sheet = 1)
happiness_1<-as.data.table(happiness_1)
happiness_2016<-happiness_1[year==2016]
happiness_2015<-happiness_1[year==2015]
happiness_2014<-happiness_1[year==2014]
happiness_3<-read_excel("online+data+chapter+2+whr+2017.xlsx", sheet = 3)
happiness_3<-as.data.table(happiness_3)
my_plot<-ggplot(happiness_3, aes(x=Country))+
         theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=60, hjust=1, face="bold"))
horizontal_plot<-ggplot(happiness_3, aes(x=Country))+
                 theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold"))+
                 coord_flip()
score_plot<-my_plot+
            geom_bar(stat="identity", aes(y=`Happiness score`, fill=I("#006666")))
GDP_plot<-my_plot+
          geom_bar(stat="identity", aes(y=`Explained by: GDP per capita`, fill=I("#339999")))
Social_plot<-my_plot+
             geom_bar(stat="identity", aes(y=`Explained by: Social support`, fill=I("#0066CC")))
Healthy_plot<-my_plot+
              geom_bar(stat="identity", aes(y=`Explained by: Healthy life expectancy`, fill=I("#003366")))
Freedom_plot<-my_plot+
              geom_bar(stat="identity", aes(y=`Explained by: Generosity`, fill=I("#0033FF")))
Generosity_plot<-my_plot+
                 geom_bar(stat="identity", aes(y=`Explained by: Generosity`, fill=I("#6699CC")))
Corruption_plot<-my_plot+
                 geom_bar(stat="identity", aes(y=`Explained by: Perceptions of corruption`, fill=I("#0099CC")))
compare<-function(x)cor(x, happiness_3$`Happiness score`)
apply(happiness_3[,5:10], 2, compare)
#Top5<-subset(mvt,
             #LocationDescription%in%
               #c("STREET","ALLEY",
                 #"GAS STATION","PARKING LOT/GARAGE(NON.RESID.)",
                 #"DRIVEWAY - RESIDENTIAL")) you can change like this
group1<-subset(happiness_3, Country=="France"|Country=="Belgium"|
               Country=="Ireland"|Country=="Luxembourg"|Country=="Monaco"|Country=="Netherlands"|
               Country=="Switzerland"|Country=="United Kingdom"|Country=="United States"|Country=="Canada")

group1_plot<-ggplot(group1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, face="bold"))

score_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Happiness score`),
                                y=`Happiness score`, fill=I("#006666")))+
  geom_text(aes(x=reorder(Country, `Happiness score`),
                y=`Happiness score`, label=round(`Happiness score`, 4)), vjust=-0.5)+
  scale_y_continuous(limits=c(0,7.5), breaks=seq(0, 7.5, 0.5))

GDP_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Explained by: GDP per capita`),
                                y=`Explained by: GDP per capita`, fill=I("#339999")))+
  geom_text(aes(x=reorder(Country, `Explained by: GDP per capita`),
                y=`Explained by: GDP per capita`, label=round(`Explained by: GDP per capita`, 4)), 
                vjust=-0.4)

Social_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country,`Explained by: Social support`),
                                y=`Explained by: Social support`, fill=I("#0066CC")))+
  geom_text(aes(x=reorder(Country,`Explained by: Social support`),
                y=`Explained by: Social support`, label=round(`Explained by: Social support`, 4)), vjust=-0.4)

Healthy_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Explained by: Healthy life expectancy`),
                                y=`Explained by: Healthy life expectancy`, fill=I("#003366")))+
  geom_text(aes(label=round(`Explained by: Healthy life expectancy`, 4),
                x=reorder(Country, `Explained by: Healthy life expectancy`),
                y=`Explained by: Healthy life expectancy`), vjust=-0.4)

Freedom_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Explained by: Freedom to make life choices`),
                                y=`Explained by: Freedom to make life choices`, fill=I("#0033FF")))+
  geom_text(aes(label=round(`Explained by: Freedom to make life choices`, 4),
                    x=reorder(Country, `Explained by: Freedom to make life choices`),
                    y=`Explained by: Freedom to make life choices`), vjust=-0.4)

Generosity_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Explained by: Generosity`),
                                y=`Explained by: Generosity`, fill=I("#6699CC")))+
  geom_text(aes(label=round(`Explained by: Generosity`, 4),
                    x=reorder(Country, `Explained by: Generosity`),
                    y=`Explained by: Generosity`), vjust=-0.4)

Corruption_plot_group<-group1_plot+
  geom_bar(stat="identity", aes(x=reorder(Country, `Explained by: Perceptions of corruption`),
                                y=`Explained by: Perceptions of corruption`, fill=I("#0099CC")))+
  geom_text(aes(label=round(`Explained by: Perceptions of corruption`, 4), 
                    x=reorder(Country, `Explained by: Perceptions of corruption`),
                    y=`Explained by: Perceptions of corruption`), vjust=-0.4)
library(gridExtra)
grid.arrange(GDP_plot_group, Social_plot_group, Healthy_plot_group,
             Freedom_plot_group, Generosity_plot_group, Corruption_plot_group, ncol=3)
score_plot_group
compare_group<-function(x)cor(x, group1$`Happiness score`)
group1$`Explained by: Perceptions of corruption`<- 4-group1$`Explained by: Perceptions of corruption`
apply(group1[5:10], 2, compare_group)
