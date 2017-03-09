#Here we import a dataset from National Agricultural Statitistics Service 
#(NASS, https://quickstats.nass.usda.gov/) that focuses on field crops in Iowa.

library(readr) 
crops<-read_csv("NASS-Iowa.csv")  #ctr+enter or command+enter will run the line you are on 

#Let's look at the different eras of corn
crops%>%
  select(Year, State, Commodity, `Data Item`, Value)%>%   #choose just the columns we want to work with
  rename(Data = `Data Item`, bu_acre = Value)%>%          #get rid of annoying names
  filter(Year > 1866, 
         Data == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")%>%  #choose just the row we want data for
  mutate(era = ifelse ((Year %in% c(1867:1940)), "prehybrid",      #make a new column with era labels
                        ifelse((Year %in% c(1941:1990)), "old",    #conditional upon years
                                ifelse ((Year %in% c(1991:2015)), 
                                        "new", "nope"))))%>%
  ggplot(aes(x=Year, y=bu_acre, group=era, color=era))+            #pipe this into ggplot             
  geom_point()+
  geom_smooth(method=lm)


#######################################################
#Now we want to look at small grains and beans

crops%>%
  select(Year, State, Commodity, `Data Item`, Value)%>%
  filter(Commodity %in% c("OATS", "BARLEY", "WHEAT", "RYE", "SOYBEANS") & 
           `Data Item` %in% c("OATS - ACRES HARVESTED",
                              "BARLEY - ACRES HARVESTED",
                              "WHEAT - ACRES HARVESTED",
                              "RYE - ACRES HARVESTED",
                              "SOYBEANS - ACRES HARVESTED"))%>%
  mutate(size = ifelse ((Commodity %in% c("OATS", "BARLEY", "WHEAT", "RYE")),
                        "small","soybean"))%>%
  group_by(size, Year)%>%
  summarise(total=sum(Value))%>%
  ggplot(aes(x=Year, y=total, group=size, color=size))+
  geom_point()+
  geom_smooth()+
  ggtitle("Changes in small grain and soybean acres in Iowa")+
  labs(y="Total acres harvested")
