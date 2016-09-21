#Read data
#install.packages("readr")
library(readr)
setwd("C:/Users/Administrator/Desktop/ADS/Proj 1/Data")
a14=read_csv("ss14pusa.csv")
b14=read_csv("ss14pusb.csv")
all14=rbind(a14,b14)
rm(a14);rm(b14)
gc();gc();Sys.sleep(10);gc();gc() #To make sure clean all memory in case of crash
ST_name=read.csv("statenames.csv") #The redundant space in abbr column has been removed in Excel
state.name

library(dplyr)
attach(all14)
names(all14)
selct=select(all14,OCCP,ST,WAGP,AGEP,JWMNP,ESR,SCHL,SEX,WKHP,NAICSP,PWGTP,POWSP,POWPUMA)%>%
      na.omit()%>%
      mutate(WAGP=as.numeric(WAGP),
             AGEP=as.numeric(AGEP),
             WKHP=as.numeric(WKHP),
             JWMNP=as.numeric(JWMNP),
             PWGTP=as.numeric(PWGTP),
             ST=as.integer(ST))
      

head(selct)

group_ST=group_by(selct,ST)
group_OCCP=group_by(selct,OCCP)
group_ALL=group_by(selct,ST,OCCP)
sumrz_ST=summarise(group_ST,
                   Meanwage=weighted.mean(WAGP,w=PWGTP),
                   MeanAGEP=weighted.mean(AGEP,w=PWGTP),
                   MeanWKHP=weighted.mean(WKHP,w=PWGTP),
                   MeanJWMNP=weighted.mean(JWMNP,w=PWGTP)
                    )%>%
          mutate(ST.map=match(ST_name$abbr[-52],state.fips$abb,nomatch=99))%>%
          mutate(abb=state.fips$abb[ST.map])%>%
          arrange(ST.map)
map.data=merge(state.fips,sumrz_ST,by="abb",all=T)
stlist=read.csv("rr.csv")
maplist=cbind(state.fips,Num=c(1:dim(state.fips)[1]))
library(maps)
library(leaflet)
mapStates = map("usa", fill = TRUE, plot = T,col=c(1:50))
map=leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fill=T, stroke = FALSE)


pal <- colorNumeric(
  palette = "Blues",
  domain = map.data$Meanwage
)


mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor =pal(map.data$Meanwage), fillOpacity=0.5,stroke = FALSE)


library(rgdal)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("countries.geojson", "OGRGeoJSON")

map <- leaflet(countries)

match(ST_name$abbr,state.fips$abb)




