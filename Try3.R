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

occupa=read.csv("OCCP.csv")
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
             ST=as.integer(ST),
             SCHL=as.integer(SCHL),
             SEX=as.integer(SEX),
             OCCP=as.integer(OCCP))
noted.selct=merge(selct,occupa,by="OCCP")
      

head(selct)

group_ST=group_by(selct,ST)
sumrz_ST=summarise(group_ST,
                   Meanwage=weighted.mean(WAGP,w=PWGTP),
                   MeanAGEP=weighted.mean(AGEP,w=PWGTP),
                   MeanWKHP=weighted.mean(WKHP,w=PWGTP),
                   MeanJWMNP=weighted.mean(JWMNP,w=PWGTP)
                    )%>%
          mutate(ST.map=match(ST_name$abbr[-52],state.fips$abb,nomatch=99))%>%
          mutate(abb=state.fips$abb[ST.map])%>%
          arrange(ST.map)
map.data=merge(state.fips,sumrz_ST,by="abb",all=T)%>%
         arrange(ST.map)
library(maps)
library(leaflet)

pal1 <- colorNumeric(
  palette = "OrRd",
  domain = map.data$Meanwage
)


mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor =pal1(map.data$Meanwage), fillOpacity=0.5,stroke = FALSE)

pal2 <- colorNumeric(
  palette = "Blues",
  domain = map.data$MeanWKHP
)


mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor =pal2(map.data$MeanWKHP), fillOpacity=0.5,stroke = FALSE)


pal3 <- colorNumeric(
  palette = "OrRd",
  domain = map.data$Meanwage/map.data$MeanWKHP
)


mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor =pal3(map.data$Meanwage/map.data$MeanWKHP), fillOpacity=0.5,stroke = FALSE)

group_OCCP=group_by(noted.selct,OCCABB)
sumrz_OCCP=summarise(group_OCCP,
                   Meanwage=weighted.mean(WAGP,w=PWGTP),
                   MeanAGEP=weighted.mean(AGEP,w=PWGTP),
                   MeanWKHP=weighted.mean(WKHP,w=PWGTP),
                   MeanJWMNP=weighted.mean(JWMNP,w=PWGTP),
                   MeanSCHL=weighted.mean(SCHL,w=PWGTP),
                   MeanSEX=weighted.mean(SEX,w=PWGTP),
                   Count=sum(PWGTP)
)
library(rbokeh)
n <- nrow(sumrz_OCCP)
ramp <- colorNumeric(
  palette = "Blues",
  domain = sumrz_OCCP$MeanSCHL
)

figure() %>%
  ly_points(x=Meanwage,y=MeanWKHP, hover=OCCABB,color = ramp(sumrz_OCCP$MeanSCHL), size = Count/min(Count),data=sumrz_OCCP) %>%
  tool_box_select() %>%
  tool_lasso_select()

plot(MeanWKHP~Meanwage,data=sumrz_OCCP)
