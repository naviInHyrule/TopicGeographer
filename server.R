library(shiny)
library(ggplot2)  # for the diamonds dataset
library(sqldf)
library(tmap)
library(rgdal)
library(raster)
library(visNetwork)
library(leaflet)
setwd('Replace me!')
source("Functions.R")
source("AggregatingFunctions.R")

###########################################################
###Upload and create pertinent variables
topicWords<-read.csv('Data/random_topic_words.csv',header=T, stringsAsFactors = F)
uniqueTopics=sort(unique(topicWords$topicID))
topicsList=getTopicsList(topicWords)

rmsoa<-read.csv('Data/random_msoa.csv',header=T, stringsAsFactors = F)

fd='Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales'
msoaShape<-readOGR(paste0('./',fd),fd)#7201

fd='Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain'
laShape<-readOGR(paste0('./',fd),fd, stringsAsFactors = F)#380

fd='Regions_December_2016_Ultra_Generalised_Clipped_Boundaries_in_England'
regionShape<-readOGR(paste0('./',fd),fd, stringsAsFactors = F)#9

msoaShapeDist=rmsoa
msoaShape=merge(msoaShape,msoaShapeDist, by='msoa11cd')
colnames(msoaShape@data)=toupper(colnames(msoaShape@data))

msoaShapeDist=msoaShape@data
weight=msoaShapeDist[,grepl('TOPIC', colnames(msoaShapeDist))]
propIn=getProportions(weight,2,'PROP_IN_')
propAc=getProportions(weight,1,'PROP_AC_')
proportions=cbind(propAc,propIn)
msoaShape@data=cbind(msoaShape@data,proportions)

msoaLookup<-read.csv('msoa_lookup.csv',header=T, stringsAsFactors = F)
colnames(msoaLookup)[7]='RGN16CD'#i'm sorry,but I didnt want to download another lookup
colnames(msoaLookup)[8]='RGN16NM'

aggVar='LAD17CD'
upperShapeData=laShape@data
msoaShapeData=msoaShape@data
mergedLaData=getMergedData(msoaLookup,msoaShapeData, aggVar,upperShapeData)
laShape@data=mergedLaData

aggVar='RGN16CD'
upperShapeData=regionShape@data
msoaShapeData=msoaShape@data
mergedReData=getMergedData(msoaLookup,msoaShapeData, aggVar,upperShapeData)
regionShape@data=mergedReData

GeoList=list()
GeoList[['Region']]<-as.integer(1)
GeoList[['Local Authority']]<-as.integer(2)
GeoList[['Middle-Layer Super Output Area']]<-as.integer(3)

###########################################################
server <- function(input, output, session) {
  
  output$select_topic <- renderUI({
    selectInput("TOPIC", "TopicID:", choices=topicsList, selected = topicsList[1])})
  
  output$select_area <- renderUI({
    radioButtons("AREA", "Area:", choices=GeoList, selected = GeoList[1])})
  
  topicInput <- reactive({
    topicProds=getTopicDesc(topicWords,input$TOPIC)
    return(topicProds)})
  
  
  output$topicProdsPlot <- renderPlot({
    topicProds=topicInput()
    getTopicPlot(topicProds)})
  
  output$within_map <- renderLeaflet({
    selVar=paste0('PROP_IN_',input$TOPIC)
    if(input$AREA==1){datashape=regionShape; areaCode='RGN16NM'}
    if(input$AREA==2){datashape=laShape; areaCode='LAD17NM'}
    if(input$AREA==3){datashape=msoaShape; areaCode='MSOA11NM'}
    msoa_map=getMap(datashape,selVar,2,areaCode)
    tmap_leaflet(msoa_map)%>% 
      setView(lng = -1.5, 53, zoom = 6) 
  })
  
  output$across_map <- renderLeaflet({
    selVar=paste0('PROP_AC_',input$TOPIC)
    if(input$AREA==1){datashape=regionShape; areaCode='RGN16NM'}
    if(input$AREA==2){datashape=laShape; areaCode='LAD17NM'}
    if(input$AREA==3){datashape=msoaShape; areaCode='MSOA11NM'}
    msoa_map=getMap(datashape,selVar,3,areaCode)
    tmap_leaflet(msoa_map) %>% 
      setView(lng = -1.5, 53, zoom = 6) 
  })
  
}



#runApp(list(ui = ui, server = server),launch.browser = TRUE)


