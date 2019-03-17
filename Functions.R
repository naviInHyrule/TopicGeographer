
##############################################################

getTopicsList=function(df){#df has two columns: topicID, Word.
  uniqueTopics=sort(unique(df$topicID))
  varList=list()
  for (i in uniqueTopics){
    firstWord=df[df$topicID==i,'Word'][1]
    varList[paste0(as.character(i),':',firstWord)]=i}
  return(varList)}

mergeLists=function(x,y,dimname){
  vecGrid<- expand.grid(x, y)
  resList <- c()
  for(i in 1:nrow(vecGrid)){
    name=paste0(dimname,"Plot", vecGrid[i,1],"Type",vecGrid[i,2])
    resList<-c(resList,name)}
  return(resList)}

getMap=function(areaShape,selVar,type,areaCode){
if(type==1){palette='Reds';title='Topic Weight'}
if(type==2){palette='-RdBu';title='Proportions within Area' }
if(type==3){palette='-RdBu';title='Proportions across Areas'}
  
msoa_map=  tm_shape(areaShape)  + tm_borders(alpha=0.3)+
  tm_fill(selVar, palette = 'Reds', style = "pretty", title=title,
          popup.vars=c('Name'=areaCode,'Weight'=selVar),legend.position = c("left","top"))+
  tm_view(alpha=0.8,legend.position = c("right", "top"))+
  tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)+
  tm_scale_bar(position = c("left", "bottom")) 
return(msoa_map)}

getTopicDesc=function(df,t){#df has two columns: topicID, Word; t=topic
  prods = df[df$topicID==t,'Word']
  topicProds = data.frame(x=rep(1,length(prods)),y=seq(length(prods),1,-1), prod=prods)
  return(topicProds)
}


getTopicPlot=function(topicProds){
  g=ggplot(topicProds, aes(x, y, label = prod))+
    geom_text(size=3) +
    labs(y="",x="",title='Characteristical Words')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))
  return(g)}

################################
