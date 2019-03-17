
getProportions=function(df,col,lab){
  colmean=apply(df,col,mean)#by areas
  prop=round(df/colmean,2)
  colnames(prop)=paste0(lab,seq(1,ncol(df)))
  return(prop)}


getMergedData=function(msoaLookup,msoaShapeData, aggVar,upperShapeData){
  colnames(upperShapeData)=  toupper(colnames(upperShapeData))
  colnames(msoaShapeData)=toupper(colnames(msoaShapeData))
  selectedLookup=msoaLookup[,c('MSOA11CD',aggVar)]
  msoaShapeNames=colnames(msoaShapeData)
  selectedColumns=paste0(c(aggVar,msoaShapeNames[grepl('TOPIC', msoaShapeNames)]),collapse=',')
  query=paste0('select ',selectedColumns,' from msoaShapeData a inner join selectedLookup b on a.MSOA11CD=b.MSOA11CD')
  selectedData=sqldf(query)
  aggFor=as.formula(paste0('.~',aggVar))
  aggregatedData=aggregate(aggFor, selectedData, sum)
  weight=aggregatedData[,grepl('TOPIC', colnames(aggregatedData))]
  weight_tot=apply(weight,1,sum)
  weight=weight/weight_tot
  aggPropIn=getProportions(weight,1,'PROP_IN_')
  aggPropAc=getProportions(weight,2,'PROP_AC_')
  aggProp=cbind(aggPropAc,aggPropIn)
  aggProp[[aggVar]]=aggregatedData[[aggVar]]
  mergedata=sqldf(paste0('select * from upperShapeData a left outer join aggProp b on a.',aggVar,'=b.',aggVar))
  return(mergedata)}
