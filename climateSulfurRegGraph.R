#investigate effect of sulfur ban
#The goal of this project is to investigate the effect of regulations put into place in 2020 regulating sulfur content in fuel of shipping vessels
#explained in more detail in this post: https://twitter.com/RARohde/status/1676529805502693377
#bases on this post I hypothesized that the temperature in a region, with a baseline substracted, would go up starting from 2020
#if the region had high shipping vessel density.
#to do this I collected global temperature data, measured how it changed in the first 800 days after jan 1 2020
#then visually compared this to shipping traffic density from this source: https://moverdb.com/shipping-traffic-density/
#I wasn't able to identify any patterns in this analysis, I think the effect is too small relative to regular climate variance
#some other sources: #https://www.carbonbrief.org/analysis-how-low-sulphur-shipping-rules-are-affecting-global-warming/
#https://www.science.org/doi/10.1126/sciadv.abn7988
#https://www.science.org/cms/10.1126/sciadv.abn7988/asset/88f5e6fa-33c2-4756-b38c-de960d3b7c8e/assets/images/large/sciadv.abn7988-f2.jpg
#this script takes about 8 minutes on my old computer, it outputs the graph I included a picture of

library(ncdf4)
library(raster)

downloadNC=function(url){
  download.file(url,"tempFile.nc",mode="wb")
  ncFile=nc_open("tempFile.nc")
  sstData=ncvar_get(ncFile,"sst")
  nc_close(ncFile)
  
  return(sstData)
}
timeSeriesSlope=function(sstMatrixList,rowN,colN){
  #return the slope of linear fit of measures at the same coordinate at different timepoints
  if(is.na(sstMatrixList[[1]][rowN,colN])){
    return(NA)}
  tempValues=matrix(nrow=length(sstMatrixList),ncol=2)
  for (i in 1:length(sstMatrixList)){
    tempValues[i,1]=i
    tempValues[i,2]=sstMatrixList[[i]][rowN,colN]
  }
  fit=lm(tempValues[,2]~tempValues[,1])
  
  return(max(min(coef(fit)[2],0.2),-0.2)) #return slope of fit  #clamp to -.2 ~ .2  for higher graph contrast
}



startDate=as.Date("2020/01/01")

yearData=list()
for(i in seq(from=0,to=800,by=40)){ #365
  date=startDate+i
  date2=date-800
  url=paste("https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",format(date,"%Y%m"),"/oisst-avhrr-v02r01.",format(date,"%Y%m%d"),".nc",sep="")
  url2=paste("https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",format(date2,"%Y%m"),"/oisst-avhrr-v02r01.",format(date2,"%Y%m%d"),".nc",sep="")
  currentYearData=downloadNC(url)
  currentYearData2=downloadNC(url2)
  yearData=append(yearData,list(currentYearData-currentYearData2)) #take change in temp from one year ago
}
rasterData=currentYearData #get correct format
for(row in 1 : dim(rasterData)[1]){ #repopulate with output from timeSeriesSlope function for each point
  for(col in 1 : dim(rasterData)[2]){
    rasterData[row,col]=timeSeriesSlope(yearData,row,col)
  }
}
rasterData=t(rasterData)
rasterData=raster(rasterData)
rasterData=flip(rasterData,dir=2)
palette = colorRampPalette(c("darkblue", "white", "darkred"))(255)
par(mar=c(0,0,0.8,0.25))
plot(rasterData,col=palette)
title(main="slope of 2020->2023 temperatures (baseline subtracted)")
