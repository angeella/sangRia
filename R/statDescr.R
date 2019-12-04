#Descriptive Statistics plus Boxplot

statDescr <- function(var= "Sepal.Length", group= NULL, data= iris){

  #check package
  if(!require("dplyr")){install.packages("dplyr")}
  if(!require("ggplot2")){install.packages("ggplot2")}
  #if(!require("DT")){install.packages("DT")}
  #check dataframe
  if(!is.data.frame(data)){data = as.data.frame(data)}
  #check group
  if(is.null(group)){
    dataR = data
    dataR$Total <- rep("0",nrow(data))
    dataR$Total <- as.factor(dataR$Total)
    group = "Total"
  }else{dataR = data}
  #summary statistics
  overview <- as.data.frame(dataR %>% dplyr::group_by(get(group)) %>% dplyr::summarize(mean=round(mean(get(var)),3),
                                                                                        sd=round(sd(get(var)),3),
                                                                                        median = round(median(get(var)),3),
                                                                                        quartile1 = round(quantile(get(var),0.25),3),
                                                                                        quartile3 = round(quantile(get(var),0.75),3),
                                                                                        min = round(min(get(var)),3),
                                                                                        max= round(max(get(var)),3)))
  colnames(overview)[1] <- group
  plot1 <- ggplot(dataR, aes(x=get(group), y=get(var), fill=get(group))) + geom_boxplot() +
    xlab(paste(group)) + ylab(paste(var)) + guides(fill=guide_legend(title="Group"))




  plot2 <- ggplot(dataR,aes(x=get(var)))+geom_histogram()+facet_grid(~get(group))+theme_bw() +
    xlab(paste(var))


  if(table(get(group,dataR)) == nrow(dataR)){
    plot1 = plot1 + guides(fill=FALSE)
    plot2 = plot2 + scale_color_discrete(name="")
    }

  return(out=list(overview,plot1, plot2))
}
