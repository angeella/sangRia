effectsPlotGroup <- function(var, group,data){
  if(!require("effects")){install.packages("effects")}
  if(!require("ggplot2")){install.packages("ggplot2")}
  f <- as.formula(paste(var, "~", paste(group)))
  mod <- lm(f,data=data)
  eff <- data.frame(effect(paste(group), mod))

  e1 <- ggplot(eff, aes(x=get(group), y=fit)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
    geom_line(aes(group = 1)) +
    geom_point(aes(x=get(group), y=fit))+
    xlab("Group") + ylab(var)
  return(e1)
}
