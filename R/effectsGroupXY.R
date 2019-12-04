effectGroupXY <- function(x, y, cov, data, group){
  if(!require("effects")){install.packages("effects")}
  if(!require("ggplot2")){install.packages("ggplot2")}
  #Anova modello con Group scala e interazione e senza
  f1 <- as.formula(paste(y, "~", paste(c(cov, x, paste(x,":",group)), collapse=" + ")))
  f2 <- as.formula(paste(y, "~", paste(c(cov), collapse=" + ")))
  mod1 <- lm(f1,data=data)
  mod2 <- lm(f2,data=data)

  anResults <-anova(mod1,mod2,test="F")

  effscala <- data.frame(effect(paste(x), mod1))

  efPlot <- ggplot(effscala, aes(x= effscala[,1], y=fit)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
    geom_line(aes(group = 1)) +
    geom_point(aes(x= effscala[,1], y=fit)) +
    xlab(paste(x)) + ylab(paste(y))
  #eff size calcolato su R^2 dei due modelli (completo e senza gruppo ed interazione)
  #pR <- 1 - (anResults1$`Resid. Dev`[1]/anResults1$`Resid. Dev`[2])
  #effSize <- round(pR/(1-pR),4)
  f5 <- as.formula(paste(y, "~", paste(c(x), collapse=" + ")))
  f6 <- as.formula(paste(y, "~", paste(1, collapse=" + ")))

  mod11 <-summary(lm(f5,data=data))
  effSize <- mod11$adj.r.squared / (1-mod11$adj.r.squared)
  # f2B effect sizes of 0.02, 0.15, and 0.35 are termed small, medium, and large, respectively
  # represent small, medium, and large effect sizes, respectively.
  return(res=list(anResults,efPlot,effSize,effscala,mod11))
}
