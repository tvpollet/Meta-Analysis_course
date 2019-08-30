variance.distribution.3lm <- function(data, m){
  
  # Calculate estimated sampling variance and proportions across levels  
  data <- data
  m <- m
  n <- length(data$v)
  vector.inv.var <- 1/(data$v)
  sum.inv.var <- sum(vector.inv.var)
  sum.sq.inv.var <- (sum.inv.var)^2
  vector.inv.var.sq <- 1/(data$v^2)
  sum.inv.var.sq <- sum(vector.inv.var.sq)
  num <- (n-1)*sum.inv.var
  den <- sum.sq.inv.var - sum.inv.var.sq
  est.samp.var <- num/den
  level1<-((est.samp.var)/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level2<-((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level3<-((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  Level<-c("Sampling error", "District", "School") # Note that this has been change as level 1/2/3.
  Variance<-c(level1, level2, level3)
  df<-data.frame(Level, Variance)
  df1<-df
  colnames(df1) <- c("Level", "% of total variance")
  
  # Generate plot
  df$distribution<-"Distribution"
  df$Level<-factor(df$Level, levels(df$Level)[c(1,3,2)]) # reordered in a sensible matter.
  g <- ggplot(df, aes(fill=Level, y=Variance, x=distribution)) + 
    geom_bar(stat="identity", position="fill", width = 0.3) + coord_flip(ylim = c(1,0)) + scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.line.x = element_line(colour = "black",
                                     size = 0.5, linetype = "solid"),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm")) + guides(fill = guide_legend(reverse = TRUE))
  return(list(g, df1))
}