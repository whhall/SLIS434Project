install.packages("usmap")
library(usmap)

library(learningr)
library(ggplot2)
library(janitor)
install.packages("ggpubr")
library(ggpubr)
install.packages("tidyverse")
library(tidyverse)
library(Hmisc)
install.packages("corrplot")
library(corrplot)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

require(learningr)
require(ggplot2)
require(janitor)
require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)
require(PerformanceAnalytics)

COVID_data_file <- file.choose()
df <- read.csv(COVID_data_file)
View(df)

f_random_state <- function(){
  state_row <- sample(1:nrow(df), 1)
  df[state_row,]
}

f_random_state()

f_variable <- function(x){
  df[c("State", x)]
}

f_variable("COVID.19.Cases")

f_variable_rate <- function(){
  var <- readline(prompt = "Enter a variable:")
  max_index <- which(df[var] == max(df[var]))
  min_index <- which(df[var] == min(df[var]))
  
  paste(df$State[max_index], " has the most ", var, ", and ", df$State[min_index], " has the least ", var, sep = "")

}

f_variable_rate()


f_variable_rate_max_min <- function(){
  for(i in names(df)){
    if(i != "State"){
      max_index <- which(df[i] == max(df[i]))
      min_index <- which(df[i] == min(df[i]))
      print(paste("Max rate for", i, "happened in", df$State[max_index], "and the Min rate of", i, "happened in", df$State[min_index]))
    }
  }
}

f_variable_rate_max_min()

f_high_variable_rate_states <- function(){
  var <- readline(prompt = "Enter a variable:")
 
  avg <- mean(df[,var])
  
  print(paste("The average of", var, "is", avg))
  print(paste("Below are states with", var, "that are greater than", avg))
     
  above_avg_df <- df[which(df[var] > avg),]
  print(above_avg_df[c("State", var)])
}

f_high_variable_rate_states()

f_plot_variable <- function(v1, v2, c, s, x, y){
  par(mai=c(0.9,0.9,0.3,0.07))
  
  ggplot(df, aes_string(x = v1, y = v2))  + 
  geom_point(stat = "identity", colour = c, shape = s) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + xlab(x) + ylab(y)
}

f_plot_variable("State", "COVID.19.Cases", "red", 2, "State", "Number of Cases")

f_US_Map_Variable <- function(v, c, lc, hc){
  df_2 <- df
  df_2$fips <- fips(df_2$State)
  
  plot_usmap(data = df_2, values = v, color = c) + 
    scale_fill_continuous(low = lc, high = hc, label = scales::comma) +
    theme(legend.position = "right")
}

f_US_Map_Variable("COVID.19.Cases", "red", "blue", "orange")


f_US_Map_Variable_Reg_Div <- function(v, r, c, lc, hc){
  df_2 <- df
  df_2$fips <- fips(df_2$State)
  
  plot_usmap(data = df_2, include = r, values = v, color = c) + 
    scale_fill_continuous(low = lc, high = hc, label = scales::comma) +
    theme(legend.position = "right")
}

f_US_Map_Variable_Reg_Div("COVID.19.Cases", c(.south_region),  "red", "blue", "orange")

f_US_Map_Variable_Correlation <- function(x, y){
  ggscatter(df, x = x, y = y,
            add = "reg.line", 
            conf.int = TRUE,
            cor.coef = TRUE,
            cor.method = "pearson",
            xlab = x, ylab = y)
}

f_US_Map_Variable_Correlation("COVID.19.Death.Rate", "COVID.19.Cases")

cor_df
f_US_Map_Variable_Correlogram <- function(m, t, s) {
  cor_df <- rcorr(data.matrix(df[,2:ncol(df)]))
  
  M_df <- cor_df$r
  p_mat_df <- cor_df$P
  
  
  corrplot(M_df, method = m, type = t, order = "hclust", 
           p.mat = p_mat_df, sig.level = s, insig = "blank")
}

f_US_Map_Variable_Correlogram("color","lower", "0.05")

f_US_Map_Variable_Multi_Figs_corr <- function(){
  par(mar = c(0,0,0,0))
  
  chart.Correlation(df[,2:ncol(df)], histogram = TRUE, pch = 19)
}

f_US_Map_Variable_Multi_Figs_corr()
