library(ggplot2)
require(ggplot2)
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

