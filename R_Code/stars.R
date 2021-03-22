
  stars<- function(p_values){
      val<- rep("", length(p_values))
      for(i in 1:length(p_values)) {
       
       if(is.na(p_values[i])) { val[i] <- " "
       }else  if(p_values[i] <= 0.001){val[i] <-"***"
       } else if(p_values[i] <= 0.01){val[i] <-"**" 
       } else if(p_values[i] <= 0.05){val[i] <-"*"
       } else val[i] <-" "
      }
      val
  }
  
  