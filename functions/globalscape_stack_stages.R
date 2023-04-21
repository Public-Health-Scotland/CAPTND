
###################################################.
### Stack globalscape stages (for CAMHS and PT) ###
###################################################.




stack_stages <- function(captnd_all){
  
  list_bucket <- list()
  
  l_captnd_all <- length(captnd_all)/2 # half total number of list elements as two of each (CAMHS and PT)
  
  for( i in 1:l_captnd_all){
    
    df_joined <- rbind.fill(captnd_all[[i]], captnd_all[[i+l_captnd_all]])
    
    list_bucket[[i]] <- df_joined 
  }
  
  stage_names <- c("referral", "app_new", "app_return", "diagnosis", "outcomes", "discharge")
  
  names(list_bucket) <- stage_names
  
  return(list_bucket)
  
}