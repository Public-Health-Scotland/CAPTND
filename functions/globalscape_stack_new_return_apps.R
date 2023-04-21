
#################################.
### Stack new and return apps ###
#################################. 


stack_new_return_apps <- function(captnd_all){
  
  l_apps <- list(rbind.fill(captnd_all[[grep("app_new", names(captnd_all))]], # stack appointment data
                            captnd_all[[grep("app_return", names(captnd_all))]])) 
  names(l_apps) <- "apps"
  
  l_no_apps <- captnd_all[-c(grep("app_new", names(captnd_all)), # remove separate appointment data from captnd_all
                             grep("app_return", names(captnd_all)))]
  
  l_joined <- append(l_apps, l_no_apps) # combine stacked appointment data with captnd_all
  
  return(l_joined)
  
}


