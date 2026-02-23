

uhf_calc <- function(nd, debug=FALSE){
  
  UHFnests <- unlist(nd %>% filter(fate == 8| fate == 9) %>% select(nest))
  
  # view the UH/UF nests:
  if (debug) nd %>% filter(fate==8 | fate==9) %>% select(nest, field_fate, cam_fate, fate, cfate)
 
  # change the fate values based on whether UHisH is TRUE or not: 
  if(UHisH){
    nd$fate[nd$fate==8] = 1
  }else{
    nd$fate[nd$fate==8] = 7
  }
  
  # same with UFisF:
  if(UFisF) nd$fate[nd$fate==9] = 0 else nd$fate[nd$fate==9] = 7
  
  # view the same nests again:
  # nd %>% filter(nest %in% UHnests) %>% select(nest, field_fate, cam_fate, fate, cfate)
  if(debug) nd %>% filter(nest %in% UHFnests) %>% select(nest, field_fate, cam_fate, fate, cfate)
  return(nd)
}

