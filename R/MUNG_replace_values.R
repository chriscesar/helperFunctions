# replace_values.R
# use to replace numerical values that are less than the limit of detection (LOD) in a data
# set that are flagged with "<" (e.g. "<0.01")

replace_values <- function(x) { #function to replace "less than" values by  half their value
  if_else(str_detect(x, "^<"), as.numeric(sub("^<", "", x))/2, as.numeric(x))
}

## use like:
#df %>% 
#	### if a cell contains "<", then replace that value with (x/2)
#	### E.g. "<0.2" becomes 0.1
#	mutate_at(vars(VariableAA:VariableXX), replace_values)