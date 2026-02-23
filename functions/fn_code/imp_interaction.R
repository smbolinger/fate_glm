
# To deal with interactions (which should be included in the imputation model), use 
# transform-then-impute (von Hippel 2009) - BUT this method may require MCAR?
# 
# Explicitly compute the interaction term by creating a variable that equals:
#   dummy species variable(1=LETE, 0=CONI) times the other variable in the interaction
# 
# Then drop species from the dataset
# 
# FOr this to work, need to rewrite the models in terms of the new interaction variable.

# ```{r, warning=FALSE}
# change CI comma to a dash - now this is in the main script at the beginning
# from https://stackoverflow.com/questions/75637034/insert-dash-in-confidence-interval-instead-of-comma-in-r-gtsummary
# my_theme <-
#   list(
#     "pkgwide-str:ci.sep" = " - "
#   )
# 
# set_gtsummary_theme(my_theme)
# ```
