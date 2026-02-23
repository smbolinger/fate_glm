
#Are there any extra columns left over that don't have observations?

#I needed to remove the last six columns, so I simply put their indices
#into the code below to be extra sure I was getting the correct columns:

```{r eval=FALSE, include=FALSE}
# cols <- c(125:130)        # indices of columns for removal
#cols <- c(133, 134, 136, 137)        # indices of columns for removal
cols <- c()
names(nestdata[cols])
```

If it looks correct, remove the columns. Then check the names to see
what columns are left.

```{r eval=FALSE, include=FALSE}
nestdata <- nestdata[,-cols] 
names(nestdata)   # are you tired of looking at column names yet??
```
