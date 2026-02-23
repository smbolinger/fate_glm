## Function to calculate season-day
sday <- function(date){ # converts dates to season-days starting at a given date

  date <- as.Date(
    lubridate::parse_date_time(date, c("%d-%b", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S"), exact = TRUE))
  
  # yday(date) - 103 # actual Julian date minus no. days from 1 Jan - 13 Apr
  yday(date) - 100 # actual Julian date minus no. days from 1 Jan - 10 Apr
  #yday(date) - 90 # actual Julian date minus no. days from 1 Jan - 1 Apr
  #yday(date) - 110 # actual Julian date minus no. days from 1 Jan - 20 Apr
} 



## function to import nest data from file and appropriately name the year column
## also adds prefix to nest numbers based on year
importNest <- function(yearnum, yrs){
  year <- yrs[yearnum]
  fname <- list.files(path=importDir, pattern=year, full.names=TRUE) |> max() # get most reecent file that matches year
  dat <- read_csv(fname, skip = 0, skip_empty_rows = TRUE,
                     col_types = cols(.default="c"))
  dat$year <- year
  dat$nest <- as.numeric(dat$nest) # should make non-numeric into NA which can then be reemoved
  # non-numeric nests should only be nests that weren't observed to final fate?
  # table(dat$nest, useNA = "ifany")
  dat <- dat[!is.na(dat$nest),] 
  #dat <- dat[dat$nest != "total hatch",]
  dat$nest <- dat$nest %>% str_pad(width=4, side="left", pad="0" )
  dat$nest <- paste0(yearnum, dat$nest)
  return(dat)
}

calcJDate <- function(colIndex, nestdata, debug=F){
  dates     <- names(nestdata[colIndex]) # select names of observation columns
  julDates  <- sday(dates)                # convert the names to season-days
  names(julDates)  <- dates               # also creates index of date:season-day
  if(debug) cat("\n>> julian dates:\n", julDates)
  jDates <- sort(julDates)
  if(debug) cat("\n>> julian dates, correct order:\n", jDates)
  # create a vector of all column names by combining the nest info columns (cols minus colIndex cols) and the observation columns (with the "j_" pasted in front)
  # newNames         <- c(names(nestdata[-colIndex]),paste0("j_", julDates))
  newNames         <- c(names(nestdata[-colIndex]),paste0("j__", julDates))
  if(debug) cat("\n>> new names:\n",newNames)
  names(nestdata) <- newNames            # apply the new column names! same column order
  newOrder <- c(names(nestdata[-colIndex]), paste0("j__",jDates))# add j to the desired col order
  
  if(debug) cat("\n>> new order of new names:\n",newOrder)
  if(debug) cat("\n>> initial columns:\n", names(nestdata))
  nestdata <- nestdata[newOrder] ## reorder the columns
  if(debug) cat("\n>> final columns:\n", names(nestdata))
  write.csv(nestdata, sprintf("output/nestdata_cols_%s.csv", now), row.names = F)
  return(nestdata)
}

uniqueStr <- function(colIndex, nestdata){
  # View all unique strings
  for(col in colIndex) { 
    # print(glue::glue("Unique values in {col} variable:", unique(nestdata[col])))  
    uV <- paste("Unique values in ",names(nestdata)[col], "variable:", unique(nestdata[col]))
    filename <- paste0("output/uniqueStr_perColumn_", now, ".txt")
    write(uV, file = filename, append=TRUE)  
    } 
    
  uniqueStr <- vector()          # vector to fill with unique strings
  for(col in colIndex) uniqueStr <- append(uniqueStr, unique(nestdata[col]))
  uniqueStr <- unlist(uniqueStr)
  uniqueUnique <- unique(uniqueStr)
  return(uniqueUnique)
}

replCols <- function(colIndex, nestdata, strings){
  if(file.exists("output")==FALSE) dir.create("output") 
  filepath1 <- paste0("output/col_repl_", now, ".txt")
  repCols <- list()
  notExtr <- list()
  for( col in colIndex ) {
                repCols[[col]] = str_extract( 
                  nestdata[[col]],
                  strings
                )
      oldCol <- nestdata[[col]]
      newCol <- repCols[[col]]
      ID     <- nestdata$nest
      Cols   <- cbind(ID, oldCol)
      Cols   <- as.data.frame(cbind(Cols, newCol))
      names(Cols) <- c("ID", "oldCol", "newCol")
      Cols   <- subset(Cols, !is.na(Cols$oldCol)) # non-empty column
      notExtr[[length(notExtr)+1]] <- c(col,":", which(is.na(Cols$newCol)))
      column <- paste("\n >> column: ", col, "\n")
      
      sink(filepath1, append=TRUE)
      cat(column)
      qvcalc::indentPrint(Cols, indent=20, row.names=FALSE)
      sink()
  }
  filename <- sprintf("output/not_extr_%s.txt", now)
  write("obs = NA after extract\n\ncol : row numbers\n", file=filename)
  for(n in 1:length(notExtr)) {
    # a <- unlist(notExtr[n])
    x <- unlist(notExtr[n])
    write(x, file=filename, append=TRUE, ncolumns = length(x))
  }
  
  for ( col in colIndex ) nestdata[[col]] = repCols[[col]] # replace old column with new
  return(nestdata)
}


obsHist <- function(x){ # x is the row
  x
  nest <- x$nest
  ind <- !is.na(x[colIndex])
  # ind
  obs <- x[colIndex][ind]
  history <- paste(nest, paste(obs, collapse=" | "), sep=":   " )
  return(history)
}

#We also check which status observations have an obs interval of more than 6 
# and which ones have no observation interval.
calcObsInt <- function(nestData, debug=F){
  nestData$obs_int   <- ifelse( nestData$same_ID== 0, subtractPrev(nestData$season_day), 0)
  # if any obs int is greater than 6, check to make sure we didn't miss obs
  # longObs <- unique(nestData$nest[which(nestData$obs_int > 6)]) # which nests?
  # longObs <- which(nestData$obs_int > 6)         # which rows?
  if(debug) cat("\n >> observation intervals:\n")
  if(debug) print(table(nestData$obs_int, useNA="ifany"))
  longObs <- unique(nestData$nest[nestData$obs_int > 6 ])        # which rows?
  if(debug) cat("\n >> nests w/long observation interval:\n\n", longObs)
  noObsInt <- unique(nestData$nest[is.na(nestData$obs_int)])
  if(debug) cat("\n\n >> nests w/o observation interval:\n\n", noObsInt)
  return(nestData)
}


init <- function(date, species, nest=NULL, debug=F) {
  inc = case_when( species == "WIPL" ~ 28,
                   species == "LETE" ~ 19,
                   species == "CONI" ~ 16 )
  # cat(sprintf("date: %s  & inc: %s", date, inc))
  #if(debug) cat(sprintf("final date (%s) - incubation time (%s)", date, inc))
  # if(debug & (date < inc)) cat(sprintf("nest %s date [%s] is less than incubation period[%s]", date, inc)) 
  # if(debug){
  #    if(date < inc)  cat(sprintf("nest %s date [%s] is less than incubation period[%s]", date, inc)) 
  #   }
  
  return(date-inc) 
}

lostNests <- function(nestData){
  
  # nestData$found <- 
  lostnest <- nestData$nest[which(nestData$lost == T)]
  lostN <- nestData %>% 
    #filter(lost == TRUE & season_day < k) %>% # only select those obs
    filter(nest %in% lostnest) %>%
    group_by(nest) %>%
    summarize(status = list(status),
              final_fate = last(final_fate),
              final_obs = max(season_day),
              k         = last(k))
  return(lostN)
  
}













