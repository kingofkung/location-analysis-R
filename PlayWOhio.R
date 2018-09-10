rm(list = ls(all.names = TRUE))

## Assuming that I've guessed correctly, then I'm going to see about
## creating a file from the ohio voter file I downloaded from the url
## below.
## https://www6.sos.state.oh.us/ords/f?p=VOTERFTP:STWD:::#stwdVtrFiles
library(data.table)

setwd("~/Downloads")

dt1 <- fread("SWVF_1_22.txt", data.table = F)
head(dt1)
## lower case column names
colnames(dt1) <- tolower(colnames(dt1))

## and camel case on those underscores
## improved from the original at the location below:
## https://stackoverflow.com/questions/11672050
camel <- function(x){ #function for camel case
    ## function that capitalizes the first character
    capit <- function(x) paste0(toupper(substr(x, 1, 1)),
                                substring(x, 2, nchar(x)))
    ## split the data by its underscores and capitalize the results
    out <- vapply(strsplit(x, "\\_"),
                  function(x) paste(capit(x), collapse=""), character(1))
    ## and return the results of out, having made its first character lower case
    paste0(tolower(substr(out, 1, 1)), substr(out, 2, nchar(out)))
}
colnames(dt1) <- camel(colnames(dt1))



## data are here...  looking at it, we'll need only some of it...
dt1 <- dt1[, !grepl("primary|special|general", colnames(dt1))]

## There are 2 addresses, mailing and residential... Which one do we
## want? Going to pick residential, I guess




