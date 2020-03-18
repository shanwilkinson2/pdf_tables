# extract a table from a pdf
# https://www.bolton.gov.uk/downloads/file/2521/hmo-public-register

library(tabulizer)
library(purrr)
library(janitor)
library(forcats)
library(dplyr)

# read in pdf - imports as a list
  # each element is a matrix representation of a page of the pdf table.
  hmo <- extract_tables("https://www.bolton.gov.uk/downloads/file/2521/hmo-public-register")
# some have diff number of columns  
  # in this case, column 6 is extra & empty 
  numcols <- map_dbl(hmo, ncol)
  not10cols <- which(numcols != 10)
  
  # read in pdf - imports as a list
  # each element is a matrix representation of a page of the pdf table.
    hmo <- extract_tables("https://www.bolton.gov.uk/downloads/file/2521/hmo-public-register")
  
  # convert to dataframe so can remove columns.
    # stringsAsFactors= FALSE means can get the first row out as text rather than numeric value for factor
  hmo_reduced <- map(hmo, as.data.frame, stringsAsFactors = FALSE)  
  
  # remove column 6 where number of columns isn't 10, as this is the empty column
  for(i in 1:length(hmo_reduced)){
    if(ncol(hmo_reduced[i][[1]]) != 10) {
      message(paste("not 10, element", i))
      hmo_reduced[i][[1]] <- hmo_reduced[i][[1]][,-6]
      names(hmo_reduced[i][[1]]) <- paste0("V", 1:10)
    }
  }

  # row bind each element of the list together 
    hmo_reduced <- do.call(rbind, hmo_reduced)

  # remove first row which is just a title
    hmo_reduced <- hmo_reduced[-1,]
  # get names from first row -- this is pulling out numbers ?factor level number?
    names(hmo_reduced) <- as.character(hmo_reduced[1,])
  # remove first row which is the column names
    hmo_reduced <- hmo_reduced[-1,] %>%
      # clean up col names
      clean_names() %>%
      mutate(licence_number = as.numeric(licence_number))
    
   # count unique values
    # NA is a unique value so minus 1 from the total
    length(unique(hmo_reduced$licence_number))-1
  
 