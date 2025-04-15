# prepro-ppt.R
# Matt Kmiecik
# Started 14 April 2025

# Purpose: preprocess the PPT data from raw Wagner Alometer files

# functions ----
source("src/fns/extract_ppt.R") # for processing regular ppt files
source("src/fns/extract_ppt_log.R") # for processing log files

# proc ----

## determines which file to cycle through
base_dir <- "data/PPT_only"

## ppt files
ppt_files <- 
  list.files(
    base_dir, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE
    )

tlen <- 200
tmp1 <- vector("list", length = tlen) -> tmp2 # preallocates lists
names(tmp1) <- ppt_files[1:tlen] # names lists by file path
names(tmp2) <- ppt_files[1:tlen]

# loops through files and processes them
pb <- txtProgressBar(min = 0, max = length(tmp1), style = 3) # prog bar
for (i in 1:length(tmp1)) {
  
  # tmp1 stores the regular ppt files
  tmp1[[i]] <- 
    tryCatch(
      {
        extract_ppt(file = ppt_files[i]) # attempts a pull on ppt data
      },
      error = function(e){
        message(sprintf("Error on i = %d: %s", i, e$message))
        return(NA)  # or NULL or whatever fallback makes sense
      }
    )
  
  # tmp2 stores the ppt log files
  tmp2[[i]] <- 
    tryCatch(
      {
        extract_ppt_log(file = ppt_files[i]) # attempts a pull on ppt data
      },
      error = function(e){
        message(sprintf("Error on i = %d: %s", i, e$message))
        return(NA)  # or NULL or whatever fallback makes sense
      }
    )
  
  # Update progress bar
  setTxtProgressBar(pb, i)
  
}; close(pb)


# consolidates ppt files:
ppt_res <- Filter(is.data.frame, tmp1) %>% list_rbind(names_to = "path")
ppt_log_res <- Filter(is.data.frame, tmp2) %>% list_rbind(names_to = "path")
