count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("NULL cause")
  }
  
  ## Check that specific "cause" is allowed; else throw error
  allowedCauses <- c("asphyxiation", "blunt force", "other", "shooting",
                     "stabbing", "unknown" )
  if (length(grep(cause, allowedCauses)) == 0) {
    stop("invalid cause")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  r <- regexec("<dd>[C|c]ause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  causes <- sapply(m, function(x) tolower(x[2]))
  
  ## Return integer containing count of homicides for that cause
  length(grep(cause, causes))
}