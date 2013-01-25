agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {
    stop("NULL age")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is given
  r <- regexec("([0-9]+) (years old|year old)", homicides)
  m <- regmatches(homicides, r)
  ages <- sapply(m, function(x) x[2])
  
  ## Return integer containing count of homicides for that age
  length(grep(sprintf("^%d$",age), ages))
}
