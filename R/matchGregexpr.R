matchGregexpr = function(pattern, charv, ...) {
  require(plyr)
  if (storage.mode(charv) != "character") stop("please feed me a charactervector")
  
  allmatch = gregexpr(pattern, charv, ...)
  
  convertMatches = c()
  for (i in 1:length(allmatch)) {
    thisLine <- allmatch[[i]]
    if (thisLine[1] != -1) {
      convertMatches <- rbind(convertMatches, data.frame(element=i, start=thisLine, end=thisLine + attr(thisLine, "match.length") - 1))
    }
  }
  
    if (is.null(convertMatches)) return(NULL)
  
     convertMatches = adply(convertMatches, 1, function(row) {
        row$match = substr(charv[row$element], row$start, row$end)
        return(as.data.frame(row))
      })

  convertMatches$match <- as.character(convertMatches$match)
  return(convertMatches)
}