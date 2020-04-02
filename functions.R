records <- function(ls){
  for (i in 1:length(ls)) {
    names(ls[[i]])[[1]] <- "Batting"
    names(ls[[i]])[[2]] <- "Bowling"
  }
  batting <- list()
  bowling <- list()
  
  for (xx in names(ls)) {
    #   batting[[xx]] <-  lapply(batting[[xx]], function(x)  as.character(x) )
    #   bowling[[xx]] <-  lapply(batting[[xx]], function(x)  as.character(x) )
    #   #batting$Full_name <- names(stat_table_odi)
    batting[[xx]] <- do.call(cbind.data.frame,ls[[xx]][["Batting"]])
    bowling[[xx]] <- do.call(cbind.data.frame,ls[[xx]][["Bowling"]])
    batting[[xx]] <-  lapply(batting[[xx]], function(x)  as.character(x) )
    bowling[[xx]] <-  lapply(bowling[[xx]], function(x)  as.character(x) )
  }
  
  
  Batting <- do.call(rbind.data.frame,batting)
  Batting$Full_Name <- unlist(lapply(rownames(Batting),function(x)unlist(strsplit(x,"[.]"))[1]))
  setcolorder(Batting,neworder = c(ncol(Batting),1:(ncol(Batting)-1)))
  
  Bowling <- do.call(rbind.data.frame,bowling)
  Bowling$Full_Name <- unlist(lapply(rownames(Bowling),function(x)unlist(strsplit(x,"[.]"))[1]))
  Bowling <- Bowling[!is.na(Bowling$Type),]
  setcolorder(Bowling,neworder = c(ncol(Bowling),1:(ncol(Bowling)-1)))
  
  return(list(as.data.table(Batting),as.data.table(Bowling)))
} 