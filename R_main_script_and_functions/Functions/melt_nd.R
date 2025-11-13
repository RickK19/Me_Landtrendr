

#' Title: Melting dataframe with  multiple variable columns
#'
#' @param data      : <data.frame> dataframe to be melted
#' @param id.vars   : <character> character vector of the columns names to be used a id variables (kept in the melted dataframe)
#' @param measure.vars : <list> list of character vectors to be uses as variables. Each list vector will be sued as a variable 
#'                       if named, variable column wil be named with concatenation of "variable" with the name
#' @param value.names : <character> character vector of new variable and values

melt_nd <- function(data, id.vars, measure.vars,  value.names = NULL)
{
  require(reshape)
  # Check columns occurence in the dataframe
  columns      <- c(id.vars, unlist(measure.vars))
  checkColumns <- columns %in% colnames(data)
  
  if(!all(checkColumns))
  {
    stop("Fields not found in the dataframe: ", paste(columns[!checkColumns], collapse = ", "), ".")
  }else
  {
    # If measure.vars unamed: name it with value names (if provided)
    if(!is.null(names(measure.vars)))
    {
      warning("Arg. <value.names> has been updated with <measure.vars> names.")
      value.names <- names(measure.vars)
    }else{
      
      if(!is.null(value.names) && length(measure.vars)==length(value.names))
      {
        names(measure.vars) <- value.names
      }else{
        
        value.names <- paste0("V", 1:length(measure.vars))
        names(measure.vars) <- value.names
      }
    }
  }
  
  
  
  ############ Melting process
  df  <- as.data.frame(data[, columns])
  df.j <- NULL
  for(i in 1:length(measure.vars))
  {
    columns.i <- c(id.vars, measure.vars[[i]])
    df.i <- reshape::melt.data.frame(data = df[, columns.i], 
                                     measure.vars = measure.vars[[i]])
    df.i$variable <- as.character.factor(df.i$variable)
    df.i$id       <- 1:nrow(df.i)
    
    colnames(df.i)[which(colnames(df.i)=="value")]    <- value.names[i]
    colnames(df.i)[which(colnames(df.i)=="variable")] <- paste0("variable.", value.names[i])
    
    # Merging data with other created dataframe 
    if(i==1)
    {
      df.j <- df.i
    }else
    {
      df.j <- merge.data.frame(x=df.j, y=df.i, by=c(id.vars, "id"), all.x = T)
    }
  }
  return(df.j)
}
