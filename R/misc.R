#' Write tsv file
#'
#' @param x The object to be written, preferably a matrix or data frame. If not, it is attempted to
#' coerce x to a data frame.
#' @param file Path to the output file. If file ends with .gz, a gzip compressed file will be written
#' @param ... Arguments that can be passed to write.table()
#'
#' @return
#' @export
#'
write.tsv <- function(x, file, ...){
   write.table(
      x,
      if(grepl('[.]gz$',file)){ gzfile(file) } else { file },
      sep='\t', quote=F, row.names=F,
      ...
   )
}

####################################################################################################
#' Insert a row into a dataframe
#'
#' @param df A dataframe
#' @param new.row The row as a vector or dataframe to insert
#' @param row.num Row index to insert the row. Rows below this are shifted down.
#' @param offset Insert the row at +offset rows after row.num
#'
#' @return A dataframe
#' @export
#'
insertRow <- function(df, row.num, new.row=NULL, offset=1) {
   df[seq(row.num+1,nrow(df)+1),] <- df[seq(row.num,nrow(df)),]

   if(is.null(new.row)){
      new.row <- df[row.num,]
   }

   df[row.num+offset,] <- new.row

   return(df)
}

####################################################################################################
#' Insert column(s) after a column of an existing dataframe
#'
#' @param df Input dataframe
#' @param v A vector or dataframe
#' @param after Column index or name
#' @param colname Set inserted column name. Has no effect if v is not a vector
#'
#' @return A dataframe with the inserted column(s)
#' @export
#'
insColAfter <- function(df, v, after, colname=NULL){
   if(is.character(after)){
      after <- which(colnames(df)==after)
   }

   df_l <- df[1:after]
   df_r <- df[(after+1):ncol(df)]

   df_new <- cbind(df_l, v, df_r)

   if(!is.null(colname)){
      colnames(df_new)[(after+1)] <- colname
   }

   return(df_new)
}
