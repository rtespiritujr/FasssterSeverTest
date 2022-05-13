get_linelist <- function(){
  files_dir <- 'linelist/non-imputed/'

  data.l <- list()
  files.l <- list.files(files_dir,pattern = "*.rds")
  
  for (i in seq_along(files.l)){
    data.l[[i]] <- readRDS(paste(files_dir,files.l[i],sep=""))
  }
  
  data.df <- data.frame()
  for (i in seq_along(data.l)){
    data.df <- rbind(data.df, data.l[[i]])
  }
  
  return(data.df)
}