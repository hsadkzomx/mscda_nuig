#Assignment 2 - CT5102
#Chin Zhe Jing 22221907

conv_df_to_list <- function(dataframe) {
  colnames <- names(dataframe)
  outer_result <- list()
  for (i in 1:nrow(dataframe)){
    inner_list <- list()
    for (name in colnames){
      inner_list[[name]] <- dataframe[[name]][i]
    }
    outer_result[[paste0("R-", i)]] <- inner_list
  }
  outer_result
}

c_1 <- conv_df_to_list(Formaldehyde)
str(c_1)

c_2 <- conv_df_to_list(mtcars1)
str(c_2)