library(DT)
library(tidymodels)
library("ggfortify")
library(rattle)
library(stringr)
library(data.table)


create_df <- function(dir = "/home/maple/CodeProjects/r-dask-pyspark/data/", rows = Inf){
  files <- list.files(dir)
  df <- NULL
  for (i in 1:length(files)){
    # first verify is a csv
    if (str_detect(files[i], ".csv")){
      full_name <- paste(dir, files[i], sep ="")
      print(full_name)
      if (is.null(df)){
        df <- fread(full_name, nrows = rows)
      } else {
        df_temp <- fread(full_name, nrows = rows)
        df_temp <- df_temp %>% select(any_of(colnames(df)))
        df <- rbind(df, df_temp)
      }
    }
  }
  return(df)
}

df <- create_df(rows = 100000)
str(df)

df$Timestamp <- NULL
df$Label <- df$Label == "Benign"
table(df$Label)
df$Label <- as.factor(df$Label)


data_partitioned <- initial_split(df, prop = 0.75, strata = Label)
train <- training(data_partitioned)
test <-  testing(data_partitioned)
tree_template <- decision_tree() %>% 
  set_engine("rpart") %>% # set the engine to spark for big analysis
  set_mode("classification")

tree_model <- tree_template %>% 
  fit(formula = Label ~ ., data =  train)

fancyRpartPlot(tree_model$fit, caption = "First Decision Tree Attempt")