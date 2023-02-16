df <- read.csv("../data/rba-dataset.csv", nrows = 500000)
df$label <- df$Is.Account.Takeover
df$Is.Account.Takeover <- NULL
df$label <- as.factor(df$label)
df$index <- NULL
df$Login.Timestamp <- NULL
df$User.ID <-NULL
df$IP.Address <- NULL
df$City <- NULL
df$Region <- NULL
df$Round.Trip.Time..ms. <- NULL
df$User.Agent.String <- NULL

data_partitioned <- initial_split(df, prop = 0.75, strata = label)
train <- training(data_partitioned)
test <-  testing(data_partitioned)
tree_template <- decision_tree() %>% 
  set_engine("rpart") %>%
  set_mode("classification")

tree_model <- tree_template %>% 
  fit(formula = label ~ ., data =  train)

fancyRpartPlot(tree_model$fit, caption = "First Decision Tree Attempt")