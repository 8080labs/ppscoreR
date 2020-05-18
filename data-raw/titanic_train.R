## code to prepare `titanic_train` dataset goes here

# change respective columns to as.numeric type
titanic_train[c('PassengerId', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Fare')] <-
  lapply(titanic_train[c('PassengerId', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Fare')], as.numeric)

# change respective columns to as.character type
titanic_train[c('Name', 'Cabin', 'Ticket')] <-
  lapply(titanic_train[c('Name', 'Cabin', 'Ticket')], as.character)

usethis::use_data(titanic_train, overwrite = TRUE)
