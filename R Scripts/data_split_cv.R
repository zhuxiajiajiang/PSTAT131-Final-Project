library(tidyverse)
library(tidymodels)

df <- read.csv('data/world_population.csv')

colnames(df) <- c('Rank', 'CCA3', 'Country','Capital','Continent','Population_2022',
                  'Population_2020','Population_2015','Population_2010',
                  'Population_2000','Population_1990','Population_1980',
                  'Population_1970','Area', 'Density', 'Growth_Rate',
                  'World_Population_Percentage')

data <- df %>% select(c(Continent, Area, Population_1970, 
                        Population_1980, Population_1990, Population_2000,
                        Population_2010, Population_2015, Population_2020,
                        Population_2022))

pop_split <- initial_split(data, strata = Continent, prop = 0.7)
pop_split

pop_train <- training(pop_split)
pop_test <- testing(pop_split)
# 5 fold cross validation
pop_folds <- vfold_cv(pop_train, v = 5)

