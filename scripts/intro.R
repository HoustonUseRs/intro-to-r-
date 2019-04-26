# In the console
setwd("C:/Users/Kenia.Way/OneDrive - IHS Markit/Documents/workshop/intro-to-r-")
read.csv

read.csv()

?read.csv

read.csv(file = 'got.csv')

got <- read.csv(file = 'got.csv')

# Glancing at the data
str(got) # Structure of data                   
summary(got) #Summary of the data                 
View(got) # View data in file viewer

# Remove factors
got <- read.csv(file = 'got.csv', stringsAsFactors = FALSE)


names(got) # Column header names                 
dim(got) # Number of rows by number of columns 
nrow(got) # Number of rows                      
ncol(got) # Number of columns                   
str(got) # Structure of data                   


# packages
packageDescription("dplyr")
help("dplyr")

# We can also look at parts of the data using _verbs_ like `filter()` and `select()`, 
select(got, names)

??select

# ummm... it looks like we need to install the `dplyr` package.
# install.packages('dplyr')
installed.packages()

library(dplyr)
data(package = "dplyr")

# Exploring the data 
select(got, name)
select(got, name, isAlive)

filter(got, popularity > 0.4)
filter(got, male == 0)

mean(unlist(select(got, popularity)))

# Introducing %>% 
got %>% select(popularity) %>% unlist() %>% median()
got %>% select(popularity) %>% unlist() %>% sd()
got %>% select(popularity) %>% unlist() %>% max()
got %>% select(popularity) %>% unlist() %>% min()
got %>% select(title, popularity, contains("is"), male) %>% 
  filter(male==0) %>% arrange(desc(popularity))

# Agg data
got %>% group_by(isAlive, male) %>% summarise(mean= mean(popularity)) %>% arrange(desc(mean))
got %>% group_by(isAlive, male) %>% summarise(mean= mean(popularity),
                                              N= length(popularity)) %>% arrange(desc(mean))

# Visualizing
boxplot(got$popularity)

boxplot(got$popularity,
        main = "Popularity of GOT characters",
        xlab = "0-1",
        ylab = "Popularity",
        col = "orange",
        border = "brown",
        horizontal = TRUE)

# adding more comprehensive names
got <- got %>% mutate(gender = ifelse(male == 1, "men", "women"))
boxplot(popularity ~ gender, data=got,
        main = "Popularity of GOT characters by gender",
        xlab = "Popularity",
        ylab = "Gender",
        col = c("blue", "pink"),
        horizontal = TRUE)

gotFiltered <- got %>% filter(house %in% c("Lannister", "Stark", "Targaryen"))
boxplot(popularity ~ house, data=gotFiltered,
        main = "Popularity of GOT characters by house",
        xlab = "Popularity",
        ylab = "House",
        col = c("blue", "pink", "magenta"))


# Fav houses
gotFiltered <- got %>% filter(house %in% c("Lannister", "Stark", "Targaryen"))
boxplot(mean~gender, data=got_m, 
        col=(c("gold","darkgreen")),
        main="popularity vs gender", ylab="mean")
