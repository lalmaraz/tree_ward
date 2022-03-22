library(tidyverse)
setwd("~/Desktop/Trees")
trees <- read.csv("trees.csv")
wards <- read.csv('wards.csv', stringsAsFactors = FALSE)

# keep key variables for trees data
trees_clean <- trees %>%
  select (12, 13, 14) %>% 
  rename (ward = 1,
          common_name = 2,
          trunk_diam = 3)

# remove first column, columns that contain "Total", and subsections of ethnic origin
wards_clean <- subset(wards, select = -c(1))
wards_clean <- wards_clean[!grepl("Total", wards_clean$Description),]
wards_clean <- wards_clean[-c(37:48), ]

# set aside toronto column
toronto <- select(wards_clean, 1:2)

# skippy skip toronto column
wards_clean <- select(wards_clean, !2,)

# reset index
rownames(wards_clean)<-NULL

# separate rows by category
immigration <- wards_clean[2:3,]
birth_place <- wards_clean[4:8,]
mother_tongue <- wards_clean[9:10,]
home_language <- wards_clean[11:16,]
language_knowledge <- wards_clean[17:20,]
education <- wards_clean[21:23,]
ethnic_orgin <- wards_clean[24:35,]
visible_minority <- wards_clean[36:37,]

# find highest value per category
get_max_row <- function(column, names) {
  x <- which.max(column)
  names[x]
}

# friendly function to create rows from the get_max_row function output
make_row <- function(data){
  sapply(data[,-1], get_max_row, data[,1])
}

# create rows for categorical variables
immigration_row <- make_row(immigration)
birth_place_row <- make_row(birth_place)
mother_tongue_row <- make_row(mother_tongue)
home_language_row <- make_row(home_language)
language_knowledge_row <- make_row(language_knowledge)
education_row <- make_row(education)
ethnic_orgin_row <- make_row(ethnic_orgin)
visible_minority_row <- make_row(visible_minority)

# create ward demographic dataframe
ward_demographics <- data.frame(
                 ward = c(1:25),
                 age = c(as.double(wards_clean[1,-1])),
                 immigration = c(immigration_row),
                 birth_place = c(birth_place_row),
                 mother_tongue = c(mother_tongue_row),
                 home_language = c(home_language_row),
                 language_knowledge = c(language_knowledge_row),
                 education = c(education_row),
                 ethnic_origin = c(ethnic_orgin_row),
                 visible_minority = c(visible_minority_row),
                 income_avg = c(as.double(wards_clean[38,-1])),
                 income_med = c(as.double(wards_clean[39,-1]))
                 )

# reset index
rownames(ward_demographics)<-NULL

# merge tree and ward demographics data
tree_ward_merge = merge(x=trees_clean,y=ward_demographics,by="ward",all=TRUE)

# write new csv with merged data
write.csv(tree_ward_merge, "tree_ward_merge.csv", row.names = FALSE)
