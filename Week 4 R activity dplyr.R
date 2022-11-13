#Week 4: dplyr package

#Task: Write the function to get a dataset from Base R: HairEyeColor
#and give the dataset a new name of your choice
get_data <- function(){
  return(as.data.frame(HairEyeColor) )
}

data <- get_data()

#See the top rows of the data
#TASK: Write the function to see the top rows of the data

print_top_rows <- function(df){
  print(head(df, 5))
}

print_top_rows(data)

#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

#install.packages("dplyr")
library('dplyr')

#Let's just see the hair and sex columns
#Task: Write the function to 'select' just the hair and sex columns 
#(hint: use the 'select' function)


select_cols <- function(df){
  return(df %>% select('Hair', 'Sex'))
}


#Let's name the dataset with just the two columns, Hair and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name
df <- select_cols(data)
print_top_rows(df)

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)

deselect_cols <- function(df){
  return(df %>% select(-'Sex'))
}

df <- deselect_cols(data)
print_top_rows(df)

#Let's rename a column name
#TASK: Write the function that renames 'sex' to 'gender'

rename_cols <- function(df){
  return(df %>% rename("Gender" = "Sex"))
}
df <- rename_cols(data)
print_top_rows(df)


#Let's make a new dataset with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

duplicate_gender <- function(df){
  df <- df %>% 
    mutate(Gender = Sex)
}
df <- duplicate_gender(data)
print_top_rows(df)

#Let's 'filter' just the females from our dataset
#TASK: Write the function that includes only rows that are 'female'

filter_female <- function(df){
  return(filter(df, Sex == 'Female'))
}
female_df <- filter_female(data)
print_top_rows(female_df)

#Let's 'group' our data by gender
#TASK: Write the function to group the data by gender (hint: group_by)

groupby_sex <- function(df){
  df_g <- df %>% group_by(Sex) %>%
                summarise(Total_Freq = sum(Freq))
  return(df_g)
  
}
df <- groupby_sex(data)
print_top_rows(df)

#Let's see how many students were examined in the dataset (total the frequency)
#TASK: replace 'datasetname' with the name of your dataset and get the total
#TASK: After you run it, write the total here:592

total=mutate(data, total=cumsum(Freq))
total


#Since we have a females dataset, let's make a males dataset
#TASK: Write the function that includes only rows that are 'male'

filter_male <- function(df){
  return(filter(df, Sex == 'Male'))
}
male_df <- filter_male(data)
print_top_rows(male_df)


#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

join_dfs <- function(male, female){
  bind_rows(male, female)
}
df <- join_dfs(male_df, female_df)
print_top_rows(df)

#Optional Task: add any of the other functions 
#you learned about from the dplyr package

