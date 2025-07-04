#Libraries
library(dplyr)
library(stringr)
library(readr)

foodcoded <- read.csv('food_coded.csv')

glimpse(foodcoded)

#Nulls
anyNA(foodcoded)
colSums(is.na(foodcoded))

#Duplicated
sum(duplicated(foodcoded))

summary(foodcoded)

#Data cleaning

#Replace text with NaN in the 'weight' column
foodcoded <- foodcoded %>%
  mutate(weight = as.character(weight),
         weight = na_if(weight, "I'm not answering this. "))

#unique(foodcoded$weight)

#Convertir todos los "nan", "NaN", "NA", etc. (como texto) en NA real
foodcoded <- foodcoded %>%
  mutate(across(
    everything(),
    ~ ifelse(str_to_lower(as.character(.)) %in% c("nan", "na", "n/a", "null", ""), NA, .)
  ))


#Extract only the numeric values from the 'weight' and 'GPA' columns and create new columns from them
foodcoded <- foodcoded %>%
  mutate(
    weight_clean = str_extract(weight, "\\d+\\.?\\d*"),
    GPA_clean = str_extract(GPA, "\\d+\\.?\\d*")
  )


#Convert 'GPA_clean' and 'weight_clean' columns to float type
foodcoded <- foodcoded %>%
  mutate(across(c(GPA_clean, weight_clean), as.numeric))


#Move 'GPA_clean' column to the front of the DataFrame
foodcoded <- foodcoded %>% relocate(GPA_clean)


#Drop unnecessary or redundant columns from the DataFrame
foodcoded <- foodcoded %>%
  select(-comfort_food_reasons_coded, -eating_changes_coded, -GPA, -weight)


#Rename cleaned columns to replace original names
foodcoded <- foodcoded %>% 
  rename(
    GPA = GPA_clean,
    weight = weight_clean
  )

#Select numeric columns and convert them to float type
foodcoded <- foodcoded %>%
  mutate(across(where(is.numeric), as.numeric))


#Convert all text columns to lowercase
foodcoded <- foodcoded %>%
  mutate(across(where(is.character), ~ str_to_lower(.)))


#Fill missing values in all columns with the mode of each column

# Función para obtener la moda
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Reemplazar NA con la moda en cada columna
foodcoded <- foodcoded %>%
  mutate(across(everything(), ~ ifelse(is.na(.), get_mode(.), .)))

#Save the cleaned DataFrame
write_csv(foodcoded, "D:/Curso R/PARTE 3/foodcoded_limpio.csv")

sapply(foodcoded, function(x) sum(is.na(x)))

anyNA(foodcoded)
colSums(is.na(foodcoded))
