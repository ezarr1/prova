install.packages("xlsx")
install.packages("str2str")
install.packages("conflicted")

library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(str2str)
library(conflicted)
library(rlang) 

conflict_prefer('select','dplyr','MASS')
conflict_prefer('filter','dplyr','MASS')

#create a function that finds possible keys of a table:
detect_primary_keys <- function(table) {
  possible_keys <- c()
  for (col in names(table)) {
    if (length(unique(table[[col]])) == nrow(table) && !any(is.na(table[[col]]))) {
      possible_keys <- c(possible_keys, col)
    }
  }
  return(possible_keys)
}


#modify the function so that if a column has 0-1 NA and/or >=90% uniqueness it is a possible key:
detect_primary_keys_90 <- function(table) {
  possible_keys_90 <- c()
  for (col in names(table)) {
    if (!any(is.na(table[[col]])) | sum(is.na(table[[col]]))==1) {
      if(length(unique(table[[col]]))/nrow(table) >= 0.9){
        possible_keys_90 <- c(possible_keys_90, col)
      }
    }
  }
  return(possible_keys_90)
}


#try to further modify it with the number of NAs and of percentage:
detect_primary_keys_NAs_perc <- function(table,number_NAs,percentage) {
  possible_keys_NAs_perc <- c()
  for (col in names(table)) {
    if (!any(is.na(table[[col]])) | sum(is.na(table[[col]]))==number_NAs) {
      if(length(unique(table[[col]]))/nrow(table) >= percentage){
        possible_keys_NAs_perc <- c(possible_keys_NAs_perc, col)
      }
    }
  }
  return(possible_keys_NAs_perc)
}

# create a function that capitalize the first letter and the letter after _ and the rest lowercase:
capitalize_after_underscore <- function(x) {
  parts <- unlist(strsplit(x, "_"))
  formatted_parts <- sapply(parts, function(part) {
    if (nchar(part) > 0) {
      paste0(toupper(substring(part, 1, 1)), tolower(substring(part, 2)))
    } else {
      ""
    }
  })
  return(paste(formatted_parts, collapse = "_"))
}

#check the functional dependencies:
check_dependencies<- function(table, columns_to_check, num, key) {
  lista_100 <- list()
  lista_num <- list()
  for (col in columns_to_check) {
    grouped_data <- split(table[[col]], table[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)) == 1)
    
    if (all(dependency_check == TRUE)) {
      lista_100[[col]] <- 100
    } 
    if (sum(dependency_check) / length(dependency_check) >= num & sum(dependency_check) / length(dependency_check) < 1) {
      lista_num[[col]] <- sum(dependency_check) / length(dependency_check) * 100
    }
  }
  
  result <- list(lista_100, lista_num)
  return(result)
}



#check dependencies deleting NAs:
check_dependencies_NA <- function(table, columns_to_check, num, key) {
  lista_100_NA <- list()
  lista_num_NA <- list()
  for (col in columns_to_check) {
    tabella_prova <- table[!is.na(table[[col]]), ]
    grouped_data <- split(tabella_prova[[col]], tabella_prova[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)) == 1)
    
    if (all(dependency_check == TRUE)) {
      lista_100_NA[[col]] <- 100
    } 
    if (sum(dependency_check) / length(dependency_check) >= num & sum(dependency_check) / length(dependency_check) < 1) {
      lista_num_NA[[col]] <- sum(dependency_check) / length(dependency_check) * 100
    }
  }
  
  result <- list(lista_100_NA, lista_num_NA)
  return(result)
}

#unifying the two functions in one:
check <- function(table, columns_to_check, num, key,con_o_senza_na){
  if(con_o_senza_na){
    return(check_dependencies(table, columns_to_check, num, key))
  } else {
    return(check_dependencies_NA(table, columns_to_check, num, key))
  }
}

print_lists <- function(lists,num){
  for(j in 1:length(lists)){
    for (col in names(lists[[j]])){
      value <- as.numeric(lists[[j]][[col]])
      formatted_ratio <- paste0(format(round(value, 2), nsmall = 2), "%")
      cat("Column Name: ", col, "  Ratio: ", formatted_ratio, "\n")
    }
    cat("\n")
  }
}