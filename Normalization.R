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

conflict_prefer('select','dplyr','MASS')
conflict_prefer('filter','dplyr','MASS')


#Load NDG table:
NDG_table <- read_excel("DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "NDG")
NDG_table <- NDG_table %>% rename(Region=`Borrower's Region`, Tax_ID = 'Tax ID', Name = 'BorrowerName')
NDG_table$Group <- NDG_table$Group %>% gsub('-',NA,.)


#Remove multivalued cells, using pivot_longer function and create Borrowers_table:
separated_table <- NDG_table %>% 
      separate(Name,c('BorrowerName1','BorrowerName2','BorrowerName3','BorrowerName4'),sep = '[-,,]') %>% 
      separate(`Tax_ID`,c('TaxID1','TaxID2','TaxID3'),sep = '[-,,]')
Borrowers_table <-  separated_table %>% 
      pivot_longer(cols = matches('BorrowerN|Tax'),
                   names_to = c(".value", "taxID"),
                   names_pattern = "(BorrowerName|TaxID)(.*)",values_drop_na =  TRUE)

Borrowers_table$TaxID <- Borrowers_table$TaxID %>% str_trim(.,'left')
Borrowers_table$BorrowerName <- Borrowers_table$BorrowerName %>% str_trim(.,'left')
Borrowers_table <- Borrowers_table %>% 
                   select(- taxID)  %>%
                   rename(Name = BorrowerName, Tax_ID = TaxID)
Borrowers_table <- Borrowers_table %>% mutate(Borrower_ID = sprintf('%03d',199 + row_number()) )


#create Address_table_final:
Address_table <- NDG_table %>% select(Address, Town, City, Region)
Address_table <- Address_table %>%  mutate(Address_ID = sprintf('%03d',399 + row_number()) )
Address_table_final <- Address_table %>% select(Address_ID,Address, Town, City, Region )


# join Borrowers_table and Address_table_final:
Borrowers_table <- Borrowers_table %>% select(-Town,-City,-Region)
Borrowers_table_1 <- left_join(Borrowers_table,Address_table_final, by = ('Address'='Address'), copy=FALSE) %>% 
                     select(-Address,-City,-Town,-Region)


#Final version of the Borrowers_table and NDG_table:
Borrowers_table_final <- Borrowers_table_1 %>% select(-Category) %>% 
                         relocate(Group, .after = Address_ID) %>% 
                         relocate(Borrower_ID, .before = NDG) %>% 
                         relocate(NDG, .after = Group)

NDG_table_final <- Borrowers_table_1 %>% select(NDG,Category) %>% distinct()


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

possible_keys <- detect_primary_keys(Borrowers_table)
print(possible_keys)


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

possible_keys_90 <- detect_primary_keys_90(Borrowers_table_final)
print(possible_keys_90)


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

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Borrowers_table_final,number_NAs=1,percentage=0.9)
print(possible_keys_NAs_perc)


# Load the Loans_table:
Loans <- read_excel("DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "LOANS")
Loans_table <- Loans[-1, ] %>% row_to_names(1)


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


# modify all the column names
names(Loans_table) <- gsub('[.,/, ]', '_', names(Loans_table))
colnames(Loans_table) <- sapply(colnames(Loans_table), capitalize_after_underscore)
names(Loans_table)
Loans_table <- Loans_table %>% rename(NDG = 'Ndg',Loans_ID = 'Id_Loans',
                                      Borrower_Name = 'Borrowername', Secured_Unsecured_NDG = 'Sec__Unsec_X_Ndg')
names(Loans_table) <- names(Loans_table) %>% gsub('Gbv','GBV',.)
colnames(Loans_table) <- sub("(_\\()?_Yes_No(\\))?", "", colnames(Loans_table))
colnames(Loans_table) <- sub("(_\\()yes_No(\\))?", "", colnames(Loans_table))
colnames(Loans_table) <- gsub("_+", "_", colnames(Loans_table))


#the dates are excel numbers, recuperate the real date and create the UTP column: 
Loans_table$Database_Date <- excel_numeric_to_date(as.numeric(Loans_table$Database_Date))
UTP <- Loans_table$Default_Date %>% as.character()
Loans_table$Default_Date <- excel_numeric_to_date(as.numeric(Loans_table$Default_Date))
Loans_table <- cbind(UTP,Loans_table)
Loans_table$UTP <- if_else(Loans_table$UTP =='UTP','UTP','Bad')


#possible keys for the Loans_table:
possible_keys_loans <- detect_primary_keys(Loans_table)
print(possible_keys_loans)

possible_keys_90 <- detect_primary_keys_90(Loans_table)
print(possible_keys_90)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Loans_table,number_NAs=1,percentage=0.4)
print(possible_keys_NAs_perc)







#################################################################################

######################################################
#creare una funzione che controlla le function dependencies con le chiavi
#nella tabelle LOnans le chiavi sono: primaria - ID_Loans, esterna - NDG


keys <- c('NDG', 'ID_Loans')
num <- 0.8
check_dependencies <- function(Loans_table,num) {
  columns_to_check <- Loans_table %>% select(-NDG,-Loans_ID)
  lista_100 <- c()
  lista_80 <- c()
  for(col in names(columns_to_check)){
    grouped_data <- split(Loans_table[[col]],Loans_table$NDG, drop =TRUE)
    dependency_check <- sapply(grouped_data, function(group) length(unique(group))==1 )
    
    if( all(dependency_check== TRUE) ){
      lista_100 <-c(lista_100, c(col))
    } 
    if(sum(dependency_check)/length(dependency_check) >=num & sum(dependency_check)/length(dependency_check) < 1 ){
      lista_80 <- c(lista_80, c(col,sum(dependency_check)/length(dependency_check)*100))
    }
  }
  result <- list(lista_100,lista_80)
  return(result)
}
x <- check_dependencies(Loans_table,num)
print('Queste colonne dipendono da NDG: ')
print(x[1])
print('Queste colonne dipendono al 80% da NDG: ')
uneven_indices <- seq(from = 1, to = length(x[[2]]), by = 2)
for (i in uneven_indices) {
  value <- as.numeric(x[[2]][[i+1]])
  formatted_ratio <- paste0(format(round(value, 2), nsmall = 2), "%")
  cat("Column Name: ", x[[2]][[i]], "  Ratio: ", formatted_ratio, "\n")
}





##################################################################################
#stessa funzione ma togliamo 'NA'
library(rlang) 

check_dependencies_NA <- function(Loans_table,num) {
  columns_to_check <- Loans_table %>% select(-NDG,-ID_Loans)
  lista_100_NA <- c()
  lista_80_NA <- c()
  for(col in names(columns_to_check)){
    tabella_prova <- Loans_table[!is.na(Loans_table[[col]]), ]
    grouped_data <- split(tabella_prova[[col]],tabella_prova$NDG, drop =TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group))==1 )
    
    if( all(dependency_check== TRUE) ){
      lista_100_NA <-c(lista_100_NA, c(col))
    } 
    if(sum(dependency_check)/length(dependency_check) >=num & sum(dependency_check)/length(dependency_check) < 1 ){
      lista_80_NA <- c(lista_80_NA, c(col,sum(dependency_check)/length(dependency_check)*100))
    }
  }
  result <- list(lista_100_NA,lista_80_NA)
  return(result)
}
x_NA <- check_dependencies_NA(Loans_table,num)
print('Queste colonne dipendono da NDG: ')
print(x_NA[1])
print('Queste colonne dipendono al 80% da NDG: ')
uneven_indices <- seq(from = 1, to = length(x_NA[[2]]), by = 2)
for (i in uneven_indices) {
  value <- as.numeric(x_NA[[2]][[i+1]])
  formatted_ratio <- paste0(format(round(value, 2), nsmall = 2), "%")
  cat("Column Name: ", x_NA[[2]][[i]], "  Ratio: ", formatted_ratio, "\n")
}



#tabella_prova <- Loans_table[!is.na(Loans_table[["Default_Date"]]), ]
#tabella_prova <- Loans_table[!is.na(Loans_table[["Type_of_Mortgage"]]), ]
