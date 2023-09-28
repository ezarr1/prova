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



possible_keys <- detect_primary_keys(Borrowers_table)
print(possible_keys)

possible_keys_90 <- detect_primary_keys_90(Borrowers_table_final)
print(possible_keys_90)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Borrowers_table_final,number_NAs=1,percentage=0.9)
print(possible_keys_NAs_perc)


# Load the Loans_table:
Loans <- read_excel("DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "LOANS")
Loans_table <- Loans[-1, ] %>% row_to_names(1)


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

# apply the function to the Loans_table:

column_to_eliminate <- 'Loans_ID'
key <- 'NDG'
num <- 0.8
columns_to_check <- Loans_table %>% select(-key,-column_to_eliminate) %>% names()

x <- check(Loans_table,columns_to_check, num, key,FALSE)



#the check function return a list of two lists, create a function to print them formatted:
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

print_lists(x,num)


#create the tables from the results obtained with the dependencies check:
table_columns <- list()
for( j in 1:length(x)){
  for (col in names(x[[j]])){
    new_column <- Loans_table[[col]]
    names(new_column) <- col 
    table_columns[[col]] <- new_column
  }
}
table <- as.data.frame(table_columns)

Dependent_table <- cbind(Loans_table$NDG, table) %>% rename( NDG = "Loans_table$NDG")

#modify column names:
Dependent_table <- Dependent_table %>% rename('%_Consortium_Guarantee' ="X._Consortium_Guarantee")

for (col_name in colnames(Dependent_table)) {
  if (grepl("\\.*\\.", col_name)) {
    new_col_name <- gsub(".", "(", col_name, fixed = TRUE)
    new_col_name_1 <- paste0(substring(new_col_name, 1, nchar(col_name) - 1), ")")
    colnames(Dependent_table)[colnames(Dependent_table) == col_name] <- new_col_name_1
  }
}

Dependent_table <- Dependent_table %>%  mutate_at(vars(-Default_Date, -Database_Date), ~gsub("-", NA, .))

Dependent_table <- Dependent_table %>% select(-GBV_Expenses)



# create new table from the remaining ones:
Independent_table <- Loans_table
for(col in names(Dependent_table)){
  Independent_table <- Independent_table %>% select(-col)
}
Independent_table <- Independent_table %>% mutate(NDG = Dependent_table$NDG) %>% relocate(NDG, .after = Loans_ID ) %>%
  mutate(across(everything(), ~gsub("-", NA, .)))

