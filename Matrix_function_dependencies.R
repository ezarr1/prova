#function that check the dependencies of all column vs all column and saves the results in a matrix:

find_dependencies_matrix <- function(table) {
  num_cols <- ncol(table)
  dependency_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  colnames(dependency_matrix) <- colnames(table)
  rownames(dependency_matrix) <- colnames(table)
  
  for (i in 1:num_cols) {
    for (j in 1:num_cols) {
      if (i == j) {
        dependency_matrix[i, j] <- 1
      } else{
         dependency_matrix[i, j] <- check_dependencies_col1_vs_col2(table,i,j)
      }
    }
  }
  
  return(dependency_matrix)
}



check_dependencies_col1_vs_col2<- function(table, i, j) {
    grouped_data <- split(table[,i], table[,j], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (sum(dependency_check)== length(dependency_check)) {
      result <- 1
    } else {
      result <- length(dependency_check) / sum(dependency_check)
    }
  return(result)
}




#######################################################################################################################################
#######################################################################################################################################
# tutto quello sopra non serve




find_dependencies_matrix_2 <- function(table) {
  num_cols <- ncol(table)
  dependency_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  colnames(dependency_matrix) <- colnames(table)
  rownames(dependency_matrix) <- colnames(table)
  
  for (col_2 in colnames(dependency_matrix)) {
    new_column <- check_dependencies_1(table, col_2) 
    new_column <- unlist(new_column)
    dependency_matrix[,col_2] <- new_column
  }
  return(dependency_matrix)
}

check_dependencies_1<- function(table, key) {
  lista_num <- list()
  for (col in names(table)) {
    grouped_data <- split(table[[col]], table[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (all(dependency_check ==1 )) {
      lista_num[[col]] <- 1
    } else {
      lista_num[[col]] <- length(dependency_check) / sum(dependency_check)
    }
  }
  
  result <- lista_num
  return(result)
}



complex_data <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(25, 30, 22, 28),
  Gender = c("Female", "Male...", "Male", "Male"),
  Scores = c(95, 88, 75, 92),
  Passed = c(TRUE, TRUE, FALSE, TRUE),
  Address = c("123 Main St", "456 Elm St", "789 Oak St", "101 Pine St"),
  stringsAsFactors = FALSE  # Prevent automatic conversion to factors
)

x <- find_dependencies_matrix_2(complex_data)

