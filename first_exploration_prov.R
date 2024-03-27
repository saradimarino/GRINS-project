library("data.table")    
library("mice")



# Importing data ----------------------------------------------------------

#Importing some variables from provincial data (to be changed later)

data <- fread("db_provinciale_21-02.csv", 
                  select =c("anno", "Provincia (NUTS3) - Nome","superficie (kmq)", "Uso efficiente suolo", "Visitatori di Musei, Monumenti e Aree Archeologiche Statali", "Introiti di Musei, Monumenti e Aree Archeologiche Statali", "Parco veicolare, IN579, Unita", "Stazioni ferroviarie, IN608, Unita", "Aeroporti, IN582, Unita", "gdp_const_p_2015", "employment", "Eta media del padre al parto", "tasso di natalita (per mille abitanti)", "tasso di mortalita (per mille abitanti)", "saldo migratorio interno (per mille abitanti)")) 

head(data)

# Changing variable names to treat them more easily
names(data)<-c("year", "province","surface", "soil_eff", "museums_visit", "museums_income", "cars", "train_stations", "airports", "gdp_const_p_2015", "employment", "avg_age_father", "mortality_r", "natality_r", "internal_migr")


# Load the stringi package for Unicode support
library(stringi)

# Remove spaces and special characters from province labels
data$province <- stri_trans_general(data$province, "Latin-ASCII")  # Convert accented characters to their ASCII equivalents
data$province <- gsub("[[:space:]]", "", data$province)  # Remove spaces
data$province <- gsub("[^[:alnum:]]", "", data$province) # Remove remaining special characters

# Convert province to factor
data$province <- as.factor(data$province)

#Count the number of provinces
num_provinces<-length(unique(data$province))
num_provinces

# Convert other variables to numeric (some variables are saved as char)
data$surface <- as.numeric(gsub("\\.", "", data$surface))  # remove dots before converting to numeric
data$soil_eff <- as.numeric(data$soil_eff)
data$museums_visit <- as.numeric(data$museums_visit)
data$museums_income <- as.numeric(data$museums_income)
data$cars <- as.numeric(data$cars)
data$train_stations <- as.numeric(data$train_stations)
data$airports <- as.numeric(data$airports)
data$gdp_const_p_2015 <- as.numeric(data$gdp_const_p_2015)
data$employment <- as.numeric(data$employment)
data$avg_age_father <- as.numeric(data$avg_age_father)
data$mortality_r <- as.numeric(data$mortality_r)
data$natality_r <- as.numeric(data$natality_r)
data$internal_migr <- as.numeric(data$internal_migr)

# Table to visualize how the missing values are distributed
md.pattern(data, plot = TRUE, rotate.names = TRUE)
# 146 samples are complete, 442 samples are missing soil efficiency, ....


# Identify variables with missing data
vars_with_missing <- colnames(data)[colSums(is.na(data)) > 0]


# Group data by province and count missing values for each variable
missing_by_location <- lapply(unique(data$province), function(location) {
  location_data <- data[data$province == location, ]
  missing_counts <- colSums(is.na(location_data))
  return(missing_counts)
})

# Combine the missingness information for all locations
missing_info <- do.call(rbind, missing_by_location)

# Print missingness information for each variable by location
print(missing_info)

rownames(missing_info) <- unique(data$province)

# Identify variables and provinces where data is missing for all the years
missing_13 <- which(missing_info == 13, arr.ind = TRUE)
if (nrow(missing_13) > 0) {
  for (i in 1:nrow(missing_13)) {   
    row_idx <- missing_13[i, 1]
    col_idx <- missing_13[i, 2]
    province <- rownames(missing_info)[row_idx]
    variable <- colnames(missing_info)[col_idx]
    print(paste("Variable:", variable, "- Province:", province))
  }
} else {
  print("No variable and province combination has 13 missing data points.")
}


# Imputation for Province (first try...) -------------------------------------------------
# Group the data by province
data_by_province <- split(data, data$province)

# Initialize a list to store imputed datasets for each province
imputed_datasets <- list()

# Loop over each province and perform imputation
for (province in names(data_by_province)) {
  # Subset data for the current province
  province_data <- data_by_province[[province]]
  
  # Check for variables with missing data for all 13 years
  vars_to_impute <- colnames(province_data)[colSums(is.na(province_data)) != 13]
  
  if (length(vars_to_impute) > 0) {
    # Set up the imputation model formula for the current province
    imp_formula <- lapply(vars_to_impute, function(var) as.formula(paste(var, "~ year")))
    
    # Run the MICE algorithm for the current province
    imp_model <- mice(province_data, method = "pmm", m = 5, maxit = 50, formulas = imp_formula)
    
    # Generate imputed datasets for the current province
    imputed_data <- complete(imp_model)
    
    # Store the imputed dataset for the current province
    imputed_datasets[[province]] <- imputed_data
  } else {
    # If all variables have missing data for all 13 years, skip imputation for this province
    cat("Skipping imputation for", province, "as all variables have missing data for all 13 years.\n")
  }
}

# Combine imputed datasets for all provinces
combined_imputed_data <- do.call(rbind, imputed_datasets)





