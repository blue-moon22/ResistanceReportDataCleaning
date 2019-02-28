library(openxlsx)

# Read data
data <- read.xlsx("../Open+Atlas_Reuse_Data.xlsx", sheet= 1)

# Clean
data_clean <- data[-c(1,2),]
names(data_clean) <- data_clean[1,]
data_clean <- data_clean[-1,]

# Unique country and species
country_species <- unique(paste(data_clean$Country, data_clean$Species, sep = "_"))
for(i in 1:length(country_species)){
  v <- strsplit(country_species[i], "_")[[1]]
  country <- v[1]
  species <- v[2]
  country_species_data <- data_clean[data_clean$Country == country & data_clean$Species == species,]

  drug_status <- names(country_species_data)[grep("_I", names(country_species_data))]

  country_species_data_reshape <- data.frame()
  for(j in 1:length(drug_status)){
    drug <- gsub("_I", "", drug_status[j])
    tmp_data <- country_species_data[!is.na(country_species_data[[drug]]),c(1:13, which(names(country_species_data) == drug), which(names(country_species_data) == drug_status[j]))]
    if(nrow(tmp_data) > 0){
      col_names <- names(tmp_data)
      col_names[14] <- "MIC"
      col_names[15] <- "breakpoint"
      names(tmp_data) <- col_names
      tmp_data$Drug <- drug
      country_species_data_reshape <- rbind(country_species_data_reshape, tmp_data)
    }
  }
  country_species_data_reshape <- country_species_data_reshape[!is.na(country_species_data_reshape$breakpoint),]
  country_species_data_reshape$Drug <- gsub("\\.", "/", country_species_data_reshape$Drug)

  write.csv(country_species_data_reshape, paste0("../data/", gsub(" ", ".", country_species[i]), ".csv"), row.names = FALSE)
}
