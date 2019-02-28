library(RJSONIO)
library(dplyr)

# Create json files for dropdown selections
filenames <- list.files("../../website/data/dropdown", full.names = TRUE)

# Get countries and species
country <- sapply(filenames, function(x) strsplit(strsplit(x, "/")[[1]][5], "_")[[1]][1])
country_mod <- gsub("\\.", " ", country)
species <- sapply(filenames, function(x) gsub(".json", "", strsplit(strsplit(x, "/")[[1]][5], "_")[[1]][2]))
species_mod <- gsub("\\.", " ", species)

unique_country_mod <- unique(country_mod)
species_mod_list <- lapply(unique_country_mod, function(x) as.character(species_mod[country_mod == x]))

# Get number of points
num_points <- rep(NA, length(filenames))
for(i in 1:length(filenames)){
  json_content <- fromJSON(filenames[i])
  num_points[i] <- length(unlist(sapply(json_content$datasets, function(x) unlist(x$data))))
}
num_points_list <- lapply(unique_country_mod, function(x) num_points[country_mod == x])

# Create dropdown list
dropdown_list <- list()
for(i in 1:length(unique_country_mod)){
  dropdown_list[[i]] <- list(Country = unique_country_mod[i], Species = species_mod_list[[i]], NumPoints = num_points_list[[i]])
}
dropdown_json <- toJSON(dropdown_list)
write(dropdown_json, "../../website/js/dropdown.json")
