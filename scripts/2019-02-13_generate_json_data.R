library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RJSONIO)
library(RColorBrewer)

# # # Read data
# data <- read.xlsx("../Open+Atlas_Reuse_Data.xlsx", sheet= 1)
#
# # Clean
# data_clean <- data[-c(1,2),]
# names(data_clean) <- data_clean[1,]
# data_clean <- data_clean[-1,]
#
# # Get antibiotics
# antibiotics <- gsub("_I", "", names(data_clean)[grep("_I", names(data_clean))])
#
# # Get years
# years <- sort(as.numeric(unique(data_clean$Year)))
#
# # Save Rdata
# save(antibiotics, file = "antibiotics.RData")
# save(years, file = "years.RData")

# Read resistance map data
rm <- read.csv("../public_data/resistanceMapData.csv", stringsAsFactors = FALSE)

# Read resistance map antibiotic lookup table
rm_lookup <- read.csv("../public_data/resistance_map_antibiotics_lookup.csv", stringsAsFactors = FALSE)

# Read general antibiotic lookup table
gen_lookup <- read.csv("../public_data/antibiotics_lookup.csv", stringsAsFactors = FALSE)

# Unnest resistant map data
rmu <- rm %>% transform(Group = strsplit(Group, "; ")) %>%
  unnest(Group) %>%
  transform(Year = strsplit(Year, "; ")) %>%
  unnest(Year) %>%
  transform(Species = strsplit(Species, "; ")) %>%
  unnest(Species)

# Unnest lookup tables
rm_lookup <- rm_lookup %>% transform(Country = strsplit(Country, ";")) %>%
  unnest(Country) %>%
  transform(Species = strsplit(Species, "; ")) %>%
  unnest(Species) %>%
  transform(Drug = strsplit(Drug, "; ")) %>%
  unnest(Drug)

# Combine resistance map data and lookups
rmu <- left_join(rmu, rm_lookup, by = c("Country", "Species", "Group"))
rmu_rm_lookup <- rmu[!is.na(rmu$Drug),]
rmu_gen_lookup <- rmu[is.na(rmu$Drug),] %>% select(-Drug)
rmu_gen_lookup <- left_join(rmu_gen_lookup, gen_lookup, by = "Group")
rmu_rm_lookup$TrueLink <- TRUE
rmu_gen_lookup$TrueLink <- FALSE

rmu_comb <- rbind(rmu_rm_lookup, rmu_gen_lookup)
rmu_comb$Year <- as.integer(rmu_comb$Year)

# Create resistance map links
link <- paste0("https://resistancemap.cddep.org/AntibioticResistance.php?charttype=trend&organism=",
               gsub(" ", "%20", rmu_comb$Species), "&country=",
               gsub(",", "%2C", rmu_comb$Country), "&antibiotic[]=",
               rmu_comb$Group, "&showerrorbars=true")
link <- gsub("Korea%2C South", "Korea2C Rep.", link)
rmu_comb$Link <- link

# Get antibiotics and years
load("antibiotics.RData")
load("years.RData")

# Create json files for each country, species, and antibiotic
filenames <- list.files("../data", full.names = TRUE)
colours <- tolower(c(brewer.pal(12, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"), brewer.pal(8, "Set1"), brewer.pal(4, "Set2")))

# Create json files for each country and species
for(i in 1:length(filenames)){
  summary <- data.frame()
  df <- read.csv(filenames[i], stringsAsFactors = FALSE)
  country <- strsplit(strsplit(filenames[i], "/")[[1]][3], "_")[[1]][1]
  species <- gsub("\\.", " ", gsub(".csv", "", strsplit(strsplit(filenames[i], "/")[[1]][3], "_")[[1]][2]))

  # Summarise percentages by variables
  summary <- df %>% group_by(Drug, breakpoint, Year) %>%
    summarise(n = n()) %>%
    ungroup %>%
    group_by(Drug, Year) %>%
    mutate(percentage = n/sum(n)*100) %>%
    filter(breakpoint == "Resistant") %>%
    select(-c(breakpoint, n)) %>%
    as.data.frame()

  # Insert years with no values
  if(nrow(summary) > 0){
    unique_drug <- unique(summary$Drug)
    summary_insert <- data.frame(Drug = as.character(), Year = integer(), percentage = numeric(), stringsAsFactors = FALSE)
    for(j in 1:length(unique_drug)){
      year <- years[!years %in% summary$Year[summary$Drug == unique_drug[j]]]
      if(length(year) > 0){
        summary_insert <- rbind(summary_insert, data.frame(Drug = as.character(unique_drug[j]), Year = as.integer(year), percentage = NA))
      }
    }
    summary <- rbind(summary, summary_insert) %>% arrange(Year)

    # Create json
    summary_json <- RJSONIO::toJSON(list(Country = country, Species = species, Year = years,
                                         datasets = lapply(unique_drug, function(x)
                                           list(label = x, data = summary$percentage[summary$Drug == x], fill = FALSE, borderColor = colours[antibiotics == x]))), .na = "null",
                                    .escapeEscapes = FALSE)
    try(write(summary_json, paste0("../website/data/dropdown/", gsub(".csv", "", strsplit(filenames[i], "/")[[1]][3]), ".json")))

    # Create json of reported/non-reported data
    summary$Country <- country
    summary$Species <- species
    summary_comb <- left_join(summary, rmu_comb, by = c("Country", "Species", "Year", "Drug"))
    summary_reported <- summary_comb
    summary_reported$percentage[is.na(summary_reported$Group)] <- NA

    links <- rep(NA, length(years))
    for(k in 1:length(years)){
      summary_year <- summary_reported[summary_reported$Year == years[k],]
      if(sum(!is.na(summary_year$Link)) != 0){
        link_antibiotics <- unique(summary_year$Group[!is.na(summary_year$Link)])
        links[k] <- paste0("https://resistancemap.cddep.org/AntibioticResistance.php?charttype=trend&organism=",
               gsub(" ", "%20", species), "&country=",
               gsub(",", "%2C", country),
               paste0("&antibiotic[]=", paste(link_antibiotics, collapse="&antibiotic[]=")),
               "&showerrorbars=true")
      }
    }

    summary_reported_json <- RJSONIO::toJSON(list(Country = country, Species = species, Year = years, Link = links, DataSource = unique(summary_reported$Data_Source[!is.na(summary_reported$Data_Source)]),
                                                  datasets = lapply(unique_drug, function(x)
                                                    list(label = x, data = summary_reported$percentage[summary_reported$Drug == x], fill = FALSE,
                                                         borderColor = colours[antibiotics == x]))), .na = "null",
                                             .escapeEscapes = FALSE)
    try(write(summary_reported_json, paste0("../../website/data/reported/", gsub(".csv", "", strsplit(filenames[i], "/")[[1]][3]), "reported.json")))

    summary_notreported <- summary_comb
    summary_notreported$percentage[!is.na(summary_reported$Group)] <- NA
    summary_notreported_json <- RJSONIO::toJSON(list(Country = country, Species = species, Year = years,
                                                  datasets = lapply(unique_drug, function(x)
                                                    list(label = x, data = summary_notreported$percentage[summary_notreported$Drug == x], fill = FALSE,
                                                         borderColor = colours[antibiotics == x]))), .na = "null",
                                             .escapeEscapes = FALSE)
    try(write(summary_notreported_json, paste0("../../website/data/notreported/", gsub(".csv", "", strsplit(filenames[i], "/")[[1]][3]), "notreported.json")))
  }
}

# Create json files for each country, species, year, antibiotic
variable <- c("State", "Gender", "Age.Group", "Speciality", "Source", "In...Out.Patient")
for(i in 3748:length(filenames)){
  df <- read.csv(filenames[i], stringsAsFactors = FALSE)

  # Remove NA values in variables
  for(k in 1:length(variable)){
    if(variable[k] == "State"){
      df[variable[k]][is.na(df[variable[k]])] <- "N/A"
    } else {
      df[variable[k]][is.na(df[variable[k]])] <- "Not recorded"
    }
  }

  country <- strsplit(strsplit(filenames[i], "/")[[1]][3], "_")[[1]][1]
  species <- gsub("\\.", " ", gsub(".csv", "", strsplit(strsplit(filenames[i], "/")[[1]][3], "_")[[1]][2]))
  year <- unique(df$Year)
  for(j in 1:length(year)) {
    df_year <- df[df$Year == year[j],]
    summary <- df_year %>% group_by(Drug, breakpoint) %>%
      summarise(n = n()) %>%
      group_by(breakpoint) %>%
      mutate(percentage = n/sum(n)*100)

    summary_weights <- summary %>% group_by(breakpoint) %>%
      summarise(n_breakpoints = sum(n)) %>%
      mutate(weight = n_breakpoints/sum(n_breakpoints))

    summary <- summary %>% select(-n)
    breakpoints <- c("Susceptible", "Intermediate", "Resistant")

    summary_list <- lapply(breakpoints, function(x) {
      summary_breakpoint <- summary[summary$breakpoint == x,]
      if(nrow(summary_breakpoint) > 0){
        drugs <- lapply(unique(summary_breakpoint$Drug), function(y) {
          summary_bd <- df_year[df_year$Drug == y & df_year$breakpoint == x,]

          variable_split <- lapply(variable, function(z) {
            summary <- summary_bd %>% group_by_(z) %>%
              summarise(n = n()) %>%
              mutate(percentage = n/sum(n)*100) %>%
              select(-n)
            return(summary)
          })
          names(variable_split) <- variable
          return(variable_split)
        })
        names(drugs) <- unique(summary_breakpoint$Drug)
        sb_list <- list(Drug = drugs,
        percentage = c(summary_breakpoint$percentage),
        weight = summary_weights$weight[summary_weights$breakpoint == x],
        backgroundColor = colours[unlist(sapply(summary_breakpoint$Drug, function(y) which(antibiotics == y)))])
      } else {
        sb_list <- list(Drug = c(), percentage = c())
      }
      return(sb_list)
    })
    names(summary_list) <- breakpoints
    summary_json <- toJSON(summary_list)
    write(summary_json, paste0("../../website/data/year/", gsub(".csv", "", strsplit(filenames[i], "/")[[1]][3]), "_", as.character(year[j]), ".json"))
  }
}
