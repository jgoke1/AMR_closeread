#library----
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(readxl)
library(ggrepel)
library(tidyverse)
library(highcharter)

#data----
#dataset for atlas
data <- read_csv("atlas_2024.csv")
# Load the dataset for glass
glass <- read_excel("glass-enrolled-countries-8nov-2022 (1).xlsx")

#data prep----
data <- data %>%
  mutate(Continents = if_else(Country %in% c("France", "Spain", "Belgium", "Italy", "Germany", 
                                             "Ireland", "Portugal", "Greece", "United Kingdom", 
                                             "Poland", "Switzerland", "Hungary", "Austria", 
                                             "Finland", "Denmark", "Sweden", "Croatia", 
                                             "Czech Republic", "Netherlands", "Russia", 
                                             "Romania", "Latvia", "Lithuania", "Serbia", 
                                             "Ukraine", "Slovenia", "Bulgaria", "Norway", 
                                             "Slovak Republic", "Estonia"), "Europe",
                              if_else(Country %in% c("Canada", "United States", "Mexico", 
                                                     "Guatemala", "Dominican Republic", "Costa Rica", 
                                                     "Jamaica", "Honduras", "Puerto Rico", 
                                                     "Nicaragua", "Panama","El Salvador"), "North America",
                                      if_else(Country %in% c("Colombia", "Chile", "Venezuela", "Argentina", 
                                                             "Brazil"), "South America",
                                              if_else(Country %in% c("Australia", "New Zealand"), "Australia",
                                                      if_else(Country %in% c("China", "Hong Kong", "Japan", "Malaysia", 
                                                                             "Thailand", "Philippines", "Korea, South", 
                                                                             "Taiwan", "India", "Singapore", "Vietnam", 
                                                                             "Indonesia"), "Asia",
                                                              if_else(Country %in% c("Nigeria", "Kenya", "South Africa", 
                                                                                     "Ivory Coast", "Morocco", "Cameroon", 
                                                                                     "Malawi", "Uganda", "Ghana", "Namibia", 
                                                                                     "Mauritius", "Tunisia", "Egypt"), "Africa",
                                                                      if_else(Country %in% c("Israel", "Kuwait", "Turkey", "Jordan", 
                                                                                             "Saudi Arabia", "Pakistan", "Lebanon", 
                                                                                             "Qatar", "Oman"), "Middle East", 
                                                                              NA_character_))))))))

data <- data %>% filter(Continents == "Africa")

pathogens <- c(
  "Enterococcus faecium",
  "Staphylococcus aureus",
  "Klebsiella pneumoniae",
  "Acinetobacter baumannii",
  "Pseudomonas aeruginosa",
  "Enterobacter cloacae",
  "Escherichia coli",
  "Streptococcus pneumoniae",
  "Haemophilus influenzae",
  "Neisseria gonorrhoeae"
)

#map geocoding----
# Filter African data for specific pathogens and save
data <- data %>% filter(Species %in% pathogens)

Countries_con <- data %>%
  group_by(`Isolate Id`, Year, Country, Species) %>%
  summarise(Count = n(), .groups = "drop")

Countries_con <- Countries_con %>%
  group_by(Country) %>%
  summarise(Count = n(), .groups = "drop")

country_geocodes <- data.frame(
  Country = c("Cameroon", "Egypt", "Ghana", "Ivory Coast", "Kenya", "Malawi",
              "Mauritius", "Morocco", "Namibia", "Nigeria", "South Africa",
              "Tunisia", "Uganda"),
  Latitude = c(7.3697, 26.8206, 7.9465, 7.5399, -1.2864, -13.2543,
               -20.3484, 31.7917, -22.9576, 9.0820, -30.5595, 33.8869, 1.3733),
  Longitude = c(12.3547, 30.8025, -1.0232, -5.5471, 36.8219, 34.3015,
                57.5522, -7.0926, 18.4904, 8.6753, 22.9375, 9.5375, 32.2903)
)

# Merge with your dataset
data_with_geocodes <- merge(Countries_con, country_geocodes, by = "Country")

# Download Africa map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

#map plotting----
Africa_map_con <- ggplot() +
  # Add Africa map
  geom_sf(data = africa, fill = "gray90", color = "black", size = 0.3) +
  # Add country points sized by count
  geom_point(data = data_with_geocodes, 
             aes(x = Longitude, y = Latitude, size = Count, color = Count), 
             alpha = 0.8) +
  # Add country labels, colored by Count
  geom_text_repel(data = data_with_geocodes, 
                  aes(x = Longitude, y = Latitude, label = Country, color = Count),
                  size = 3, fontface = "bold") +
  # Scale for count
  scale_color_gradientn(colors = c("#313695", "#4575b4", "#74add1", "#abd9e9", 
                                   "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"), 
                        name = "Contribution") +
  scale_size_continuous(range = c(3, 10), name = "Contribution") +
  # Theme and titles
  theme_minimal() +
  theme(
    text = element_text(size = 12, color = "#333333"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "African Countries in the ATLAS Surveillance",
    subtitle = "Volume of contributions to the data",
    caption = "Data Source: Pfizer-ATLAS"
  )


#Species trend by year----
species_trend <- data %>%
  group_by(`Isolate Id`, Year, Species) %>%
  summarise(Count = n(), .groups = "drop")

species_trend <- species_trend %>%
  group_by(Year, Species) %>%
  summarise(Count = n(), .groups = "drop")

unique_species <- unique(species_trend$Species)

species_colors <- c(
  "#000000", # House Stark (Black Direwolf)
  "#FFD700", # House Lannister (Gold Lion)
  "#800000", # House Targaryen (Red Dragon)
  "#003366", # House Baratheon (Navy Blue with Stag)
  "#4B5320", # House Tyrell (Olive Green Rose)
  "#708090", # House Greyjoy (Slate Grey Kraken)
  "#DAA520", # House Martell (Golden Spear and Sun)
  "#8B4513", # House Tully (Brown/Brindle Trout)
  "#2E8B57", # House Arryn (Sea Green Falcon)
  "#B22222"  # The Wall (Firebrick Red for the Night's Watch)
)

# Generate the chart with your custom settings
highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(
    text = "Species Trend Over Years",
    margin = 20,
    align = "left",
    style = list(color = "steelblue")
  ) %>%
  hc_subtitle(
    text = "Yearly Trends of Different Species",
    align = "left",
    style = list(color = "#2b908f", fontWeight = "bold")
  ) %>%
  hc_xAxis(categories = unique(species_trend$Year), title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Count")) %>%
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 0,
    y = 100
  ) %>%
  hc_tooltip(
    crosshairs = TRUE,
    backgroundColor = "#FCFFC5",
    shared = TRUE,
    borderWidth = 4
  ) %>%
  hc_credits(
    enabled = TRUE,
    text = "Species Data",
    href = "http://example.com"
  ) %>%
  hc_add_series_list(
    lapply(seq_along(unique_species), function(i) {
      current_species <- unique_species[i]
      list(
        name = current_species,
        data = species_trend %>%
          filter(Species == current_species) %>%
          arrange(Year) %>%
          pull(Count),
        color = species_colors[i] # Assign Game of Thrones-inspired color
      )
    })
  )

#ESKAPE----
unique_species <- unique(species_trend$Species)

# Ensure there are enough colors for all species
if (length(species_colors) < length(unique_species)) {
  stop("Not enough colors specified for all species")
}

# Create a list to store the individual species charts
species_charts <- list()

# Iterate over each species to generate individual charts
for (i in seq_along(unique_species)) {
  current_species <- unique_species[i]
  current_color <- species_colors[i]
  
  chart <- highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(
      text = paste("Trend for", current_species),
      margin = 20,
      align = "left",
      style = list(color = "steelblue")
    ) %>%
    hc_subtitle(
      text = paste("Yearly Trend of", current_species),
      align = "left",
      style = list(color = "#2b908f", fontWeight = "bold")
    ) %>%
    hc_xAxis(categories = unique(species_trend$Year), title = list(text = "Year")) %>%
    hc_yAxis(title = list(text = "Count")) %>%
    hc_legend(enabled = FALSE) %>% # Disable legend for individual charts
    hc_tooltip(
      crosshairs = TRUE,
      backgroundColor = "#FCFFC5",
      shared = TRUE,
      borderWidth = 4
    ) %>%
    hc_credits(
      enabled = TRUE,
      text = paste("Species Data: ", current_species),
      href = "http://example.com"
    ) %>%
    hc_add_series(
      name = current_species,
      data = species_trend %>%
        filter(Species == current_species) %>%
        arrange(Year) %>%
        pull(Count),
      color = current_color
    )
  
  # Store the chart in the list
  species_charts[[current_species]] <- chart
}

#Individual species----
species_charts[[unique_species[1]]]
species_charts[[unique_species[4]]]
species_charts[[unique_species[8]]]
species_charts[[unique_species[6]]]
species_charts[[unique_species[9]]]
species_charts[[unique_species[3]]]
#GLASS plotting----
# Filter for African countries and GLASS participation status
africa_glass <- glass %>%
  filter(`WHO Region` == "African Region") %>%
  mutate(Participation = ifelse(AMR == "Y", "Participating", "Not Participating")) %>%
  select(Label, Participation)

# Load a shapefile of Africa for mapping
africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Merge participation data with the map
africa_merged <- africa_map %>%
  left_join(africa_glass, by = c("admin" = "Label"))

# Calculate percentage of participating countries
total_countries <- nrow(africa_merged)
participating_countries <- nrow(africa_merged %>% filter(Participation == "Participating"))
participation_percentage <- round((participating_countries / total_countries) * 100, 2)

# Plot the map
ggplot(data = africa_merged) +
  geom_sf(aes(fill = Participation), color = "black") +
  scale_fill_manual(
    values = c("Participating" = "green", "Not Participating" = "red"),
    na.value = "gray"
  ) +
  labs(
    title = "African Countries in GLASS",
    subtitle = paste0(
      participating_countries, " out of ", total_countries, 
      " countries participating (", participation_percentage, "%)"
    ),
    fill = "Participation Status",
    caption = "Source: GLASS-data(2022)" # Add your data source link here
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(), # Remove latitude and longitude labels
    axis.title = element_blank() # Remove axis titles
  )
