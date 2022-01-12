# Adele Tyson, SLaM, Adele.Tyson@slam.nhs.uk
# 11/11/2021
# Gets IMD data by LSOA

# Data for IMD is from
# Data from https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833978/File_5_-_IoD2019_Scores.xlsx

# NB: low IMD scores mean least deprived (0-8.49), high scores mean most
# deprived (34.18+)

################################################################################

# Combine data for lsoa boundaries, IMD scores and costs by GP practice

# Get IMD scores by lower layer super output areas
imd_data_all <- read_excel(path = "Data_inputs/imd_lsoa.xlsx", 
                        sheet = "IoD2019 Scores", 
                        range = cell_cols(1:5)) %>%
  clean_names() %>%
  rename(lsoa_code = lsoa_code_2011,
         lsoa_name = lsoa_name_2011,
         lad_code = local_authority_district_code_2019,
         lad_name = local_authority_district_name_2019,
         imd_score = index_of_multiple_deprivation_imd_score) # syntax is new_name = old_name

# Restrict to SLaM boroughs
imd_data <- imd_data_all %>%
  filter(lad_name == "Lambeth" |
           lad_name == "Southwark" |
           lad_name == "Lewisham" |
           lad_name == "Croydon")

# Import boundaries of LSOAs as shapefile
lsoa_boundaries_all <- st_read(dsn = "Data_inputs/London_lsoa_shape/LLSOA_Dec11_BFC_EW_V3.shp") 

# Restrict to SLaM boroughs
lsoa_boundaries <- lsoa_boundaries_all %>%
  filter(grepl("Croydon|Lambeth|Lewisham|Southwark", LSOA11NM))

# Add data on IMD scores to LSOA boundaries
lsoa_imd <- left_join(lsoa_boundaries, imd_data, 
                      by = c("LSOA11CD" = "lsoa_code"))

################################################################################

# Create static ggplot map

# Plot LSOA boundaries and colour by IMD score
ggplot() +
  geom_sf(data = lsoa_imd, aes(fill = imd_score)) +
  geom_sf(data = practices_cost_sf, aes(size = annual_cost, alpha = 0.01)) +    #aes(colour = annual_cost
  coord_sf() +
  scale_fill_viridis_c(option = "turbo")

################################################################################

# Create interactive leaflet map

# Transform into long/lat coordinates
lsoa_imd_ll <- st_transform(lsoa_imd, crs = 4326)

# Define colour palate
col_pal <- colorNumeric(palette = "viridis", domain = NULL)

# Define labels for LSOAs and practices
lsoa_labels <- sprintf("<strong>%s</strong><br/>IMD Score: %g",
                       lsoa_imd_ll$LSOA11NM, lsoa_imd_ll$imd_score) %>%
  lapply(htmltools::HTML)
prac_labels <- sprintf("<strong>%s</strong><br/>MH prescription cost: ?%g",
                       practices_cost_sf$row_name, practices_cost_sf$annual_cost) %>%
  lapply(htmltools::HTML)

leaflet(lsoa_imd_ll) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, # Don't know what stroke does, works without it
              weight = 0.5,
              color = "black",
              opacity = 1.0, # Boundary line opacity
              fillColor = ~col_pal(imd_score),
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 2,
                color = "white",
                bringToFront = TRUE),
              label = ~lsoa_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "right")) %>% 
  addLegend(position = "bottomright",
            pal = col_pal, 
            values = ~imd_score, 
            opacity = 1.0,
            title = "IMD Score") %>%
  addCircles(data = practices_cost_sf,
             weight = ~annual_cost/10000,
             color = "red",
             opacity = 0.5,
             label = ~prac_labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "left")) %>%
  # would like to be able to have a second legend here for the circles
  setView(lng = -0.066, lat = 51.4, zoom = 11.3)


