# Adele Tyson, SLaM, Adele.Tyson@slam.nhs.uk
# 11/11/2021
# Maps GP practices

# Data for borough boundaries is from
# Data from https://data.gov.uk/dataset/b308dc50-3b4c-4dca-b870-eab1053a58c7/local-planning-authorities-april-2019-uk-buc

################################################################################

# Create base map of SLaM boroughs

# Import boroughs data from shapefiles
boroughs <- st_read(dsn = "Data_inputs/London_Boroughs_shape/LPA_MAY_2021_UK_BFC_V2.shp") 

learning = 0
if (learning) {
  # Show some of the metadata for interest/learning
  print(st_geometry_type(boroughs)) # the objects are MULTIPOLYGONS
  print(st_crs(boroughs)) # the coordinate reference system (CRS) is Ordnance Survey GB 1936 British National Grid
  ### NB: For OSGB 1936, the EPSG code is 27700 ###
  print(st_bbox(boroughs)) # the boundary box is x = (-116, 655654), y = (5337, 1220302)
  # Show all metadata
  print(boroughs)
}


# Only want SLaM boroughs
boroughs_slam <- boroughs %>%
  filter(LPA21NM == "Lambeth LPA" |
           LPA21NM == "Southwark LPA" |
           LPA21NM == "Lewisham LPA" |
           LPA21NM == "Croydon LPA")

# Plot SLaM boroughs
boroughs_plot <- ggplot() +
  geom_sf(data = boroughs_slam) +
  coord_sf()
boroughs_plot

################################################################################

# Add locations of GP practices to map

practices_df <- read_excel(path = "Data_outputs/GP-practices.xlsx")

# Turn data.frame into sf object with geometry column
practices_sf <- st_as_sf(practices_df, coords = c("LAT", "LONG"),
                         crs = 4326, agr = "identity")
# Transform geospatial coordinates system from WGS84 lat/long (4326) to
# OSGB 1936 (27700)
practices_sf <- st_transform(practices_sf, 27700)

# Plot SLaM boroughs and practice locations
ggplot() +
  geom_sf(data = boroughs_slam) +
  geom_sf(data = practices_sf) +
  coord_sf()

################################################################################

# Add cost of MH drugs per practice (2019 and sum of all drugs for now) to map

practices_cost_df <- merge(x = cost_prac, 
                        y = practices_df, 
                        by.x = "row_name", 
                        by.y = "prac.name", 
                        all.x = TRUE)
# NB: there are 4 extra rows because there are some practices that appear twice
# in practices_sf because either they had 2 different GP codes in the original
# data or they have 2 different locations. It's not worth fixing for this
# visualisation but note that 4 practices will be plotted twice, sometimes on
# top of each other (not an issue) and sometimes in 2 locations (an issue).
practices_cost_sf <- st_as_sf(practices_cost_df, coords = c("LAT", "LONG"),
                              crs = 4326, agr = "identity")

ggplot() +
  geom_sf(data = boroughs_slam) +
  geom_sf(data = practices_cost_sf, aes(colour = annual_cost)) +
  coord_sf() + 
  scale_colour_viridis(option = "turbo") #+ theme_bw()

################################################################################
################################################################################
