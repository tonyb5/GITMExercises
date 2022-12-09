### "onGEO GEOVisualization Course: Census API and WebMapping script"
## Kyle Redican 7/2022


###################################################################################
### First step in any project is to set your working directory
### Working directory is just a fancy way of saying where you are keeping and saving the data
### By setting the working directory you can easily save without having to add more code
### setwd() function sets the working directory that you identify for you
setwd("C:/Users/bowman84/Documents/GEOVIS _EXCERCISES/CourseMaterial/EXERCISE4")

### Libraries
### Libraries are essentially the software packages that provide the functions 
### That you will use to manipulate and change the data
# Tidyverse is a library that is used to manipulate data in an easy to use cross sectional approach
library(tidyverse)
#Tidy Census is the library that connects to the Census API and allows you to pull the data
library(tidycensus)
# GGPLOT2 is one of the most popular data visualization packages.  You can make maps and anytype of plot you might want
library(ggplot2)
# Hmisc is another library that helps with the data manipulation
library(Hmisc)
# SF is the simple features library that allows you work with the spatial datatype of simple features
library(sf)
# Leaflet Gives the base map
library(leaflet)
# Stringr
library(stringr)
# Allows us to export our leaflet map
library(htmlwidgets)

### Part 1 Finding your Census Variables
# First you will need to install your census api
# The census API should have been gotten from https://api.census.gov/data/key_signup.html
# using the census_api_key function here put in your api key.  
# If you want to not have to do this every time you want to download census data, then add the second argument of install=TRUE.  If you choose this, then you might need to restart R. 
census_api_key("a27beb2d8cc5300d8ff0ee66d430af96a2d713af", install=TRUE)
#alternative would be census_api_key("YOUR API KEY GOES HERE", install=TRUE)



# Loading the Possible Census Variables -----------------------------------
# Now we need to load the variables to determine which ones would be the best for us.
# using the load_variables() function to load an dataframe that tells you the codes for the variables we want to extract.
# First we will load the American Community Survey (ACS) 5 year estimates 2016-2020(last available ACS with tidycensus).
# First argument is the year,second is the dataset (acs is the complete ACS dataset), and third is just left as cache=false
ACS<-load_variables(2020,
                    "acs5",
                    cache=FALSE)


# Check out what it brought back.  Looks like 27,040 observations of 3 variables
head(ACS)
# now lets open the dataframe and catch all the codes for the topic you are most interested in.
view(ACS)



### Let's grab some variables
### Race/ethnicity variables come in counts of the population
### Since it is counts, that is only going to pick up on the most populated areas and not based on concentration
### Due to this issue, we are going to have to do a simple calculation

# EXAMPLE 1- Grabbing the White alone population and turning it into a %
### Using the get ACS Function we can get these counts
#First we define the list of variables that we want to pull and give them better names then the code
# To make the list it just your objectlistname <- then c() which means combine items into a vector.  Then inside the c() you are defining your items and names.
# Since the names from the census API are those codes that don't have a lot of meaning, we give it meaning with the MHI =, where that code will be saved with the name MHI
Variables<-c(MHI = "B19013_001", # Median Household Income
             MHV = "B25077_001", # Median Home Value
             Totalpop = "B01003_001") # Total Population
             

County<-get_acs(geography = "county",  #Defining the spatial scale
                      variables = Variables, #grabbing the variables we defined previously
                      survey = "acs5",      # survey we are pulling is the ACS 5 year estimates
                      year = 2020,   # End year for the 5 year estimates- 2016-2020
                      output = "wide", # data format wide (cross sectional)
                      geometry=T) # Pulls the spatial data information with it.

#Check out your data a little bit, it brought back 3220 observations of 9 variables
# First 10 records/observations return
head(County)

# summary() provides an overview of the data
summary(County)

# Notice how there is two numbers and columns for each of the variables? ex. MHIE and MHIM
# MHIE or the ones with an E on the end are the estimate, while the MHIM or ones with M are margin of error
# For the purposes of this lab we only care about the estimates
# Lets clean up this data a little bit before we go too far
# Here we are just pulling out the columns we care about using the indexing dataframe[,c("column name","etc")]
Counties<-County[,c("GEOID", "NAME", "MHIE", "MHVE", "TotalpopE", "geometry")]
# Easy renaming of the variables
names(Counties)[names(Counties) =="MHIE"]<-"MHI"
names(Counties)[names(Counties) =="MHVE"]<-"MHV"
names(Counties)[names(Counties) =="TotalpopE"]<-"Totalpop"


### Setting up the color palletes
### To get the color schemes you will need to set these up
# The instructions have the different color palettes you can choose
# domain is the data that you want to create breaks for.  Here the domain for each of these is the variable
# n says the number of colors and breaks that you will need.  By making that 10 here we are making a color palette for deciles or 10%.
# Lowest values would be in the bottom 10% and highest in the top 10%
palMHI<-colorQuantile(palette = "viridis", domain=Counties$MHI, n=10)
palMHV<-colorQuantile(palette = "RdYlBu", domain=Counties$MHV, n=10)
palpop<-colorQuantile(palette = "RdBu", domain=Counties$Totalpop, n=10)


### The Leaflet map
## Named map
## this will take awhile
map<-Counties %>% # Data set you want to map and the '%>%' is a piping command which makes it keep moving forward 
  st_transform(crs = "+init=epsg:4326") %>% # Setting the projection of our simple feature spatial dataframe to match the webmapping application
  leaflet() %>%  # Leaflet function which puts this all in motion
  setView(lng = -98.55, lat = 39.8098, zoom = 3)%>% # the long and lat set the center point, while zoom chooses how zoomed in it is
  addTiles(group = "OSM (default)") %>%  #This sets the base map and the following addProviderTiles() are optional otehr basemaps
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto B") %>%
  addPolygons(group = "Total", #Add polygons is how we choose what we want to map.
    popup = paste("County: ", Counties$NAME, "<br>", #Pop up provides the pop up information if we were to click on the county, paste() pastes all the text together
                            "Total Population: ", Counties$Totalpop, "<br>", # <br> is html code for new line.   
                            "Median Household Income: ", "$", Counties$MHI, "<br>",
                            "Median Home Value: ", "$", Counties$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ palpop(Totalpop)) %>% #Color palette or what the color scheme is.  You made these earlier and they need to match
  addLegend(position = "bottomleft", pal=palpop, values= Counties$Totalpop, #Legend for this polygon layer
            title="Percentile of County Total Population", group = "Total", opacity = 1) %>% 
  addPolygons(group = "MHI", #Next layer with the same set up as before.  We could add as many as the computer would let us. 
              popup = paste("County: ", Counties$NAME, "<br>",
                            "Total Population: ", Counties$Totalpop, "<br>",
                            "Median Household Income: ", "$", Counties$MHI, "<br>",
                            "Median Home Value: ", "$", Counties$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ palMHI(MHI)) %>%
  addLegend(position = "bottomright", pal=palMHI, values= Counties$MHI, 
            title="Percentile of County Median Household Income", group = "MHI", opacity = 1) %>% 
  addPolygons(group = "MHV",
              popup = paste("County: ", Counties$NAME, "<br>",
                            "Total Population: ", Counties$Totalpop, "<br>",
                            "Median Household Income: ", "$", Counties$MHI, "<br>",
                            "Median Home Value: ", "$", Counties$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ palMHV(MHV)) %>%
  addLegend(position = "bottomleft", pal=palMHV, values= Counties$MHV, 
            title="Percentile of County Median Household Income", group = "MHV", opacity = 1) %>% 
  # Here we hide two of the groups so that only one shows on loading
  hideGroup("MHI") %>% # Hides the MHI layer so it doesn't all pop up at load
  hideGroup("MHV") %>% # Hides the MHV layer so it all doesn't pop up at the load
  addLayersControl(  #Gives you the option to turn on and off different basemaps or layers.
    baseGroups = c("OSM (default)", "Toner Lite", "Carto B"),
    overlayGroups = c("Total", "MHI", "MHV"),
    options = layersControlOptions(collapsed = FALSE)) 
  

map # loads the map you just made, this will take awhile

# Saving the Html map
# first argument is the leaflet map, second is the file name and extension, and final is whether it is selfcontained
saveWidget(map, file="map1.html", selfcontained = T)


### Saving the shapefile
st_write(Counties, "counties.shp")



# Smaller Defined Spatial Unit Example ------------------------------------

### Here is a small example to show how to pull out the Census Tracts for a specific MSA (the 6 county Detroit MSA)
# First we define a new dataframe.  Since it isn't all US counties, we call it DetroitMSACT
# You should notice some other differences in the code
# The geography was changed from "county" to "tract" because we want the smaller spatial unit
# variables, Survey, Year, Output stay the same (it will bring back same variables from same year in the same way)
# The state="" is new and here you could define the state.  We want the Detriot MSA so its only "MI", but if you wanted to add more, you could with state=c("MI", "OH", ...)
# The county= argument is also new.  here you can define the specific counties you would want to pull.  the c("county1name", "county2name", etc) comes from the MSA counties for 
SeattleMSACT <-get_acs(geography = "tract",  #Defining the spatial scale
                variables = Variables, #grabbing the variables we defined previously
                survey = "acs5",      # survey we are pulling is the ACS 5 year estimates
                year = 2020,   # End year for the 5 year estimates- 2016-2020
                output = "wide", # data format wide (cross sectional)
                state="WA", # Define which State you want to pull data from
                county= c("King","Kitsap", "Pierce", "Thurston", "Snohomish", "Skagit", "Island", "Mason"), # Defining the counties in the state that the data will come from
                geometry=T) # Pulls the spatial data information with it.


# First 10 records/observations return
head(SeattleMSACT)

# summary() provides an overview of the data
summary(SeattleMSACT)

# Lets clean up this data a little bit before we go too far
# Here we are just pulling out the columns we care about using the indexing dataframe[,c("column name","etc")]
Seattle<-SeattleMSACT[,c("GEOID", "NAME", "MHIE", "MHVE", "TotalpopE", "geometry")]
# Easy renaming of the variables
names(Seattle)[names(Seattle) =="MHIE"]<-"MHI"
names(Seattle)[names(Seattle) =="MHVE"]<-"MHV"
names(Seattle)[names(Seattle) =="TotalpopE"]<-"Totalpop"


### Setting up the color palletes
DpalMHI<-colorQuantile(palette = "viridis", domain=Seattle$MHI, n=10)
DpalMHV<-colorQuantile(palette = "RdYlBu", domain=Seattle$MHV, n=10)
Dpalpop<-colorQuantile(palette = "RdYlGn", domain=Seattle$Totalpop, n=10)


### The Leaflet map
## Named map
## this will take awhile
Seamap<-Seattle %>% # Data set you want to map and the '%>%' is a piping command which makes it keep moving forward 
  st_transform(crs = "+init=epsg:4326") %>% # Setting the projection of our simple feature spatial dataframe to match the webmapping application
  leaflet() %>%  # Leaflet function which puts this all in motion
  setView(lng = -122.33, lat = 47.61, zoom = 8)%>% # the lng and lat set the center point, while zoom chooses how zoomed in it is
  addTiles(group = "OSM (default)") %>%  #This sets the base map and the following addProviderTiles() are optional otehr basemaps
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto B") %>%
  addPolygons(group = "Total", #Add polygons is how we choose what we want to map.
              popup = paste("Census Tract: ", Detroit$NAME, "<br>", #Pop up provides the pop up information if we were to click on the county, paste() pastes all the text together
                            "Total Population: ", Detroit$Totalpop, "<br>", # <br> is html code for new line.   
                            "Median Household Income: ", "$", Detroit$MHI, "<br>",
                            "Median Home Value: ", "$", Detroit$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ Dpalpop(Totalpop)) %>% #Color palette or what the color scheme is.  You made these earlier and they need to match
  addLegend(position = "bottomleft", pal=Dpalpop, values= Seattle$Totalpop, #Legend for this polygon layer
            title="Percentile of Census Tract Total Population", group = "Total", opacity = 1) %>% 
  addPolygons(group = "MHI", #Next layer with the same set up as before.  We could add as many as the computer would let us. 
              popup = paste("Census Tract: ", Seattle$NAME, "<br>",
                            "Total Population: ", Seattle$Totalpop, "<br>",
                            "Median Household Income: ", "$", Seattle$MHI, "<br>",
                            "Median Home Value: ", "$", Seattle$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ DpalMHI(MHI)) %>%
  addLegend(position = "bottomright", pal=DpalMHI, values= Seattle$MHI, 
            title="Percentile of Census Tract Median Household Income", group = "MHI", opacity = 1) %>% 
  addPolygons(group = "MHV",
              popup = paste("Census Tract: ", Seattle$NAME, "<br>",
                            "Total Population: ", Seattle$Totalpop, "<br>",
                            "Median Household Income: ", "$", Seattle$MHI, "<br>",
                            "Median Home Value: ", "$", Seattle$MHV, "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ DpalMHV(MHV)) %>%
  addLegend(position = "bottomleft", pal=DpalMHV, values= Seattle$MHV, 
            title="Percentile of Census Tract Median Household Income", group = "MHV", opacity = 1) %>% 
  # Here we hide two of the groups so that only one shows on loading
  hideGroup("MHI") %>% # Hides the MHI layer so it doesn't all pop up at load
  hideGroup("MHV") %>% # Hides the MHV layer so it all doesn't pop up at the load
  addLayersControl(  #Gives you the option to turn on and off different basemaps or layers.
    baseGroups = c("OSM (default)", "Toner Lite", "Carto B"),
    overlayGroups = c("Total", "MHI", "MHV"),
    options = layersControlOptions(collapsed = FALSE)) 


Seamap # loads the map you just made, this will take awhile

# Saving the Html map
# first argument is the leaflet map, second is the file name and extension, and final is whether it is selfcontained
# The space needed for the self contained file is a lot which is why we reduced it down to just one layer.
saveWidget(Seamap, file="Seamap.html", selfcontained = T)




