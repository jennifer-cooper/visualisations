#-------------------------------------------------------------------------
#GIS Visualisations
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#Libraries
library(sf)
library(geojsonsf)
library(sp)
library(zoo)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(shiny)
library(leaflet.extras)

#-------------------------------------------------------------------------

sf<- geojson_sf("Region_(December_2015)_Boundaries_ultra.geojson")
spdf<- as_Spatial(sf)

plot(spdf)
#-------------------------------------------------------------------------
#Read in COVID-19 case data

positive_cases <- read.csv('region_2021-09-15.csv')

#-------------------------------------------------------------------------
#Plot data

plot_positive_cases <- positive_cases

plot_positive_cases$date<- as.Date(plot_positive_cases$date)

plot_positive_cases %>% 
  filter(date>="2021-06-01") ->plot_positive_cases


ggplot(plot_positive_cases, aes(date, newCasesBySpecimenDate, colour = areaName)) +
  geom_line()

plot_positive_cases %>% 
  ggplot( aes(x=date, y=newCasesBySpecimenDate, group=areaName, color=areaName)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "weeks") +
  ylab("Number of positive cases") +
  xlab("Date") +
  labs(color='Region') -> p 

p + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) 




#-------------------------------------------------------------------------
#Keep simple and use data for last few months only
positive_cases %>% 
  filter(date>="2021-06-01") ->positive_cases

positive_cases$date<- as.Date(positive_cases$date)


#-------------------------------------------------------------------------
#Determine case rate for 7 day period

positive_cases %>% 
  group_by(areaName) %>% 
  arrange(date) %>% 
  mutate(weekrollavg = rollapplyr(newCasesBySpecimenDate, 7, sum, partial=T)) -> positive_cases

#-------------------------------------------------------------------------
#Left join region population to get per 100,000 population

pop_reg_est <- read.csv('population_region_estimates.csv')

#Change of the regions so they match for joining the data
pop_reg_est$region[pop_reg_est$region=="East"] <-"East of England"

#-------------------------------------------------------------------------
#Add population estimate
#Left join so wont add Wales from pop_reg_est

positive_cases %>% 
  left_join(pop_reg_est[c("region", "population_2020")], by = c("areaName"="region")) -> positive_cases
#-------------------------------------------------------------------------
#Case rates per 100,000 people for the 7-day period 
#Calculated this by dividing the 7-day total by the area population and multiplying by 100,000.

positive_cases %>% 
  mutate(weekroll_per100000 = round((weekrollavg/population_2020)*100000)) -> positive_cases

#-------------------------------------------------------------------------



#-------------------------------------------------------------------------
#University institution location markers
#-------------------------------------------------------------------------
location <- read.csv("LOCATION.csv") %>% select(UKPRN, LATITUDE, LONGITUDE)
ukprn_lookup<- read.csv("ukprn_lookup.csv")


#Group by ukprn and take the first row as multiple records
location %>% 
  group_by(UKPRN) %>% 
  filter(row_number()==1) ->location


ukprn_lookup %>% 
  inner_join(location, by="UKPRN") -> location_markers

#Can add on all sorts of other information if needed from the other Unistats datasets
#e.g.
numbers_insts<- read.csv("numbers_institutions amended.csv") %>% select(-"HE.provider")

#location_markers %>% 
#  left_join(numbers_insts, by="UKPRN") -> location_markers

#May want to do an inner_join here instead to keep only complete data examples of Unis


location_markers %>% 
  inner_join(numbers_insts, by="UKPRN") -> location_markers


#-------------------------------------------------------------------------
#Mapping Code
#-------------------------------------------------------------------------

#CV_region <- positive_cases %>% filter(areaName %in% spdf$rgn15nm)

summary(positive_cases$weekroll_per100000)

bins=c(0,200,400,600,800,1000) #to futureproof this make it inf

library("RColorBrewer")
display.brewer.pal(n = 5, name = 'BuPu')

cv_pal<- colorBin("BuPu", domain = positive_cases$weekroll_per100000, bins = bins, reverse=F)


#-------------------------------------------
#Do not use this in the end

Region_popup <- paste0("<strong>LSOA Name: </strong>",
                     positive_cases$areaName,
                     "<br><strong> Case rate per 100,000 people for 7–day period: </strong>",
                     positive_cases$weekroll_per100000)

#-------------------------------------------
#Basic example to test
#-------------------------------------------
JiscIcon <- makeIcon(
  iconUrl = "https://uxd.jisc.ac.uk/wp-content/themes/uxdtheme/assets/jisc-logo.svg",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
)

jisc_locations <- c("Bristol", "Belfast", "Cardiff Bay", "Cheltenham", "Didcot", "Glasgow", "London", "Manchester")
lng<- c(-2.5887163863787888, -5.966229786858929, -3.1578831023650396, -2.0788197618655433, -1.3106936311952653, -4.259398130970776, -0.10902414284008903, -2.24216785993317)
lat<- c(51.44960071837351, 54.6010033966976, 51.467221538800324, 51.8987101649444, 51.57746625975382, 55.86140217950865, 51.51522358900154, 53.47538051846536)

jisc<- data.frame(jisc_locations, lng, lat)

spdf %>% 
  leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addPolygons(stroke = T, 
              weight = 1,
              color = "black",
              fillOpacity = 0, 
              smoothFactor = 0.5) %>% 
  addMarkers(lng = location_markers$LONGITUDE,
             lat = location_markers$LATITUDE,
             label = paste0(location_markers$name, " (", format(location_markers$Total, big.mark=",", scientific = FALSE), ")")) %>% 
  addMarkers(jisc$lng, jisc$lat, icon = JiscIcon, label = paste0("JISC Location: ", jisc$jisc_locations)) %>% 
  setView(lng = -1.228838, lat = 52.76846, zoom = 6) %>% 
  addResetMapButton()


#-------------------------------------------
#make basemap just outlines of all Regions and plots where the Universities are
#-------------------------------------------
basemap <- 
  spdf %>% 
  leaflet() %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  #Adding Polygons
  addPolygons(stroke = T, 
              weight = 1,
              color = "black",
              fillOpacity = 0, 
              smoothFactor = 0.5) %>%
  addLegend("topright", pal = cv_pal, values = ~positive_cases$weekroll_per100000,
            title = "Case rate per 100,000 people for 7–day period",
            opacity = 1) %>% 
  
  #Add user controls
  addLayersControl(baseGroups = c("OSM", "Carto"))%>% 
  
  #Add additional controls
  addSearchOSM() %>%  #I have added these additionally
  addResetMapButton() %>% 
  setView(lng = -1.228838, lat = 52.76846, zoom = 6)


#-------------------------------------------


#-------------------------------------------

#Defining dates to use (latest and earliest week date):
cv_min_date<-as.Date(min(positive_cases$date), "%Y-%m-%d")
current_date<- as.Date(max(positive_cases$date), "%Y-%m-%d")


#-------------------------------------------
ui<- fluidPage(
  theme=shinytheme("darkly"),
  themeSelector(),
  titlePanel("Higher Education Institution Location and Current COVID-19 Case Rate"),
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("plot_date", 
                  label = h5("Select mapping date"), 
                  min = as.Date(cv_min_date,"%Y-%m-%d"), 
                  max = as.Date(current_date,"%Y-%m-%d"), 
                  value = as.Date(current_date), 
                  timeFormat = "%d %b",  
                  animate=animationOptions(interval = 3000, loop = FALSE))
    ),
    
    mainPanel(
      
      leafletOutput("mymap", height="800")
    )
  )
)

#--------------------------------------------------------------------------------------------------


server = function(input, output, session) {
  
  reactive_db = reactive({ 
    positive_cases %>% filter(date == input$plot_date)   #takes input date by user, subsets data to just that date
  }) 
  
  reactive_db_2 = reactive({ 
    reactive_db() %>% filter(areaCode %in% spdf$rgn15cd)  #Takes the date subsetted data and takes just the regions present in spatial data
  }) 
  
  reactive_polygons = reactive({ 
    spdf[spdf$rgn15cd %in% reactive_db_2()$areaCode, ] }) #Takes the spatial data for the date subsetted data for 563 lsoas of interest
  
  output$mymap <- renderLeaflet({  
    basemap 
  }) 
  
  observeEvent(input$plot_date, { 
    leafletProxy("mymap") %>%  
      #clearMarkers() %>%
      clearShapes() %>% 
      addPolygons(data = reactive_polygons(),
                  stroke = T, 
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1, 
                  smoothFactor = 0.5, 
                  fillColor = ~cv_pal(reactive_db_2()$weekroll_per100000),
                  popup =  paste0("<strong>Region Name: </strong>",
                                  reactive_db_2()$areaName,
                                  "<br><strong>Case rate per 100,000 for 7 day period: </strong>",
                                  reactive_db_2()$weekroll_per100000),
                  highlight = highlightOptions(weight = 5, color = "white")) %>% 
    addCircleMarkers(lng = location_markers$LONGITUDE,
                     lat = location_markers$LATITUDE, 
                     radius = 2, 
                     color = "#33CCFF",
                     label = paste0(location_markers$name, " (", format(location_markers$Total, big.mark=",", scientific = FALSE), ")")) 
  })
}


#--------------------------------------------------------------------------------------------------

shinyApp(ui=ui, server=server)

#--------------------------------------------------------------------------------------------------





