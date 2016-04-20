###################################################
###################################################
################# ORANGE SPOT #####################
###################################################
###################################################



###################################################
################ SET-UP SECTION ###################
###################################################

#setwd("C:/Users/snhalbritter/Documents/GitHub/code-orange")

## LOAD LIBRARIES ##

library( RCurl )
library( shiny )
library( plyr )
library( dygraphs )
library( DT )
library( dplyr )
library( leaflet )

## Read in Data ##

dat <- readRDS("data/code.violations.final.rds")

#reading parcels in for hector for now
parcels <- read.csv( "https://raw.githubusercontent.com/lecy/code-orange/master/data/parcels.csv" )

## Map Set-Up ##



#lat.lon
lat.lon <- dat[ , c("lat","lon","Code") ] 
lat.lon <- na.omit( lat.lon )

#pop up 
violation.description <- dat$Code 


## Graphs Set-Up ##

## Complaints ##
# Drop dates before 2012
complaint.date <- dat$Violation.Date
post.2012 <- complaint.date > "2011-12-31"
dat.post.2012 <- dat[ post.2012 , ]
complaint.date <- dat.post.2012$Violation.Date

# this creates a factor for month-year
month.year <- cut( complaint.date, breaks="month" )

# this creates pretty names
month.year.name <- format( complaint.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat.post.2012$month.year <- month.year


complaint.types <- c("Property Maintenance-Int", 
                     "Trash/Debris-Private, Occ", 
                     "Bed Bugs", 
                     "Property Maintenance-Ext", 
                     "Building W/O Permit",
                     "Overgrowth: Private, Occ",
                     "Zoning Violations",
                     "Fire Safety",
                     "Fire Alarm",
                     "Unsafe Conditions",
                     "Infestation",
                     "Other (FPB)")

## Violations ##

vio.date <- dat.post.2012$Violation.Date

m.year <- cut( vio.date, breaks="month" )
m.y.name <- format( vio.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat.post.2012$m.year <- m.year

violation.types <- c("Section 305.3 - Interior surfaces",
                     "Section 27-72 (f) - Overgrowth",
                     "Section 27-72 (e) -Trash & Debris",
                     "325: General",
                     "Section 308.1 - Infestation",
                     "Section 27-32 (d) Protective coating for wood surfaces",
                     "252: General",
                     "Section 27-31 (c) Structural members",
                     "Section 304.13 - Window, skylight and door frames",
                     "Section 27-32 (b) Stairs, porches and railings"
)





## Property Ownership ##


### Data Table Explorer
explorer <- select(dat, Complaint.Type, Violation.Date, Comply.By.Date, 
                   Violation.Status, Complaint.Status, Owner, Nhood, LandUse, Address)


### mashed ###

by.ownerv <- group_by (dat, Owner)
by.ownerp <- group_by (parcels, Owner)

dist.ownerv <- summarise (by.ownerv, violations = n())
dist.ownerp <- summarise (by.ownerp, properties = n())

mashed <- merge (dist.ownerp, dist.ownerv, by = "Owner")

### Add # of Open Violations

only.open <- dat [ dat$Violation.Status == "Open" , ]
by.owneropen <- group_by (only.open, Owner)
dist.ownerop <- summarise (by.owneropen, open = n())

mashed <- merge (mashed, dist.ownerop, by = "Owner", all = TRUE)
mashed$open [is.na(mashed$open)] <- 0

### Add Acres Owned ###

acres.owned <- summarise (by.ownerp, Acres = sum(Acres))
mashed <- merge (mashed, acres.owned, by = "Owner")

### Add Square Feet Owned ###

sqft.owned <- summarise (by.ownerp, sqft = sum(SqFt))
mashed <- merge (mashed, sqft.owned, by = "Owner")


### Add Total Assessed Value ###

total.value <- summarise (by.ownerp, value = sum(AssessedVa))
mashed <- merge (mashed, total.value, by = "Owner")

colnames (mashed) <- c("Owner", "Properties", "Violations", "Open Violations", 
                       "Acres Owned", "Square Feet Owned", "Assessed Value")

mashed$`Acres Owned` <- round (mashed$`Acres Owned`, digits = 2)
mashed$`Square Feet Owned` <- round (mashed$`Square Feet Owned`, digits = 2)

### prop.mash ###

# Create Property Profiles

props <- select (dat, Address, LandUse, Owner, AssessedVa)

by.prop <- group_by (props, Address)
only.open <- dat [ dat$Violation.Status == "Open" , ]
by.propen <- group_by (only.open, Address)


### Add Violations
prop.v <- summarise (by.prop, violations = n())  

### Add Open Cases
dist.propen <- summarise (by.propen, open = n())  
prop.mash <- merge (prop.v, dist.propen, by = "Address", all = TRUE)

### Finalize Table
prop.mash <- merge (prop.mash, props, by = "Address")
prop.mash <- unique (prop.mash)

prop.mash$open [is.na(prop.mash$open)] <- 0

colnames (prop.mash) <- c("Property Address", "Violations", 
                          "Open", "Property Type", "Owner", "Assessed Value")

prop.mash$`Assessed Value` <- format (prop.mash$`Assessed Value`, digits = 2, scientific = FALSE)


rm (acres.owned, by.owneropen, by.ownerp, by.ownerv, by.prop, 
    by.propen, dist.ownerop, dist.ownerp, dist.ownerv, dist.propen,
    only.open, prop.v, props, sqft.owned, total.value)



# violation.date <- dat$Violation.Date

getData <- function( start.date, end.date )
{     these <- dat$Violation.Date >= start.date & dat$Violation.Date <= end.date
      return(  na.omit( lat.lon[ these, ] )  ) 
}




###################################################
################ SERVER SECTION ###################
###################################################

my.server <- function(input, output) 
{ 
  #static color vectors
  
  #color vector open closed
  col.vec.open.closed <- NULL
  col.vec.open.closed <- ifelse( dat$Violation.Status == "Open", "orange", NA)
  col.vec.open.closed <- ifelse( dat$Violation.Status == "Closed", "blanchedalmond", col.vec.open.closed  )
  
  #color vector severity
  col.vec.severity <- NULL
  col.vec.severity <- ifelse( dat$Severity == "1", "thistle", NA )
  col.vec.severity <- ifelse( dat$Severity == "2", "plum", col.vec.severity)
  col.vec.severity <- ifelse( dat$Severity == "3", "orchid", col.vec.severity)
  col.vec.severity <- ifelse( dat$Severity == "4", "mediumorchid", col.vec.severity)
  col.vec.severity <- ifelse( dat$Severity == "5", "darkorchid", col.vec.severity)
  col.vec.severity <- ifelse( dat$Severity == "FALSE", "whitesmoke", col.vec.severity)
  
  #color vector time between open closed - TOC
  col.vec.TOC <- NULL
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 0 & dat$TimeBetweenOC <= 60, "skyblue", NA )
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 60 & dat$TimeBetweenOC <= 123, "deepskyblue", col.vec.TOC)
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 123 & dat$TimeBetweenOC <= 186, "dodgerblue", col.vec.TOC)
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 186 & dat$TimeBetweenOC <=249, "royalblue", col.vec.TOC)
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 249 & dat$TimeBetweenOC <= 312, "navy", col.vec.TOC)
  col.vec.TOC <- ifelse( dat$TimeBetweenOC >= 312 & dat$TimeBetweenOC <= 400, "midnightblue", col.vec.TOC)
  col.vec.TOC <- ifelse( dat$TimeBetweenOC == 9999, "whitesmoke", col.vec.TOC)
  
  colvec <- reactive({
    
    if( input$color == "Severity" ) 
    {
      return(col.vec.severity)
    }  
    if(input$color == "Open/Closed")
    {
      return(col.vec.open.closed)
    }
    if( input$color == "Time to Close")
    {
      return(col.vec.TOC)
    }
  })
  
  output$mymap <- renderLeaflet({
    
    temp.dat <- getData( start.date=input$dateRange[[1]], end.date=input$dateRange[[2]] )
    
    # build base map on load
    syr.map <- leaflet(data=temp.dat ) %>% 
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-76.13, lat=43.03, zoom=13) %>%
      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
    
    syr.map <- addCircleMarkers( syr.map, lng = temp.dat$lon, lat = temp.dat$lat, 
                                 popup = temp.dat$Code, 
                                 radius=4, color=NA, fillColor=colvec() )
  })
  
  
  # COMPLAINTS PLOT
  
  output$complaints <- renderDygraph({
    
    dat.sub <- dat.post.2012[ dat.post.2012$Complaint.Type %in% input$show_comps , ]
    
    # Dropping months with zero complaints
    ncomps <- 0
    comp.checks <- as.data.frame(input$show_comps)
    ncomps <- length(input$comp.checks)
    
    # If there is no input, then set it to 1
    # if(ncomps == 0) { 
    #  ncomps = 1
    # }
    
    # Create chart for a subset of data
    complaint.sub <- tapply( dat.sub$Complaint.Type, dat.sub$month.year, length )
    complaint.sub[ is.na(complaint.sub) ] <- 0
    
    # Set maximum y limit
    complaint.sub.df <- as.data.frame(complaint.sub)
    max.ylim <- round_any((1.1*max(complaint.sub.df[ , 1 ] )), 10, f = ceiling)
    
    # If there is no max y limit, then set it to 1
    if(max.ylim == 0) { 
      max.ylim = 1
    }
    
    # Set pretty names
    pretty.names <- format( as.Date(names(complaint.sub)), "%b-%Y" )
    month.labels <- format( as.Date(names(complaint.sub)), "%b" )
    
    # If month has no complaints, then that month's label is null
    month.labels[ complaint.sub == 0 ] <- ""
    
    #xrange <- 0
    #yrange <- c(0:1) 
    
    # Plot Complaints
    dygraph(complaint.sub) %>% 
      dyRangeSelector()
    
  })
  
  
  # VIOLATIONS PLOT 
  
  output$violations <- renderDygraph({
    
    vdat.sub <- dat.post.2012[ dat.post.2012$Code %in% input$show_vios , ]
    
    nvios <- 0
    vio.checks <- as.data.frame(input$show_vios)
    nvios <- length(input$vio.checks)
    
    # Create chart for a subset of data
    violation.sub <- tapply( vdat.sub$Code, vdat.sub$m.year, length )
    violation.sub[ is.na(violation.sub) ] <- 0
    
    # Set maximum y limit
    violation.sub.df <- as.data.frame(violation.sub)
    max.ylim <- round_any((1.1*max(violation.sub.df[ , 1 ] )), 10, f = ceiling)
    
    # If there is no max y limit, then set it to 1
    if(max.ylim == 0) { 
      max.ylim = 1
    }
    
    # Set pretty names
    vpretty.names <- format( as.Date(names(violation.sub)), "%b-%Y" )
    vmonth.labels <- format( as.Date(names(violation.sub)), "%b" )
    
     # If month has no violations, then that month's label is null
    vmonth.labels[ violation.sub == 0 ] <- ""
    
    # Plot Violations
    dygraph(violation.sub) %>% 
      dyRangeSelector()
    
  })
  
  
  output$explorer <- DT::renderDataTable(DT::datatable({
    data <- explorer
    
    if (input$status != "All") {
      data <- data[data$Violation.Status == input$status,]
      
    }
    if (input$use != "All") {
      data <- data[data$LandUse == input$use,]
      
    }
    
    if (input$complaint != "All") {
      data <- data[data$Complaint.Status == input$complaint,]
      
    } 
    
    data
    
  }))
  
  output$summaries <- DT::renderDataTable(DT::datatable({
    data <- explorer    
    
    if (input$category == "owner") { 
      data <- mashed
      
    }
    
    if (input$category == "property") {   
      data <- prop.mash
      
    } 
    
    data
    
  }))
  
}



############################################
###### HTML Specs for Checkbox Widgets #####
###########################################

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 280px; width: 400px;
                                 -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 2;    /* Firefox */ 
                                 column-count: 2; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

###########################
### Widgets as Variables###
###########################

use.boxes <-
  list(h3("Select property type"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'use', 
                                   label    = NULL, 
                                   choices  = c("All", unique(as.character(dat$LandUse))),
                                   selected = "All",
                                   inline   = FALSE))) 

status.boxes <-
  list(h3("Choose Complaint Status"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'complaint', 
                                   label    = NULL, 
                                   choices  = c("All", unique(as.character(dat$Complaint.Status))),
                                   selected = "All",
                                   inline   = FALSE)))

dropdown <- 
  list(h3("Choose Violation Status"), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("status", 
                            "", 
                            c("All",unique(as.character(dat$Violation.Status))))))

summary.drop <- 
  list(h3("Choose Violation Status"), 
       tags$div(align = 'left', 
                class = 'dropdown',
                
                selectInput ("category",
                             "",
                             choices = list("By Owner" = "owner",
                                            "By Property" = "property"),
                             selected = "property")
       )
  )







###################################################
################### UI SECTION ####################
###################################################


my.ui <- navbarPage("Orangespot", id="nav", collapsible=T,
                    #Tab 1. MAP
                    tabPanel("Map",
                             tags$head(
                               includeScript("./www/analytics.js"),
                               tags$link(rel = "stylesheet", type = "text/css",
                                         href = "ion.rangeSlider.skinFlat.css"),
                               includeScript("./www/spin.min.js"),
                               includeCSS("./www/styles.css")
                              ),
                              
                              leafletOutput("mymap", width="100%", height="800" ), 
                              
                              
                              absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                                             width = 360, height = "auto",
                                             
                                             h2(),
                                             p(class="intro",
                                               strong("Orangespot"), " shows code violations in",
                                               "Syracuse, NY. Data from",
                                               a("SYR Neighborhood and Business Development.",
                                                 href="http://www.syrgov.net/Neighborhood_and_Business_Development.aspx",
                                                 target="_blank")),
                                             
                                             tabsetPanel(
                                               
                                               tabPanel("Controls",
                                                        
                                                        dateRangeInput('dateRange',
							      label = 'Select Date Range',
							      start = "2014-12-01", end = "2014-12-31"),
                                                        
                                                        selectInput("color", "Color by:",
                                                                    choices=list("Open/Closed", "Severity", "Time to Close")), #"Days to Comply")),
                                                        
                                                        hr(class="thin"),
                                                        p("See About Tab",
                                                          a("", href="",
                                                            target="_blank"),
                                                          HTML("&bull;"), "See the code on ",
                                                          a("github", href="http://github.com/lecy/code-orange",
                                                            target="_blank"),
                                                          class="foot")
                                               ),
                                               
                                               tabPanel("About",
                                                        p(class="topp", "Visualize code violations in the city of Syracuse",
                                                          "between 2012 and 2015 in this interactive map."
                                                        ),
                                                        p("Orangespot is written in ",
                                                          a("Shiny,", href="http://shiny.rstudio.com/", target="_blank"),
                                                          "a web application framework for the R language.",
                                                          "Maps are built with ",
                                                          a("leaflet.js", href="http://leafletjs.com/", target="_blank"),
                                                          "via the",
                                                          a("R language bindings,", href="https://rstudio.github.io/leaflet/",
                                                            target="_blank"),
                                                          "and using map data from",
                                                          a("Google Maps.", href="http://www.google.com/maps",
                                                            target="_blank")
                                                        ),
                                                        p("Project under development by ",
                                                          p("Susannah Bartlett & Rory Tikalsky under the guidance of Professor Jesse Lecy, Maxwell School of Citizenship and Public Affairs.",
                                                            HTML("&mdash;"),
                                                            "see the full code on ",
                                                            a("github", href="http://github.com/subartle/orangespot",
                                                              target="_blank"),
                                                            "or run locally with:"
                                                          ),
                                                          pre("shiny::runGitHub('subartle/orangespot')"),
                                                          hr(class="thin")
                                                        )
                                                        # end about panel
                                               ) )
                                             
                              ) # end of tabPanel "Map"
                    ), 
                    #Tab 2. Graphs
                    tabPanel("Graphs",
                             fluidPage(
                             fluidRow(
                               titlePanel( "  Syracuse Code Violation Trends, 2012-2016" ),
                               column( checkboxGroupInput("show_comps", 
                                                          label = h3("Complaint types:"), 
                                                          selected = "Bed Bugs",
                                                          choices = complaint.types
                               ),
                               title="Complaints Over Time", 
                               width=3 ),
                               column( dygraphOutput( "complaints" ),
                                       width=9 )),
                             
                             fluidRow(
                               column( checkboxGroupInput("show_vios",
                                                          label= h3("Violation types:"),
                                                          selected = "Section 308.1 - Infestation",
                                                          choices= violation.types),
                                       title="Violations Over Time",
                                       width=3 ),
                               column( dygraphOutput( "violations" ),
                                       width=9 ))
                    )),
                    
                    #Tab 3. Ownership
                    tabPanel("Ownership",
                             navbarPage("Ownership Dashboard",
                                        tabPanel("Owner and Property Profiles",
                                                 titlePanel("Summary Tables for Owners and Properties"),
                                                 fluidRow(
                                                   column (3,
                                                           summary.drop),
                                                   column (7,
                                                           
                                                           mainPanel(
                                                             DT::dataTableOutput("summaries"),
                                                             align = "left",
                                                             width = 12
                                                           )
                                                           
                                                   )
                                                 )
                                        ),
                                        
                                        tabPanel("Searchable",tweaks,
                                                 fluidRow(
                                                   
                                                   #### The Three Widgets ####   
                                                   column(3,
                                                          dropdown,
                                                          use.boxes,
                                                          status.boxes),
                                                   
                                                   
                                                   #### The Table ####
                                                   
                                                   column(6,
                                                          
                                                          
                                                          mainPanel(
                                                            DT::dataTableOutput("explorer"),
                                                            align = "center",
                                                            width = 12
                                                          )
                                                   )
                                                 )
                                        )
                    
)))

shinyApp(ui=my.ui, server=my.server)
