###################################################
###################################################
################# ORANGE SPOT #####################
###################################################
###################################################



###################################################
################ SET-UP SECTION ###################
###################################################

#### LOAD LIBRARIES ###

library( RCurl )
library( shiny )
library( plyr )
library( dygraphs )
library( DT )
library( dplyr )
library( leaflet )

#### DATA STEPS ####

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")
code.severity <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/Severity.csv")
code.violations <- merge(code.violations, code.severity, by.x = "Code", by.y = "Row.Labels", all.x=T )
code.violations$Severity[is.na(code.violations$Severity)] <- "FALSE"  #this makes the NA in severity 0

#Convert to time class
code.violations$Violation.Date <- as.Date(code.violations$Violation.Date,format = "%m/%d/%Y")
code.violations$Complaint.Close.Date <- as.Date(code.violations$Complaint.Close.Date, format = "%m/%d/%Y")
code.violations$Complaint.Date <- as.Date(code.violations$Complaint.Date, "%m/%d/%Y")
code.violations$Comply.By.Date <- as.Date(code.violations$Comply.By.Date, format = "%m/%d/%Y")

#new variables representing time between dates and getting rid of negative amounts (due to incorrect data entry)
code.violations <- mutate(code.violations, TimeBetweenOCB = code.violations$Comply.By.Date - code.violations$Violation.Date)
code.violations$TimeBetweenOCB[code.violations$TimeBetweenOCB < 0 ] <- NA
code.violations <- mutate(code.violations, TimeBetweenCV = (code.violations$Complaint.Date - code.violations$Violation.Date) )
code.violations$TimeBetweenCV[code.violations$TimeBetweenCV < 0 ] <- NA
code.violations <- mutate(code.violations, TimeBetweenOC = (code.violations$Complaint.Close.Date - code.violations$Violation.Date))
code.violations$TimeBetweenOC[code.violations$TimeBetweenOC < 0 ] <- NA
code.violations$TimeBetweenOC[is.na(code.violations$TimeBetweenOC)] <- 9999  #this makes the NA in severity 0

#lat.lon
lat.lon <- code.violations[ 1:50000, c("lat","lon") ] # sample for dev purposes
lat.lon <- na.omit( lat.lon )

#pop up 
violation.description <- code.violations$Code 



# Funtimes with dygraphs






#~#~#~#~#~##~#~#
###Need to confirm with Jacqui/Ben that this is the right data source, but the code works with this link right now.##
dat <- read.csv("https://raw.githubusercontent.com/jacquibuchanan/SyrCode/master/SyracuseCodeViolations.csv")
#~#~#~#~#~##~#~#





# Drop dates before 2012
complaint.date <- as.Date( dat$Violation.Date, "%m/%d/%Y" )
pre.2012 <- complaint.date > "2011-12-31"
dat <- dat[ pre.2012 , ]

complaint.date <- as.Date( dat$Violation.Date, "%m/%d/%Y" )

# this creates a factor for month-year
month.year <- cut( complaint.date, breaks="month" )

# this creates pretty names
month.year.name <- format( complaint.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat$month.year <- month.year

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

# I tried to keep the syntax as close to yours as possible for the corresponding violation data

dat.v <- read.csv("https://raw.githubusercontent.com/jacquibuchanan/SyrCode/master/SyracuseCodeViolations.csv")

# Drop dates before 2012
vio.date <- as.Date( dat.v$Violation.Date, "%m/%d/%Y" )
vpost.2012 <- vio.date > "2011-12-31"
dat.v <- dat.v[ vpost.2012, ]

vio.date <- as.Date( dat.v$Violation.Date, "%m/%d/%Y" )

m.year <- cut( vio.date, breaks="month" )
m.y.name <- format( vio.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat.v$m.year <- m.year

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





##Start Most Wanted Section##



dat.3 <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")

parcels <- read.csv( "https://raw.githubusercontent.com/hectorlca/Code-Violations/master/data/parcels.csv" )

cv <- dat.3[ !( is.na(dat.3$lon) | is.na(dat.3$lat)) , ]

merged <- merge( cv, parcels, by.x="Identifier", by.y="SBL" )

### Data Table Explorer



explorer <- select(merged, Complaint.Type, Violation.Date, Comply.By.Date, 
                   Violation.Status, Complaint.Status, Owner, Nhood, LandUse, Address)


### mashed ###

by.ownerv <- group_by (merged, Owner)
by.ownerp <- group_by (parcels, Owner)

dist.ownerv <- summarise (by.ownerv, violations = n())
dist.ownerp <- summarise (by.ownerp, properties = n())

mashed <- merge (dist.ownerp, dist.ownerv, by = "Owner")

### Add # of Open Violations

only.open <- merged [ merged$Violation.Status == "Open" , ]
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

props <- select (merged, Address, LandUse, Owner, AssessedVa)

by.prop <- group_by (props, Address)
only.open <- merged [ merged$Violation.Status == "Open" , ]
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




###################################################
################ SERVER SECTION ###################
###################################################

#0. Combine

my.server <- function(input, output) 
{ 
  #static color vectors
  
  #color vector open closed
  col.vec.open.closed <- NULL
  col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Open", "orange", NA)
  col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Closed", "blanchedalmond", col.vec.open.closed  )
  
  #color vector severity
  col.vec.severity <- NULL
  col.vec.severity <- ifelse( code.violations$Severity == "1", "thistle", NA )
  col.vec.severity <- ifelse( code.violations$Severity == "2", "plum", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "3", "orchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "4", "mediumorchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "5", "darkorchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "FALSE", "whitesmoke", col.vec.severity)
  
  #color vector time between open closed - TOC
  col.vec.TOC <- NULL
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 0 & code.violations$TimeBetweenOC <= 60, "skyblue", NA )
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 60 & code.violations$TimeBetweenOC <= 123, "deepskyblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 123 & code.violations$TimeBetweenOC <= 186, "dodgerblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 186 & code.violations$TimeBetweenOC <=249, "royalblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 249 & code.violations$TimeBetweenOC <= 312, "navy", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 312 & code.violations$TimeBetweenOC <= 400, "midnightblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC == 9999, "whitesmoke", col.vec.TOC)
  
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
    
    # build base map on load
    syr.map <- leaflet(data=lat.lon ) %>% 
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-76.13, lat=43.03, zoom=13) %>%
      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
    
    syr.map <- addCircleMarkers( syr.map, lng = lat.lon$lon, lat = lat.lon$lat, col=colvec(), popup = violation.description )
  })
  
  
  # COMPLAINTS PLOT
  
  output$complaints <- renderDygraph({
    
    dat.sub <- dat[ dat$Complaint.Type %in% input$show_comps , ]
    
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
    
    vdat.sub <- dat.v[ dat.v$Code %in% input$show_vios , ]
    
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
                                   choices  = c("All", unique(as.character(merged$LandUse))),
                                   selected = "All",
                                   inline   = FALSE))) 

status.boxes <-
  list(h3("Choose Complaint Status"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'complaint', 
                                   label    = NULL, 
                                   choices  = c("All", unique(as.character(merged$Complaint.Status))),
                                   selected = "All",
                                   inline   = FALSE)))

dropdown <- 
  list(h3("Choose Violation Status"), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("status", 
                            "", 
                            c("All",unique(as.character(merged$Violation.Status))))))

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
                                includeScript("analytics.js"),
                                tags$link(rel = "stylesheet", type = "text/css",
                                          href = "ion.rangeSlider.skinFlat.css"),
                                includeScript("spin.min.js"),
                                includeCSS("styles.css")
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
                                                        
                                                        selectInput("color", "Colour by:",
                                                                    choices=list("Open/Closed", "Severity", "Time to Close")), #"Days to Comply")),
                                                        
                                                        hr(class="thin"),
                                                        p("See About Tab",
                                                          a("", href="",
                                                            target="_blank"),
                                                          HTML("&bull;"), "See the code on ",
                                                          a("github", href="http://github.com/subartle/orangespot",
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
                                                          choices= violation.types),
                                       title="Violations Over Time",
                                       width=3 ),
                               column( dygraphOutput( "violations" ),
                                       width=9 ))
                    )),
                    
                    #Tab 3. Most Wanted
                    tabPanel("Ownership",
                             navbarPage("Most Wanted Dashboard",
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
                                                 titlePanel("Most Wanted List"),
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
