#------------------------------------------------------------------------

#                   UI, or "User Interface" Script

# this script designs the layout of everything the user will see in this Shiny App
#------------------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(emojifont)


# make dashboard header
header <- dashboardHeader(
    title = paste0("Exploring volcano explosivity", emojifont::emoji("volcano")),
    titleWidth = 350 # since we have a long title, we need to extend width element in pixels
)


# create dashboard body - this is the major UI element
body <- dashboardBody(
    
    # make first row of elements (actually, this will be the only row)
    fluidRow(
        
        # make first column, 25% of page - width = 3 of 12 columns
        column(width = 3,
               
               
               # box 1 : input for selecting volcano type
               #-----------------------------------------------
               box(width = NULL, status = "primary",
                   title  = "Selection Criteria", solidHeader = T, 
                   
                   # Widget specifying the species to be included on the plot
                   checkboxGroupButtons(
                       inputId = "volcano_type",
                       label = "Volcano Type",
                       choices = c("Stratovolcano", "Shield","Cone","Caldera","Volcanic Field",
                                   "Complex", "Other", "Lava Dome","Submarine"),
                       checkIcon = list(
                           yes = tags$i(class = "fa fa-check-square", 
                                        style = "color: steelblue"),
                           no = tags$i(class = "fa fa-square-o", 
                                       style = "color: steelblue"))
                   ), # end checkboxGroupButtons
                   
                   sliderInput(inputId = "elevation",
                               label = "elevation_meters",
                               min = -2500, max = 7000,
                               value = c(0, 4000)),
                   br(),
                   
                   paste0(emojifont::emoji("rocket"))
                   
                   # space for your addition here:
                   #-------------------------------------------
                   # --- --- --- ---   HINT   --- --- --- --- 
                   # here, you will paste code for another Widget to filter volcanoes on the map.
                   # you'll need to paste code for some widget, name it, then call it at the top of the server page
                   # when we are filtering the selected_volcanoes() reactive object. 
                   
                   #  --- --- --- some suggestions: --- --- ---
                   # 1. slider bar to only show volcanoes with "population over xxx within 30 km"
                   # 2. slider input to show only volcanoes that have erupted in the last xxx years
                   # 3. slider input to only show volcanoes taller than xxx elevation
                   # 4. checkbox input to only show volcanoes in this evidence category
                   
                   # see available widgets here: http://shinyapps.dreamrs.fr/shinyWidgets/

                   
               ), # end box 1
               
               
               
               # box 2: ggplot of selected volcanoes by continent
               #------------------------------------------------
               box(width = NULL, status = "primary",
                   solidHeader = TRUE, collapsible = T,
                   title = "Volcanoes by Continent",
                   plotOutput("continentplot", # this calls to object continentplot that is made in the server page
                              height = 350)
               ) # end box 2
        ), # end column 1
         
        # second column - 75% of page (9 of 12 columns)
        column(width = 9,
               
               # Box 3: leaflet map
               box(width = NULL, background = "light-blue", 
                   leafletOutput("volcanomap", height = 760) 
                   # this draws element called volcanomap, which is created in the "server" tab
               ) # end box with map
        ) # end second column
        
    ) # end fluidrow
) # end body


# compile dashboard elements
dashboardPage(
    header = header,
    sidebar = dashboardSidebar(disable = TRUE), # here, we only have one tab, so we don't need a sidebar
    body = body
)

