

library(shiny)
library(tidyverse)
library(leaflet)
library(magrittr)
MASS <- read.csv("11-5 MASSCONTRIBUTIONS-csv.csv")

## According to Partys to build new dataframe
# calculate the total amount 

MASS.amount <- MASS %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) 

dmap <- spread(MASS.amount, party, party)
# three different party
dmap_stR <- dmap %>% filter(R==R)

dmap_stD <- dmap %>% filter(D==D)

dmap_stI <- dmap %>% filter(I==I)

###################################### Republican donor map

dmapR <- dmap_stR %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))


states <- map_data("state")
states %<>% dplyr::select(long,lat,group,order,region) %>% rename(state=region)

st_name <- unique(states$state)

st_abrev <- dmapR$state

st <- read_csv("states.csv")
st %<>% rename(state=st_name)

states <- left_join(states, st, by="state")

dmapR %<>% rename(st_abrev=state)

statesR <- left_join(states, dmapR, by="st_abrev")
statesR$Donors <- as.character(statesR$Donors)



states_centerR <- statesR %>% group_by(st_abrev) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = st_abrev)

states_dataR <- statesR %>% dplyr::select(st_abrev,Donations, Donors) %>% unique()


states_centerR <- left_join(states_centerR, states_dataR, by=c("st_abrev"))
########################################################################################



### Adjust location of state labels

states_centerR[states_centerR$st_abrev=="ID",]$long=-115.5
states_centerR[states_centerR$st_abrev=="MI",]$long=-84.7
states_centerR[states_centerR$st_abrev=="MI",]$lat=43
states_centerR[states_centerR$st_abrev=="VT",]$long=-72.7
states_centerR[states_centerR$st_abrev=="VT",]$lat=44.4
states_centerR[states_centerR$st_abrev=="NH",]$lat=43.6
states_centerR[states_centerR$st_abrev=="FL",]$long=-81.7
states_centerR[states_centerR$st_abrev=="LA",]$long=-92.5

# code for reviewing and changine location values
#
# states_center[states_center$st_abrev=="LA",]
# 
# states_center[states_center$st_abrev=="LA",]$long=-92.5
# states_center[states_center$st_abrev=="NH",]$lat=43.6
# 
# states_center[states_center$st_abrev=="ID",]

##########################

########################################################################################
########################################################################################

################################################################# 
#Democrats


dmapD <- dmap_stD %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))


states <- map_data("state")
states %<>% dplyr::select(long,lat,group,order,region) %>% rename(state=region)

st_name <- unique(states$state)

st_abrev <- dmapD$state



st <- read_csv("states.csv")
st %<>% rename(state=st_name)

states <- left_join(states, st, by="state")

dmapD %<>% rename(st_abrev=state)

statesD <- left_join(states, dmapD, by="st_abrev")

#############
statesD$Donors <- as.character(statesD$Donors)



states_centerD <- statesD %>% group_by(st_abrev) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = st_abrev)

states_dataD <- statesD %>% dplyr::select(st_abrev,Donations, Donors) %>% unique()


states_centerD <- left_join(states_centerD, states_dataD, by=c("st_abrev"))

########################################################################################



### Adjust location of state labels

states_centerD[states_centerD$st_abrev=="ID",]$long=-115.5
states_centerD[states_centerD$st_abrev=="MI",]$long=-84.7
states_centerD[states_centerD$st_abrev=="MI",]$lat=43
states_centerD[states_centerD$st_abrev=="VT",]$long=-72.7
states_centerD[states_centerD$st_abrev=="VT",]$lat=44.4
states_centerD[states_centerD$st_abrev=="NH",]$lat=43.6
states_centerD[states_centerD$st_abrev=="FL",]$long=-81.7
states_centerD[states_centerD$st_abrev=="LA",]$long=-92.5

# code for reviewing and changine location values
#
# states_center[states_center$st_abrev=="LA",]
# 
# states_center[states_center$st_abrev=="LA",]$long=-92.5
# states_center[states_center$st_abrev=="NH",]$lat=43.6
# 
# states_center[states_center$st_abrev=="ID",]






#####################################
#######################################################################################
#SHINY

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  titlePanel("Donation contribution to MA"),
  
  #Select Party
  sidebarPanel(
    radioButtons("Party", label = h3("Please choose the Party"),
                 choices = c("Republican","Democrat"), 
                 selected = "Republican"), "The map displays donation amounts from various states for each MA party candidates for the 2018 election. Colors indicate amount donated from each state. Hover over a state to view number of donors." ),
  
  #Creates Hover Coordinates
  mainPanel(
    div(
      style = "position:relative",
      plotOutput("scatterplot", 
                 hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
      uiOutput("hover_info")
    ),
    width = 7
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$scatterplot <- renderPlot({ 
    if (input$Party=="Republican") {
      ggplot(data = statesR) + 
        geom_polygon(aes(x = long, y = lat, fill = log10(Donations), 
                         group = group), color = "white") + 
        coord_fixed(1.3) + 
        
        labs(title = "Donors to Massachusetts Republicans",
             caption = "Number of Donors by State") +
        
        
        scale_fill_gradient("Donations", low="pink1", 
                            high="red4", 
                            breaks=c(3.4, 4.7, 6),
                            labels=c("low", "", "high") ) +
        
        
        
        theme(text = element_text(size=10),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              
              panel.background = element_rect(fill = "grey71", color="blue"))
    }
    else if (input$Party=="Democrat"){
      ggplot(data = statesD) + 
        geom_polygon(aes(x = long, y = lat, fill = log10(Donations), 
                         group = group), color = "white") + 
        coord_fixed(1.3) +
        
        labs(title= "Donors to Massachusetts Democrats", 
             caption = "Number of Donors by State") +
        
        scale_fill_gradient("Donations", low =  "lightblue1", 
                            high = "blue4",
                            breaks=c(4, 5.5, 7),
                            labels=c("low","","high")) +
        
        
        
        theme(text = element_text(size=10),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_rect(fill = "gray71", color="blue"))
    }
  } )
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    if (input$Party=="Republican") {
      point <- nearPoints(states_centerR, hover, threshold = 100, maxpoints = 1, addDist = TRUE)
    }
    else if (input$Party=="Democrat") {
      point <- nearPoints(states_centerD, hover, threshold = 100, maxpoints = 1, addDist = TRUE)
    }
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> State: </b>", point$st_abrev, "<br/>",
                    "<b> Donors: </b>", point$Donors, "<br/>")))
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

