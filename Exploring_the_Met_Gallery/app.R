library(shiny)
library(tidyverse)
library(styler)
library(ggplot2)
library(viridis)
library(shinythemes)
library(shinyWidgets)

# Import Met Gallery Data
df <- read_csv("MetObjects.csv")

# Import country centroid data
centroid_all <- read_csv("country_centroids_all.csv")

# tidy centroid data
centroid <- centroid_all %>%
  select("SHORT_NAME", "LAT", "LONG") %>% #only choosing lat/long and country name
  dplyr::rename("Country" = SHORT_NAME) #rename to match small_df

# tidy Met Gallery Data
small_df <- df %>%
  select("Country", "Object_Begin_Date", "Medium", "Credit_Line") %>%
  left_join(centroid) %>% #join with centroid data to add lat/long
  drop_na("Country") %>% #remove where there is no country given
  mutate(acquisition = case_when( #making new column with more general acquisition methods
    str_detect(str_to_lower(Credit_Line), "gift") ~ "gift",
    str_detect(str_to_lower(Credit_Line), "bequest") ~ "gift",
    str_detect(Credit_Line, "Crosby Brown") ~ "gift",
    str_detect(str_to_lower(Credit_Line), "loan") ~ "loan",
    str_detect(str_to_lower(Credit_Line), "fund") ~ "fund",
    str_detect(str_to_lower(Credit_Line), "purchase") ~ "purchase"
  )) %>% 
  drop_na("acquisition") 

country_df <- small_df %>%
  drop_na("LAT")

#make list of countries for dropdown menu in app
country_df <- small_df %>%
  drop_na("LAT", "LONG") #removing where there is not lat/long data
countries <- sort(c(unique(country_df$Country))) #getting list of distint values of countries

#make lists of artists for dropdown menu in app
Artist_names <- sort(c("Berthe_Morisot", "Lilla_Cabot_Perry", "Mary_Cassatt", "Louise_Catherine_Breslau", "Cecilia_Beaux"))

#Function to get color palette from .jpg
palettebuildr <- function(pathToJPEG = "logo.jpg", ncols = 3, dist.method = "euclidian", clust.method = "complete") {
  require(jpeg)
  # Read in the jpeg file
  img <- readJPEG(pathToJPEG)
  
  # create a grid from which extract the colors
  xgrid <- ceiling(seq(1, dim(img)[1], length.out = 50))
  ygrid <- ceiling(seq(1, dim(img)[2], length.out = 50))
  pixels <- expand.grid(xgrid, ygrid)

  # Get the red/green/blue values for each pixel
  pixels$redVals <- apply(pixels, 1, function(x) {
    return(img[x[1], x[2], ][1])
  })
  pixels$greenVals <- apply(pixels, 1, function(x) {
    return(img[x[1], x[2], ][2])
  })
  pixels$blueVals <- apply(pixels, 1, function(x) {
    return(img[x[1], x[2], ][3])
  })

  # Get the euclidian distances of the colour values (how similar they are to one another)
  distMat <- dist(pixels[, 3:5], method = dist.method)

  # Use hierarchical clustering to group the colours into n groups
  x <- hclust(distMat, method = clust.method)

  # Assign each pixel to one of k-groups based on clustering
  pixels$groups <- cutree(x, k = ncols)

  # Take average red/green/blue values for the whole group
  redGroup <- tapply(pixels$redVals, pixels$groups, mean)
  greenGroup <- tapply(pixels$greenVals, pixels$groups, mean)
  blueGroup <- tapply(pixels$blueVals, pixels$groups, mean)

  # Create a dataframe of colours to return
  groupCols <- data.frame(gp = 1:ncols, red = redGroup, green = greenGroup, blue = blueGroup)
  groupCols$hexCode <- rgb(red = groupCols$red, green = groupCols$green, blue = groupCols$blue, alpha = 1, maxColorValue = 1)

  return(groupCols)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("journal"), #setting the theme
  titlePanel("Exploring the Met Gallery"), #main title
  tabsetPanel(
    tabPanel("Introduction", #first tab for introduction
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          align = "center",
          p("Data from the Met Gallery Open Access Initiative"),
          img(src = "the_met.png", height = 100, width = 100), #image from met gallery
          br(),
          br(),
          img(src = "oai.png", height = 100, width = 100), #image for met's open access initiative
          br(),
          br(),
          p("This project is not affiliated with the Metropolitan Museum of Art.") 
        ),
        mainPanel(
          br(),
          p("Using the Metropolitan Museum of Art Open Access Initiative, this Shiny App will allow you to explore the Met Collection, and even use jpegs of paintings to create a color palette to inspire your own work!"),
          p(
            "for more information, visit:",
            a("https://github.com/metmuseum/openaccess",
              href = "https://github.com/metmuseum/openaccess"
            )
          ),
          h2("Features"),
          p("- Explore the objects in the gallery by year, country, and method of acquisition"),
          p("- Make a color palette from painting in the gallery")
        )
      )
    ),
    tabPanel("By Country and Year", #second tab
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          p("Use the slider below to select the year that an object was created.  The map will automatically update to show where the objects in the time range were created"),
          chooseSliderSkin(skin = c("Flat")), #changing the slider tyle
          sliderInput( #making a slider input for user to select year
            inputId = "year_range",
            label = "Year:",
            min = -1500,
            max = 2018,
            value = c(0, 2018),
            sep = ""
          ),
          br(),
          helpText("Country centroid data from Frank Donnelly (https://atcoordinates.info/resources/)")
        ),
        mainPanel(
          align = "center",
          plotOutput("plot2") #ouputs a plot
        )
      )
    ),
    tabPanel("Acquisition by Country", #third tab
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          p("Choose a country below and discover how the museum acquired their objects from that country, be it as a gift or loan, or through a fund or purchase"),
          selectInput("country", "Select Country", countries) #dropdown menu
        ),
        mainPanel(
          align = "center",
          plotOutput("plot1") #outputs a plot
        )
      )
    ),
    tabPanel("Color Palette", #fourth tab
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          p("Select an artist below to display a sample painting and create a color palette"),
          selectInput("artist", "Artist:", choices = Artist_names), #dropdown menu
          br(),
          helpText("Color palette function from Andrea Cirillo"),
          helpText("https://datascienceplus.com/author/andreacirilloac/")
        ),
        mainPanel(
          align = "center",
          br(),
          uiOutput("img1"), #first output is the image
          plotOutput("img2") #second output is the color palette
        )
      )
    ),
    tabPanel("Code", #fifth tab for code
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(),
        mainPanel(
          includeMarkdown("Shiny_App.rmd") #just including code in an R Markdown file
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #for second tab, taking year inputs and filtering the df
  dataset <- reactive({
    new_df <- small_df %>%
      filter(Object_Begin_Date >= input$year_range[1]) %>% #using first term of slider input
      filter(Object_Begin_Date <= input$year_range[2]) %>% #using second term of slider input
      group_by(Country) %>% #group by country
      mutate(n= n()) %>% #getting country frequency to scale the geom_points on the map
      distinct(Country, .keep_all=TRUE) #getting just one value for each country, keeping all other data
  })
  
  #brining in a world map from base R
  world <- map_data("world")

  #making a world map using changes to df as listed above
  output$plot2 <- renderPlot({
    ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray", alpha = 0.3) + #making base map of the world in gray
      geom_point(data = dataset(), aes(x = LONG, y = LAT, size = n, color = n), alpha = 0.9) + #adding the points for each country
      scale_color_viridis(trans = "log", option = "magma",  name = "Number of Objects") + #setting continuous color based on value
      scale_size_continuous(range = c(1, 12), name = "Number of Objects") + #setting continuous size based on value
      theme_void() #changing theme
  })

  plot1 <- reactive({ #reactive output for acquissition pie chart
    acquisition_df <- small_df %>%
      filter(Country == input$country) %>% #filtering by the user's country input
      drop_na("acquisition", "LAT") %>% #dropping missing data
      group_by(acquisition) %>% #grouping by country
      count() %>% #getting counts
      ungroup() %>% # ungroup
      mutate(per = `n` / sum(`n`)) %>% #getting frequencies
      arrange(desc(acquisition)) #arranging so countries are in alphabetical order
  })

  output$plot1 <- renderPlot({
    ggplot(plot1()) + #making pie chart in ggplot
      geom_bar(aes(x = "", y = per, fill = acquisition), stat = "identity", width = 1) +
      scale_fill_manual(values=c("gift" = "#F39F9E", #filling by acquisition method, custom colors
                                 "fund" = "#C8C8C8",
                                 "loan" = "#7B9DBB",
                                 "purchase" = "#99956B")) +
      coord_polar("y", start = 0) + #changing from bar plot to pie chart
      theme_void() #changing theme
  })

  output$img1 <- renderUI({ #for third tab, taking the input and then returning the correct jpg
    if (input$artist == "Berthe_Morisot") {
      img(height = 400, src = "Berthe_Morisot.jpg")
    }
    else if (input$artist == "Lilla_Cabot_Perry") {
      img(height = 400, src = "Lilla_Cabot_Perry.jpg")
    }
    else if (input$artist == "Mary_Cassatt") {
      img(height = 400, src = "Mary_Cassatt.jpg")
    }
    else if (input$artist == "Louise_Catherine_Breslau") {
      img(height = 400, src = "Louise_Catherine_Breslau.jpg")
    }
    else if (input$artist == "Cecilia_Beaux") {
      img(height = 400, src = "Cecilia_Beaux.jpg")
    }
  })

  output$img2 <- renderPlot({ #creating a pie chart using the function colorPalette
    colorPalette <- palettebuildr((paste(input$artist, ".jpg", sep = "")), ncols = 5)
    pie(rep(1, nrow(colorPalette)),
      col = colorPalette$hexCode,
      labels = substr(colorPalette$hexCode, 1, 7), border = "white"
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
