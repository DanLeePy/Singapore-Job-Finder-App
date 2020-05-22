library(shiny)
library(shinydashboard)
library(shiny.semantic)
library(semantic.dashboard)
library(ggplot2)
library(ggmap)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(tibble)
library(wordcloud)
library(flexdashboard)

register_google(key = "AIzaSyDebvQ96bhsQRVBV9riGE7PXlJMgFiMSh4")
load("gauge_score.rda")
jobstreet_data <- read.csv('final_v2.csv')
jobstreet_data[['post_datetime']] <- as.Date(jobstreet_data[['post_date']], format='%d-%B-%Y')
jobstreet_data[['close_datetime']] <- as.Date(jobstreet_data[['close_date']], format='%d-%B-%Y')
jobstreet_industries <- list('Banking'= 'Banking','Semiconductor'= 'Semiconductor','Vehicle'= 'Vehicle','Sports'= 'Sports',
                             'Food & Beverage'= 'Food & Beverage','Production'= 'Production','Real Estate'= 'Real Estate',
                             'Textiles'= 'Textiles','Grooming'= 'Grooming','Architectural Services'= 'Architectural Services',
                             'Pharmaceutical'= 'Pharmaceutical','Plastic'= 'Plastic','Tourism'= 'Tourism',
                             'Wafer Fabrication'= 'Wafer Fabrication','IT-Enabled Services'= 'IT-Enabled Services',
                             'Hotel'= 'Hotel','Social Services'= 'Social Services','Interior Designing'= 'Interior Designing',
                             'MICE'= 'MICE','Equipment'= 'Equipment','Beauty'= 'Beauty','Call Center'= 'Call Center',
                             'Plantation'= 'Plantation','Paper'= 'Paper','Consulting'= 'Consulting',
                             'Automobile'= 'Automobile','Restaurant'= 'Restaurant','Others'= 'Others','Fisheries'= 'Fisheries',
                             'Education'= 'Education','Apparel'= 'Apparel','Property'= 'Property','Hospitality'= 'Hospitality',
                             'Fibre'= 'Fibre','Manufacturing'= 'Manufacturing','Travel'= 'Travel','Printing'= 'Printing',
                             'Computer'= 'Computer','Marine'= 'Marine','Fashion'= 'Fashion','Transportation'= 'Transportation',
                             'Law Enforcement'= 'Law Enforcement','Advertising'= 'Advertising','Agricultural'= 'Agricultural',
                             'Garment'= 'Garment','Accounting'= 'Accounting','Insurance'= 'Insurance','Defence'= 'Defence',
                             'Building'= 'Building','Library'= 'Library','Promotion'= 'Promotion',
                             'Consulting (IT, Science, Engineering & Technical)'= 'Consulting (IT, Science, Engineering & Technical)',
                             'Petroleum'= 'Petroleum','Aquaculture'= 'Aquaculture','Clinical research'= 'Clinical research',
                             'Polymer'= 'Polymer','Merchandise'= 'Merchandise','Tax Services'= 'Tax Services','Telecommunication'= 'Telecommunication',
                             'Machinery'= 'Machinery','Human Resources Management'= 'Human Resources Management',
                             'Information Technology (Hardware)'= 'Information Technology (Hardware)','Government'= 'Government',
                             'Information Technology (Software)'= 'Information Technology (Software)','Pesticides'= 'Pesticides',
                             'Heavy Industrial'= 'Heavy Industrial','Legal'= 'Legal','Event management'= 'Event management',
                             'General & Wholesale Trading'= 'General & Wholesale Trading','Fertilizers'= 'Fertilizers','Safety'= 'Safety',
                             'Construction'= 'Construction','Jewellery'= 'Jewellery','BioTechnology'= 'BioTechnology',
                             'Consumer Products'= 'Consumer Products','Electrical & Electronics'= 'Electrical & Electronics',
                             'Mining'= 'Mining','Health'= 'Health','Power'= 'Power','Aviation'= 'Aviation','Poultry'= 'Poultry',
                             'Science & Technology'= 'Science & Technology','Medical'= 'Medical','Aerospace'= 'Aerospace',
                             'Museum'= 'Museum','Non-Profit Organisation'= 'Non-Profit Organisation','Financial Services'= 'Financial Services',
                             'Repair & Maintenance Services'= 'Repair & Maintenance Services','Retail'= 'Retail','Publishing'= 'Publishing',
                             'Logistics'= 'Logistics','Wood'= 'Wood','Gems'= 'Gems','Automotive Ancillary'= 'Automotive Ancillary',
                             'Tyres'= 'Tyres','Media'= 'Media','Chemical'= 'Chemical','Environment'= 'Environment','Arts'= 'Arts',
                             'Securities'= 'Securities','Design'= 'Design','Audit'= 'Audit','Catering'= 'Catering',
                             'Consulting (Business & Management)'= 'Consulting (Business & Management)','Utilities'= 'Utilities',
                             'Journalism'= 'Journalism','Marketing'= 'Marketing','Engineering'= 'Engineering','Security'= 'Security',
                             'Healthcare'= 'Healthcare','Rubber'= 'Rubber','Fitness'= 'Fitness','Exhibitions'= 'Exhibitions',
                             'Entertainment'= 'Entertainment','FMCG'= 'FMCG','Airline'= 'Airline','Stockbroking'= 'Stockbroking')

header = dashboardHeader(dropdownMenuOutput("dropdown_user"),
                         dropdownMenu(icon = uiicon("red warning sign"),
                                      notificationItem("You have 2 new job offers!", color = "red")),
                         color = "blue", title = HTML("<font color='red'> Dashboard Demo </font>"), inverted=TRUE)

sidebar = dashboardSidebar(
  size = "thin", color = "black", inverted=TRUE,
  sidebarMenu(
    menuItem(tabName = "home", "Home", icon = icon("home")),
    menuItem(tabName = "statistics", "Overview", icon = icon("line chart")),
    menuItem(tabName = "maps", "Maps", icon = icon("map")),
    menuItem(tabName = "resume", "Resume Score", icon = icon("address card")),
    menuItem(tabName = "search", "Search for a Job", icon = icon("search")))
)

body = dashboardBody(
  tags$img(
    src = "teamwork.jpeg",
    height="800px", width="1200px",
    style = 'position: absolute'
  ),
  tabItems(
    selected = 1,
    tabItem(
      tabName = "home",
      fluidRow(box(column(width=5,h1("Home Page", style = "color: #4d3a7d;")),
                   column(width=5,h2("Welcome to our R Shiny App!")))
        ),
      fluidRow(box(title='Who we are', color = "blue", ribbon = FALSE,
                   title_side = "top left", solidHeader=TRUE,width=6,htmlOutput("intro")),
               box(title='What we do', color = "blue", ribbon = FALSE,
                   title_side = "top left", solidHeader=TRUE,width=6,htmlOutput("about")))
    ),
    tabItem(
      tabName = "statistics",
      fluidRow(box(column(width=5,h1("Job Statistics", style = "color: #4d3a7d;")),
                   column(width=5,h2("Here are all posted Jobs"))))
      ,
      fluidRow(column(width=9,
                      box(title='Posted Trend', color = "blue", ribbon = FALSE,
                          title_side = "top left", solidHeader=TRUE,
                          mainPanel(withSpinner(plotlyOutput("trendPlot")))),
                      box(title='Aggregate Trend', color = "blue", ribbon = FALSE,
                          title_side = "top left", solidHeader=TRUE,
                          mainPanel(withSpinner(plotlyOutput("aggPlot"))))),
               column(width=5,box(width=4,selectInput(inputId = "company_industry3",label=HTML("<b>Industry</b>"),
                                                      choices = jobstreet_industries)),
                      
                      box(h1("Top In-Demand Keywords")),
                      withSpinner(plotOutput("wordcloud")))
               )
               
    ),
    tabItem(
      tabName = "maps",
      fluidRow(box(column(width=5,h1("Job Maps", style = "color: #4d3a7d;")),
                   column(width=5,h2("You can view all locations of Open Jobs here")))
      ),
      fluidRow(box(title='Map', color = "blue", ribbon = FALSE,
                   title_side = "top left", solidHeader=TRUE,width = 10,     
                   mainPanel(withSpinner(plotOutput("plotAvailability"))),style="padding-top: 0px"),
               column(width=4, box(width=4,selectInput(inputId = "company_industry",label=HTML("<b>Industry</b>"),
                                                       choices = jobstreet_industries, selected = "Airline")),
                               box(radioButtons(inputId = "mapType",label=HTML("<b>Map Type</b>"),
                                                choices = c("hybrid","roadmap","satellite","terrain","toner")),style="line-height: 35px;padding-top: 0%"),
                               box(radioButtons(inputId = "format",label=HTML("<b>View Format</b>"),
                                                        choices = c("Point","Heatmap"))),style="line-height: 35px")
               )
      )

    ,
    tabItem(
      tabName = "resume",
      fluidRow(box(column(width=5,h1("Resume Score", style = "color: #4d3a7d;")),
                   column(width=5,h2("You can check your resume score here")))
      ),
      fluidRow(box(title='Job Openings', color = "blue", ribbon = FALSE,
                   title_side = "top left", solidHeader=TRUE,width = 10,     
                   mainPanel(DT::dataTableOutput("mytable"))),
               column(width=4, box(width=4,selectInput(inputId = "company_industry2",label=HTML("<b>Industry</b>"),
                                                       choices = jobstreet_industries)),
                      box(title='Resume Score', color = "blue", ribbon = FALSE,
                          title_side = "top left", solidHeader=TRUE, width = 10, gaugeOutput("gauge"),
                          actionButton("button", "Upload Resume")))
      )
      
      
      
    ),
    tabItem(
      tabName = "search",
      fluidRow(box(column(width=5,h1("Find a Job", style = "color: #4d3a7d;")),
                   column(width=5,h2("Let's do a job search here")))
      ),
      fluidRow(box(column(width=9,htmlOutput("value"))),
               column(width=4,box(textInput("caption", "Enter Your Ideal Job Here",placeholder = "Software Engineer"),
                                  actionButton("button2", "Enter Ideal Job"))))
    )
  )
)


ui1 <- dashboardPage(
    header,
    sidebar, 
    body
)

server1 <- shinyServer(function(input, output, session) {{
  autoInvalidate <- reactiveTimer(2000)
  
  getMap <- function(){
      get_map('Singapore', zoom = 11, maptype = input$mapType)
  }
  
  
  
  
  getIndustryData <- reactive( {
    data <- as.data.frame(jobstreet_data[jobstreet_data$company_industry== input$company_industry, c("longitude", "latitude")])
    colnames(data) <- c("long","lat")
    data
  })
  
  getDataByIndustry <- reactive( {
    data <- as.data.frame(jobstreet_data[jobstreet_data$company_industry== input$company_industry2, c("position_title", "company_name")])
    data
  })
  
  ### FOR HOME PAGE
  output$teamwork <- renderImage({
    filename <- 'teamwork.jpeg'
    list(src = filename,
         alt = paste("Teamwork", input$n))
    
  }, deleteFile = FALSE)
  
  dates <- unique(jobstreet_data$post_datetime)
  cdates <- dates[order(dates)]
  
  get_count <- function(date,industry){
    ind <- jobstreet_data[(jobstreet_data$company_industry==industry),]
    count <- nrow(ind[(ind$post_datetime==date),])
    count
  }
  plot_trend <- function(industry){
    res <- sapply(cdates,get_count,industry)
    df<-data.frame(x=cdates,y=res)
    ggplot(df) + geom_line(aes(x=cdates,y=res)) +
      labs(x = "Date", y = "Posted Jobs", title = 'Number of Posted Jobs vs Time') +
      scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few() +
      theme(legend.direction = "horizontal", legend.position = "bottom")
  }
  
  count_filtered <- function(date,industry) {
    filtered <- jobstreet_data[(jobstreet_data$company_industry==industry),]
    filtered <- filtered[!(grepl('\\|',filtered$close_date)),]
    nrow(filtered[((filtered$post_datetime <= date) & (date <= filtered$close_datetime)),])
  }

  plot_agg <- function(industry){
    filtered <- jobstreet_data[(jobstreet_data$company_industry==industry),]
    date_range <- seq(as.Date(min(filtered$post_datetime)), as.Date(max(filtered$post_datetime)), "days")
    count_daily <- sapply(date_range, count_filtered, industry)
    df<-data.frame(x=date_range,y=count_daily)
    ggplot(df) + geom_line(aes(x=date_range,y=count_daily)) +
      labs(x = "Date", y = "Open Jobs", title = 'Number of Open Jobs vs Time') +
      scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few() +
      theme(legend.direction = "horizontal", legend.position = "bottom")
  }
  ########################
  
  output$dropdown_user <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "John Doe", icon="user circle o", style = "min-width: 300px"),
                 messageItem("Seniority", "BBA Undergraduate", icon = "graduation cap"),
                 messageItem("School/Company", "NUS", icon = "building"),icon = icon("user o"),
                 notificationItem("Sign Out"))
  })
  
  output$intro <- renderUI({HTML(paste("<b>We are from Group:</b>", "3<br/>",
                                       "<b>Of Class:</b>", "A2<br/>",
                                       "<b>Our members are:</b>", "-Stefan<br/>-Daniel<br/>-Li Ming<br/>-Justus<br/>-Maxine",
                                       sep="<br/>"))})
  
  output$about <- renderUI({HTML(paste("<b>What we do:</b></br>", "We have created a dashboard to help fellow Undergraduates and others find a job in the market based on their preferences, qualifications, and also locations.</br></br>Please feel free to look around our Dashboard and let us know if you have any question  :)</br></br></br></br></br>",
                                       sep="<br/>"))})
  
  
  output$trendPlot <- renderPlotly({
    gg <- plotly_build(plot_trend(input$company_industry3))
  })
  
  output$aggPlot <- renderPlotly({
    gg <- plotly_build(plot_agg(input$company_industry3))
  })
  
  output$wordcloud <- renderImage({
    filename <- 'wordcloud.png'
    list(src = filename,
         alt = paste("WordCloud", input$n))
    
  }, deleteFile = FALSE)
  
  output$plotAvailability <-renderPlot(
    
    {
      if(input$format=='Point') {
        ggmap(getMap()) +  geom_point(data=getIndustryData(), aes(x = long, y = lat),color='red',alpha=0.5) +
          coord_fixed(ylim = c(1.24, 1.48), ratio = 1/cos(pi*1.29286/180))
      }
      else if (input$format == 'Heatmap'){
        ggmap(getMap()) + stat_density2d(data=getIndustryData(), aes(x= long, y=lat,  fill = ..level.., alpha = ..level..), size = 15, geom = 'polygon')+
          scale_fill_gradient(low = "green", high = "red", guide=FALSE) + scale_alpha(range = c(0, 1), guide = FALSE) +
          coord_fixed(ylim = c(1.24, 1.48), ratio = 1/cos(pi*1.29286/180))
      }
      else {print("Wrong format name")}
    }
  )
  
  output$mytable = DT::renderDataTable({getDataByIndustry()},selection='single',rownames=FALSE)
  selectedRow <- eventReactive(input$mytable_rows_selected,{
    row.names(getDataByIndustry())[c(input$mytable_rows_selected)]
  }) 
  output$gauge = renderGauge({
    gauge(scoretable[as.numeric(selectedRow())], 
          min = 0, 
          max = 1, 
          sectors = gaugeSectors(success = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))})
  
  #output$value <- renderText({paste("Company:", "RF360 Singapore Pte Ltd\n",
  #                                  "Position:", "Product Development Engineer / Senior Engineer - Software Programming\n",
  #                                  "Job Description:", "Develops information systems by designing developing and installing software solutions determines operational feasibility by evaluating analysis problem definition requirements solution development and proposed solutions documents and demonstrates solutions by developing documentation flowchart s layouts diagrams charts code comments and clear code provides information by collecting analyzing and summarizing development and service issues accomplishes engineering and organization mission by completing related results as needed develops software solutions by studying information needs conferring with users studying systems flow data usage and work processes investigating problem areas following the software development lifecycle requirements min degree in computer engineering computer science or rel a vent engineering field analyzing information general programming skills software design software debugging software documentation software testing problem solving software development fundamentals software development process software requirements experience in j mp mini tab programming as well as data visualization and manipulation will be an added advantage\n",
  #                                  sep="\n")})
  
  output$value <- renderUI({HTML(paste("<b>Company:</b>", "RF360 Singapore Pte Ltd<br/>",
                                      "<b>Position:</b>", "Product Development Engineer / Senior Engineer - Software Programming<br/>",
                                      "<b>Job Description:</b>", "Develops information systems by designing developing and installing software solutions determines operational feasibility by evaluating analysis problem definition requirements solution development and proposed solutions documents and demonstrates solutions by developing documentation flowchart s layouts diagrams charts code comments and clear code provides information by collecting analyzing and summarizing development and service issues accomplishes engineering and organization mission by completing related results as needed develops software solutions by studying information needs conferring with users studying systems flow data usage and work processes investigating problem areas following the software development lifecycle requirements min degree in computer engineering computer science or rel a vent engineering field analyzing information general programming skills software design software debugging software documentation software testing problem solving software development fundamentals software development process software requirements experience in j mp mini tab programming as well as data visualization and manipulation will be an added advantage<br/>",
                                      sep="<br/>"))})
}
})

shinyApp(ui=ui1, server=server1)





