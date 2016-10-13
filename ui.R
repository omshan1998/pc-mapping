shinyUI(
  fluidPage(
    navbarPage("HCD+D Data Chloropleth Tool", id="nav",
     tabPanel("Map", icon = icon("map-marker"),
      div(class="outer",
        tags$style(type = "text/css",
         ".outer {position: fixed; top: 50px; left: 0; right: 0;
           bottom: 0; overflow: hidden; padding: 0}"),
        leafletOutput("map", width="100%", height="100%"),

        absolutePanel(top = 0, right = 30, draggable=TRUE, 
          
            wellPanel(style = "background-color: #eeeeee; width: 350px;overflow-y:auto; max-height: 600px",
              checkboxInput('showPanel1', 'Show Filter panel', TRUE),
              conditionalPanel(condition = 'input.showPanel1',
              selectizeInput('MapFilter', 'Display a certain chloropleth:',
               choices= MapTypeList, multiple=FALSE, selected="WORK"),
              selectizeInput("hideCountry","Hide these countries", 
                             choices = WorldCountryList, multiple = TRUE),
              selectizeInput("countryFilter","Show only these countries",
                             choices = WorldCountryList, multiple = TRUE),
              #Cross filter
              wellPanel(style = "background-color: #ffffff; overflow-y:auto",
                checkboxInput("displaycrossfilter", "Cross filter", TRUE),
                conditionalPanel(condition = 'input.displaycrossfilter',
                  selectizeInput('FilterType', 'Cross Filter Type:',
                   choices = c("",MapTypeList), multiple=FALSE,selected="ALLPUB"),
                  selectizeInput('HideCrossCountry', 'Hide these cross countries:',
                   choices= WorldCountryList, multiple=TRUE),
                  selectizeInput('countryCrossFilter', 'Show only these cross countries:',
                   choices = WorldCountryList, multiple=TRUE)
                )
              ),
              
              #Other filter
              wellPanel(style = "background-color: #ffffff; overflow-y:auto",
                checkboxInput('morefilters', 'More filters', TRUE),
                conditionalPanel(condition = "input.morefilters",
                  sliderInput('citations', 'Filter by Citations', min = min(articles$Cites), max = max(articles$Cites), value = c(min(articles$Cites),max(articles$Cites)), step = 1),
                  sliderInput('Year', 'Filter by Latest Year', min = min(articles$Year), max = max(articles$Year), value = c(min(articles$Year),max(articles$Year)), step = 1),
                  sliderInput('GSRank', 'FIlter by GSRank', min = min(articles$GSRank), max=max(articles$GSRank), value= c(min(articles$GSRank),max(articles$GSRank)), step = 1),
                  selectizeInput('Authors', 'Filter by Authors:',
                   choices= AuthorList, multiple=TRUE),
                  selectizeInput('University', "Filter by 1st Author's University:",
                   choices= UniversityList, multiple=TRUE),
                  selectizeInput('Publisher', 'Filter by Publisher:', 
                   choices= UniversityList, multiple=TRUE),
                  selectizeInput('KeywordList', 'Choose Keywords', 
                   choices = c("Poverty", "Developing Countries", "low-resource settings",
                     "low-income", "developing world", "third world", 
                     "resource-limited settings", "resource-limited", 
                     "Global Inequality", "international development"), multiple=TRUE)
                  )
              )

              
              )
            )
          )
        )
      ),
     tabPanel("CSV", icon = icon("table"),
              DT::dataTableOutput("table")
              
              ),
     tabPanel("About",
      icon = icon("question"),

        #content on left hand side of the page
        h1("About"),
        br(),
        p("This map is an interactive tool which depicts geographically the current state of the field of human-centered design for development. This tool shows 
          the number of first authors from institutions in certain countries completing research,
          the number of authors altogether from institutions in certain countries completing research,
          the number of authors that aren't first authors from institutions in certain countries completing research, 
          the number of papers, per country, where the dataset engages in research.
          the tool gives researchers the ability to filter by countries where countries decide to work. (e.g. is United States is chosen, one can see where United States-affiliated authors has engaged in research), and conversely, the tool gives the ability to filter by countries that engage in work in a certain country (e.g. if the United States is chosen, one can see authors from separate countries engage in research in the United States).
          Additionally, the tool can be filtered by critical metrics analyzed from each paper, including GSRank, number of citations, available authors in the field, publications, universities, and search keywords which found the papers.")
        )
     )
    )
  )

