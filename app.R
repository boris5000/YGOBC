required <- c('curl', 'jsonlite', 'shiny', 'shinydashboard', 'DT')
for(pkg in required){
    if (!require(pkg)) install.packages(pkg, repos='https://cloud.r-project.org')
}
library(curl)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(DT)

getCardInfo <- function(){
    message('Getting Card List...')
    card_db <- lapply(jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory('https://db.ygoprodeck.com/api/v7/cardinfo.php')$content))$data, c)
    card_table <- with(card_db, data.frame(id, name, type, desc, race, archetype, level, atk, def, attribute, scale, stringsAsFactors=TRUE))
    card_table$desc <- as.character(card_table$desc)
    rownames(card_table) <-  as.character(card_table$id)
    return(card_table)
}

read_card_collection_csv <- function(file, db){
    collection <- read.csv(file, stringsAsFactors=FALSE, header=TRUE)
    totals <- tapply(X=collection$cardq, INDEX=collection$cardid, FUN=sum)
    owned <- cbind(db[as.character(names(totals)),], quantity=totals)
    owned <- owned[ ,c(1,2,12,3,4,5,6,7,8,9,10,11)]
    return(owned)
}

get_img_url <- function(id) sprintf("https://storage.googleapis.com/ygoprodeck.com/pics/%s.jpg", id)

normalise_path <- function(x, root=getwd()){
    fp <- unlist(x)
    tfp <- head(fp, length(fp)-1)
    cfp <- paste(c(root, tfp), collapse='/')
    return(cfp)
}

ui <- dashboardPage(
    header = dashboardHeader(title='Boris5000\'s YGO Collection Browser'),
    sidebar = dashboardSidebar(
        sidebarMenu(
            id='sidebar',
            menuItem(
                'Collection',
                tabName = 'main',
                icon = icon('th')
                ),
            uiOutput('collection_import'),
            uiOutput('searches')
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = 'main',
                fluidPage(
                    uiOutput('sorting_options'),
                    DT::dataTableOutput('main_table')
                )
            )
        )
    )
)

server <- function(input, output, session){
    card_db <- getCardInfo()

    if(!interactive()) {
        session$onSessionEnded(function(){
          stopApp()
          q('no')
        })
    }

    sessiondata <- reactiveValues()
    sessiondata$owned <- ''

    output$collection_import <- renderUI({
        fileInput('collection_file', 'Choose File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    })

    observeEvent(input$collection_file, ignoreNULL=TRUE, {
        sessiondata$owned <- read_card_collection_csv(input$collection_file$datapath, db=card_db)
    })

    output$sorting_options <- renderUI({
        fluidPage(
        column(6,
            selectizeInput('attribute', label = 'Attribute', choices=levels(factor(as.character(sessiondata$owned[,'attribute']))), multiple=TRUE),
            selectizeInput('type', label = 'Type', choices=levels(factor(as.character(sessiondata$owned[,'type']))), multiple=TRUE),
            selectizeInput('race', label='Race', choices=levels(factor(as.character(sessiondata$owned[,'race']))), multiple=TRUE),
            selectizeInput('archetype', label='Archetype', choices=levels(factor(as.character(sessiondata$owned[,'archetype']))), multiple=TRUE)
        ),
        column(6,
            fluidRow(
                column(3, checkboxInput('atk', 'Filter by ATK?', value=FALSE)),
                column(9, sliderInput('atk_slider', 'ATK Range', 
                    min = min(sessiondata$owned[,'atk'], na.rm=T),
                    max = max(sessiondata$owned[,'atk'], na.rm=T),
                    value = c(
                        min(sessiondata$owned[,'atk'], na.rm=T), 
                        max(sessiondata$owned[,'atk'], na.rm=T)
                    )))
            ), 
            fluidRow(
                column(3, checkboxInput('def', 'Filter by DEF?', value=FALSE)),
                column(9, sliderInput('def_slider', 'DEF Range', 
                    min = min(sessiondata$owned[,'def'], na.rm=T),
                    max = max(sessiondata$owned[,'def'], na.rm=T),
                    value = c(
                        min(sessiondata$owned[,'def'], na.rm=T), 
                        max(sessiondata$owned[,'def'], na.rm=T)
                    )))
            ), 
            fluidRow(
                column(3, checkboxInput('lvl', 'Filter by Level?', value=FALSE)),
                column(9, sliderInput('lvl_slider', 'Level Range', 
                    min = min(sessiondata$owned[,'level'], na.rm=T),
                    max = max(sessiondata$owned[,'level'], na.rm=T),
                    value = c(
                        min(sessiondata$owned[,'level'], na.rm=T), 
                        max(sessiondata$owned[,'level'], na.rm=T)
                    ), step=1))
            ),
            fluidRow(
                textInput('fuzzy', label = 'Search Card Descriptions', value = '', placeholder='e.g. Destroy')
            )
        ))
    })

    main <- reactive({
        rows_to_render <- rep(TRUE, nrow(sessiondata$owned))
        if(!is.null(input$attribute)) {
            rows_to_render <- rows_to_render & (sessiondata$owned[,'attribute'] %in% input$attribute)
        }
        if(!is.null(input$type)) {
            rows_to_render <- rows_to_render & (sessiondata$owned[,'type'] %in% input$type)
        }
        if(!is.null(input$race)) {
            rows_to_render <- rows_to_render & (sessiondata$owned[,'race'] %in% input$race)
        }
        if(!is.null(input$archetype)) {
            rows_to_render <- rows_to_render & (sessiondata$owned[,'archetype'] %in% input$archetype)
        }
        if(input$atk){
            rows_to_render <- rows_to_render & (sessiondata$owned[,'atk'] >= input$atk_slider[1] & sessiondata$owned[,'atk'] <= input$atk_slider[2])
        }
        if(input$def){
            rows_to_render <- rows_to_render & (sessiondata$owned[,'def'] >= input$def_slider[1] & sessiondata$owned[,'def'] <= input$def_slider[2])
        }
        if(input$lvl){
            rows_to_render <- rows_to_render & (sessiondata$owned[,'level'] >= input$lvl_slider[1] & sessiondata$owned[,'level'] <= input$lvl_slider[2])
        }
        if(!input$fuzzy == ''){ 
            rows_to_render <- rows_to_render & grepl(input$fuzzy, sessiondata$owned[,'desc'], ignore.case=TRUE)
        }
        sessiondata$owned[which(rows_to_render), ]
    })

    output$main_table <- DT::renderDataTable({
        DT::datatable(
            main(),
            selection = 'single',
            options = list(pageLength = 100)
        )
    })

}

app <- shinyApp(ui=ui, server=server)
runApp(app, launch.browser=TRUE)
