# V0.0.5
required <- c('curl', 'jsonlite', 'shiny', 'shinydashboard', 'DT', 'stringr')
for(pkg in required){
    if (!require(pkg, character.only = TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
}
library(curl)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(DT)
library(stringr)
library(ggplot2)

effectPosits <- function(){
    effects <- c(
        'Back to Deck', 
        'Back to Hand', 
        'Banish', 
        'Control', 
        'Change ATK or DEF',
        'Counter',
        'Damage LP',
        'Destroy Monster', 
        'Direct Attack',
        'Draw',
        'Effect Damage',
        'Fusion-Related',
        'Gamble',
        'Graveyard',
        'Increase Level', 
        'LINK-Related',
        'Negate',
        'Pendulum-Related',
        'Piercing',
        'Recover LP',
        'Repeat Attack',
        'Ritual-Related',
        'Search',
        'Select',
        'Special Summon',
        'Synchro-Related',
        'Token',
        'Tuner-Related',
        'Win the Duel',
        'XYZ-Related'
    )
    names(effects) <- effects
    effects <- gsub(' ', '%20', effects)
    ed <- lapply(effects, function(x) jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(sprintf('https://db.ygoprodeck.com/api/v7/cardinfo.php?effect=%s', x))$content))$data[,1] 
    )
    return(ed)
}

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
    sets <- tapply(X=collection$cardset, INDEX=collection$cardid, FUN=function(x) paste(unique(x), collapse=';'))
    rarity <- tapply(X=collection$cardrarity, INDEX=collection$cardid, FUN=function(x) paste(unique(x), collapse=';'))
    owned <- cbind(img = get_small_img_url(as.character(names(totals))),db[as.character(names(totals)),], quantity=totals, set=sets, rarity=rarity)
    owned <- owned[ ,c(1,2,3,13,4,5,6,7,8,9,10,11,12,14,15)]
    return(owned)
}

get_small_img_url <- function(id) sprintf('<img src="https://storage.googleapis.com/ygoprodeck.com/pics_small/%s.jpg" height="75"></img>', id)
get_big_img_url <- function(id) sprintf('<img src="https://storage.googleapis.com/ygoprodeck.com/pics/%s.jpg"></img>', id)

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
            menuItem(
                'Deck',
                tabName = 'deck',
                icon = icon('th')
                ),
            uiOutput('collection_import'),
            uiOutput('searches'),
            downloadButton("download_button", label = "Export Deck")
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = 'main',
                fluidPage(
                    uiOutput('sorting_options'),
                    DT::dataTableOutput('main_table'),
                    plotOutput("luck_plot")

                )
            ),
            tabItem(tabName = 'deck',
                fluidPage(
                    DT::dataTableOutput('main_deck'),
                    DT::dataTableOutput('side_deck'),
                    DT::dataTableOutput('extra_deck')
                )
            )
        )
    )
)

server <- function(input, output, session){
    card_db <- getCardInfo()
    
    effects <- c(
        'Back to Deck', 
        'Back to Hand', 
        'Banish', 
        'Control', 
        'Change ATK or DEF',
        'Counter',
        'Damage LP',
        'Destroy Monster', 
        'Direct Attack',
        'Draw',
        'Effect Damage',
        'Fusion-Related',
        'Gamble',
        'Graveyard',
        'Increase Level', 
        'LINK-Related',
        'Negate',
        'Pendulum-Related',
        'Piercing',
        'Recover LP',
        'Repeat Attack',
        'Ritual-Related',
        'Search',
        'Select',
        'Special Summon',
        'Synchro-Related',
        'Token',
        'Tuner-Related',
        'Win the Duel',
        'XYZ-Related'
    )
    
    eff_ids <- effectPosits()

    deck_cards <- reactiveValues()
    deck_cards$main_deck_cards <- matrix(NA, 10, 6)
    deck_cards$side_deck_cards <- matrix(NA, 15, 1)
    deck_cards$extra_deck_cards <- matrix(NA, 15, 1)

    selected_card <- reactiveValues()
    selected_card$imgcode <- ''

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
            selectizeInput('set', label='Card Set', choices=unique(unlist(strsplit(as.character(sessiondata$owned[,'set']), ';'))), multiple=TRUE),
            selectizeInput('rarity', label='Rarity', choices=unique(unlist(strsplit(as.character(sessiondata$owned[,'rarity']), ';'))), multiple=TRUE),
            selectizeInput('archetype', label='Archetype', choices=levels(factor(as.character(sessiondata$owned[,'archetype']))), multiple=TRUE),
            
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
                selectizeInput('eff', 'Filter by Effect', choices = levels(factor(effects)), multiple = TRUE)   
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
        if(!is.null(input$set)) {
            rows_to_render <- rows_to_render & grepl(paste(input$set, collapse='|'), sessiondata$owned[,'set'])
        }
        if(!is.null(input$rarity)){
            rows_to_render <- rows_to_render & grepl(paste(input$rarity, collapse='|'), sessiondata$owned[,'rarity'])
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
        if(!is.null(input$eff)){
            selected <- unique(unlist(eff_ids[input$eff]))
            if(is.null(selected)){
            valid <- rows_to_render
            } else {
            valid <- sessiondata$owned[,'id'] %in% selected
            }
            rows_to_render <- rows_to_render & valid
        }
        sessiondata$owned[which(rows_to_render), ]
    })

    output$main_table <- DT::renderDataTable({
        DT::datatable(
            main(),
            selection = 'single',
            escape=FALSE,
            options = list(pageLength = 100)
        )
    })

    output$main_deck <- DT::renderDataTable({DT::datatable(t(deck_cards$main_deck_cards), escape=FALSE, selection = list(mode='single',target = 'cell'), options = list(autoWidth = TRUE, searching = FALSE,dom = 't'))})
    output$side_deck <- DT::renderDataTable({DT::datatable(t(deck_cards$side_deck_cards), escape=FALSE, selection = list(mode='single',target = 'cell'), options = list(autoWidth = TRUE, searching = FALSE,dom = 't'))})
    output$extra_deck <- DT::renderDataTable({DT::datatable(t(deck_cards$extra_deck_cards), escape=FALSE, selection = list(mode='single',target = 'cell'), options = list(autoWidth = TRUE,searching = FALSE,dom = 't'))})

    # Main Table Actions??
    observeEvent(input$main_table_rows_selected, {
        id <- main()[input$main_table_rows_selected, 'id']
        imgcode <- get_small_img_url(id)
        selected_card$imgcode <- imgcode
        selected_card$id <- id
        output$N_Cards_Where <- renderPrint({
            ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
            sprintf(
                'M: (%s) S: (%s) E: (%s)', ggc[1], ggc[2], ggc[3]
            )
        })
        imgcode <- get_big_img_url(id)
        invoke_card_modal(imgcode)
    })

    output$Card_Ns <- renderPrint({
        sprintf(
            'M: (%s/60) S: (%s/15) E: (%s/15)', 
            sum(!is.na(deck_cards$main_deck_cards)), 
            sum(!is.na(deck_cards$side_deck_cards)), 
            sum(!is.na(deck_cards$extra_deck_cards))
        )
    })

    output$N_Cards_Where <- renderPrint({
        ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
        sprintf(
            'M: (%s) S: (%s) E: (%s)', ggc[1], ggc[2], ggc[3]
        )
    })

    # This needs refactoring.
    observeEvent(input$addtomain, {
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
        n_in_decks <- sum(ggc)
        if(n_in_decks < count & n_in_decks<3){
            # Show number of slots?
            deck_cards$main_deck_cards[which(is.na(deck_cards$main_deck_cards))[1]] <- imgcode
            deck_cards$main_deck_cards <- matrix(sort(deck_cards$main_deck_cards, na.last=TRUE),10,6)
        }
    })

    observeEvent(input$removemain,{
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        cardloc <- deck_cards$main_deck_cards == imgcode
        deck_cards$main_deck_cards[which(cardloc)[1]] <- NA
        deck_cards$main_deck_cards <- matrix(sort(deck_cards$main_deck_cards, na.last=TRUE),10,6)    
    })

    observeEvent(input$addtoextra, {
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
        n_in_decks <- sum(ggc)
        if(n_in_decks < count & n_in_decks<3){
            # Show number of slots?
            deck_cards$extra_deck_cards[which(is.na(deck_cards$extra_deck_cards))[1]] <- imgcode
            deck_cards$extra_deck_cards <- matrix(sort(deck_cards$extra_deck_cards, na.last=TRUE),15,1)
        }
    })

    observeEvent(input$removeextra,{
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        cardloc <- deck_cards$extra_deck_cards == imgcode
        deck_cards$extra_deck_cards[which(cardloc)[1]] <- NA
        deck_cards$extra_deck_cards <- matrix(sort(deck_cards$extra_deck_cards, na.last=TRUE),15,1)    
    })

    observeEvent(input$addtoside, {
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
        n_in_decks <- sum(ggc)
        if(n_in_decks < count & n_in_decks<3){
            # Show number of slots?
            deck_cards$side_deck_cards[which(is.na(deck_cards$side_deck_cards))[1]] <- imgcode
            deck_cards$side_deck_cards <- matrix(sort(deck_cards$side_deck_cards, na.last=TRUE),15,1)
        }
    })

    observeEvent(input$removeside,{
        id <- selected_card$id
        ids <- which(main()[, 'id'] == selected_card$id)
        count <- main()[ids, 'quantity']
        imgcode <- get_small_img_url(id)
        cardloc <- deck_cards$side_deck_cards == imgcode
        deck_cards$side_deck_cards[which(cardloc)[1]] <- NA
        deck_cards$side_deck_cards <- matrix(sort(deck_cards$side_deck_cards, na.last=TRUE),15,1)    
    })


    # Deck Table Actions!
    observeEvent(input$main_deck_cells_selected, {
        if(length(input$main_deck_cells_selected)>0){
            idx <- input$main_deck_cells_selected[1,]
            cellcontents <- deck_cards$main_deck_cards[idx[2]+1, idx[1]]
            id <- strsplit(strsplit(cellcontents, '/')[[1]], '.jpg')[[6]][1]
            imgcode <- get_small_img_url(id)
            selected_card$id <- id
            selected_card$imgcode <- imgcode
            output$N_Cards_Where <- renderPrint({
                ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
                sprintf(
                'M: (%s) S: (%s) E: (%s)', ggc[1], ggc[2], ggc[3]
                )
            })
            imgcode <- get_big_img_url(id)
            invoke_card_modal(imgcode)
        }
    })
    observeEvent(input$side_deck_cells_selected, {
        if(length(input$side_deck_cells_selected)>0){
            idx <- input$side_deck_cells_selected[1,]
            cellcontents <- deck_cards$side_deck_cards[idx[2]+1, idx[1]]
            id <- strsplit(strsplit(cellcontents, '/')[[1]], '.jpg')[[6]][1]
            imgcode <- get_small_img_url(id)
            selected_card$id <- id
            selected_card$imgcode <- imgcode
            output$N_Cards_Where <- renderPrint({
                ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
                sprintf(
                'M: (%s) S: (%s) E: (%s)', ggc[1], ggc[2], ggc[3]
                )
            })
            imgcode <- get_big_img_url(id)
            invoke_card_modal(imgcode)
        }
    })
    observeEvent(input$extra_deck_cells_selected, {
        if(length(input$extra_deck_cells_selected)>0){
            idx <- input$extra_deck_cells_selected[1,]
            cellcontents <- deck_cards$extra_deck_cards[idx[2]+1, idx[1]]
            id <- strsplit(strsplit(cellcontents, '/')[[1]], '.jpg')[[6]][1]
            imgcode <- get_small_img_url(id)
            selected_card$id <- id
            selected_card$imgcode <- imgcode
            output$N_Cards_Where <- renderPrint({
                ggc <- get_global_count(isolate(selected_card$imgcode), isolate(deck_cards))
                sprintf(
                'M: (%s) S: (%s) E: (%s)', ggc[1], ggc[2], ggc[3]
                )
            })
            imgcode <- get_big_img_url(id)
            invoke_card_modal(imgcode)
        }
    })


    output$download_button <- downloadHandler(
        filename = function(){
            paste("Deck-", Sys.Date(), ".ydk", sep = "")
        },
        content = function(file) {
            text<-c(
                '#created by ...', 
                '#main', 
                unlist(lapply(na.omit(as.vector(deck_cards$main_deck_cards)), getpaddedid_from_imgstring)),
                '#extra',
                unlist(lapply(na.omit(as.vector(deck_cards$extra_deck_cards)), getpaddedid_from_imgstring)),
                '!side',
                unlist(lapply(na.omit(as.vector(deck_cards$side_deck_cards)), getpaddedid_from_imgstring))
            )
            print(text)
            writeLines(paste(text, collapse='\n'), file)
        }
    )
}

getpaddedid_from_imgstring <- function(x){
    str_pad(as.character(strsplit(strsplit(x, '/')[[1]], '.jpg')[[6]][1]), width=8, side='left', pad='0')
}

invoke_card_modal <- function(imgcode){
    showModal(modalDialog(
        title = "",
        fluidPage(
            verbatimTextOutput("Card_Ns"),
            verbatimTextOutput("N_Cards_Where"),
            fluidRow(
                column(4, actionButton('addtomain', 'Add to Main')),
                column(4, actionButton('addtoside', 'Add to Side')),
                column(4, actionButton('addtoextra', 'Add to Extra'))
            ),
            fluidRow(
                column(4, actionButton('removemain', 'Remove From Main')),
                column(4, actionButton('removeside', 'Remove From Side')),
                column(4, actionButton('removeextra', 'Remove From Extra'))
            ),
            HTML(imgcode)
        ),
        easyClose = TRUE,
        footer = NULL
    ))
}

get_global_count <- function(card, decks){
    c(
        sum(decks$main_deck_cards == card, na.rm = TRUE), 
        sum(decks$side_deck_cards == card, na.rm = TRUE), 
        sum(decks$extra_deck_cards == card, na.rm = TRUE)
    )
}

app <- shinyApp(ui=ui, server=server)
runApp(app, launch.browser=TRUE)
