jail_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = 'Jail',
           fluidPage(
             # tags$head(tags$style(css)),
             sidebarPanel(width = 3,
                          h4("York County Jail",
                             style = "color: grey; font-variant: small-caps;"),
                          h4("mental health status codes",
                             style = "color: grey; font-variant: small-caps;"),
                          br(),
                          tags$strong("A:"),
                          "No identified mental health needs or history of psychiatric treatment for the last five years.",
                          br(),
                          br(),
                          tags$strong("B:"),
                          "A history of treatment at the facility, but does not currently require mental health services.",
                          br(),
                          br(),
                          tags$strong("C:"),
                          "Currently active in treatment with the mental health department.",
                          br(),
                          br(),
                          tags$strong("D:"),
                          "Currently active in treatment with the mental health department and has been diagnosed with a Serious Mental Illness (SMI) and/or exhibits significant adjustment/behavioral concerns.",
                          tags$hr(),
                          #horizontal line
                          uiOutput(ns("timeperiod")),
                          hr(),
                          # helpText("Inmates in jail at least one day of the above year are represented in statistics.
                          #   (In 2016 after Oct. 1.)") #extract categories from table of counts. "; in 2019 until Mar. 6"
                          conditionalPanel(
                            condition = "input.conditionedPanels ==1",
                            tags$small("The average length of stay is calculated as = (sum of the total lengths of stay) / (total number of release events during the reporting period)")
                          ),
                          conditionalPanel(
                            condition = "input.conditionedPanels ==3",
                            tags$small("The percentage recidivating is = (# admitted who have a prior jail admission in the county jail in the past year) / (total admissions during the reporting period)")
                          )
             ),
             
             mainPanel(width = 9,
                       fluidRow(
                         column(width = 12,
                                box(width = 6, height = 125,
                                    h2(textOutput(ns('bookingCount')),
                                       style = "text-align: center; font-face: bold"),
                                    h5(paste("All Bookings"), style = "text-align: center;")
                                ),
                                box(width = 6, height = 125,
                                    h2(textOutput(ns('smiCounter')),
                                       style = "text-align: center; font-face: bold"),
                                    h5(paste("Bookings Rated D"), style = "text-align: center;"),
                                    br()
                                ),
                                tags$small("Jail-assigned codes are assessed at 90 days. People without a rating were not yet evaluated. Ratings may change during a jail term and also in subsequent jail bookings."),
                                tags$hr())
                       ),
                       
                       fluidRow(
                         tabsetPanel(
                           tabPanel(
                             "Length of Stay",
                             value = 1,
                             width = 12,
                             addSpinner(htmlOutput(ns("alos_graph")), spin =
                                          "bounce", color = ol_cols[1]),
                             tags$hr(),
                             h4("Average Length of Stay (days)"),
                             DT::dataTableOutput(ns("lengthOfStay_table")),
                             tags$hr(),
                             h4("Number of release events"),
                             DT::dataTableOutput(ns("releases_table"))
                           ),
                           tabPanel(
                             "ADP",
                             value = 2,
                             width = 12,
                             h4("Average Daily Population"),
                             tags$hr(),
                             DT::dataTableOutput(ns("adp_table")),
                             br(),
                             tags$small("The average daily population is = (bookings during reporting period * ALOS during reporting period) / (days in reporting period)"),
                             tags$hr()
                           ),
                           tabPanel(
                             "Recidivism",
                             value = 3,
                             width = 12,
                             addSpinner(plotlyOutput(ns("recidivism_graph")), spin =
                                          "bounce", color = ol_cols[1]),
                             tags$hr(),
                             h4("Recidivism (%)"),
                             DT::dataTableOutput(ns("recidivism_table")),
                             br(),
                             tags$hr()
                           ),
                           tabPanel(
                             "Release Type",
                             value = 4,
                             width = 12,
                             addSpinner(plotlyOutput(ns("releaseType")), spin =
                                          "bounce", color = ol_cols[1]),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             tags$small(
                               "This graph shows all releases for individuals assigned an SMI status starting in 2017.
                     Many individuals have multiple jail bookings stretching backwards before 2017, which are not included."),
                             br(),
                             br()
                           ),
                           
                           id = "conditionedPanels"
                         )
                       )
             )
           )
           
  )
  
}

jail_server <-
  function(input,
           output,
           session) {
    ns <- session$ns
    
    
    output$timeperiod <- renderUI(selectInput(
      ns("timeperiod"),
      "Year:",
      choices = unique(numbookings_status$yr_booked),
      selected = "2017"
    ))
    
    output$bookingCount <- renderText({
      if (!is.null(input$timeperiod)) {
            numbookings_status %>% 
              filter(yr_booked %in% input$timeperiod & status  %in% "All") %>%
          pull(n_bookings)
      }
    })
    
    #### Reactive labels, using updated totals
    output$smiCounter <- renderText({
      numbookings_status %>%
        filter(status %in% "MHSR-D" & yr_booked %in% input$timeperiod) %>%
        pull(n_bookings)
    })
    
    output$lengthOfStay_table <- DT::renderDataTable({
      DT::datatable(alos_allinmates %>%
                      # filter(!is.na(status)) %>%
                      pivot_wider(id_cols = status, names_from = yr_released,
                                  values_from = avg_los)
      )
    })
    
    output$releases_table <- DT::renderDataTable({
      DT::datatable(alos_allinmates %>%
                      # filter(!is.na(status)) %>%
                      pivot_wider(id_cols = status, names_from = yr_released,
                                  values_from = n_releases)
      )
    })
    
    output$alos_graph <- renderUI({
      list_of_plots <- map(unique(alos_allinmates$yr_released), function(x) {

        filtered <-
          alos_allinmates %>%
          filter(yr_released == x ) #& !is.na(status)

        highchart() %>%
          hc_add_series(data = filtered,
                        hcaes(x = status, y = avg_los, group = status),
                        type = "column") %>%
          hc_title(text = x) %>%
          hc_colors(rev(customcolor2))   #ol_cols[1:4]) #c("#D95F02", "#EFC000FF")

      })

      hw_grid(list_of_plots, rowheight = 300) %>%
        htmltools::browsable()
    })
    
    #---------- Average Daily Population (yearly)
    output$adp_table <- DT::renderDataTable({
    # Make data: adp = (bookings during reporting period * ALOS during reporting period) / (days in reporting period)"
    adp <- yorkjail_data %>%
      filter(yr_booked_num >= 2017) %>%
      mutate(status =  replace_na(status, "Not Evaluated"),
             status = ordered(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))) %>%
      group_by(yr_booked_num, yr_booked, status) %>%
      summarize(n_bookings = n_distinct(personID)) %>%
      ungroup() %>%
      # filter(!is.na(status)) %>%
      left_join(alos_allinmates %>%
                  mutate(status =  replace_na(status, "Not Evaluated"),
                         status = ordered(status, levels = c("MHSR-A", "MHSR-B", "MHSR-C", "MHSR-D", "Not Evaluated"))),
                by=c("yr_booked_num" = "yr_released", "status"="status"),
                na_matches = "never") %>%
      mutate(adp = n_bookings * avg_los / 365)
      
      DT::datatable(adp %>%
                      mutate(adp = round(adp,0)) %>%
                      pivot_wider(id_cols = status, names_from = yr_booked_num,
                                  values_from = adp)
      )
    })

    output$recidivism_table <- DT::renderDataTable({
      DT::datatable(recidivism %>%
                      mutate(recidivism = round(recidivism, 1)) %>%
                      pivot_wider(id_cols = status, names_from = yr_booked,
                                  values_from = recidivism)
      )
    })

    output$recidivism_graph <- renderPlotly({
      rec_g <- recidivism %>%
        ggplot(aes(x=status, y=recidivism, fill=status,
                   text = paste0('% recidivism: ', round(recidivism, 1),
                                 '<br># of people: ',num_withflag,
                                 '<br># of bookings: ',n_bookings,
                                 '<br>status: ', status))) +
        geom_bar(stat="identity") +
        facet_wrap(~ yr_booked) +
        scale_fill_manual(values = rev(customcolor2)) +
        scale_x_discrete(labels = c("A", "B", "C", "D", "None")) +
        # scale_fill_brewer(palette = "GnBu") +
        theme_bw() + theme(legend.position = "none", axis.title = element_blank())

      ggplotly(rec_g, tooltip = "text")
    })

    output$releaseType <- renderPlotly({
      #colorramp
      color <- brewer.pal(11, "Spectral")
      release_color <- colorRampPalette(color)(13)

      data <- yorkjail_data %>% 
        filter(yr_booked %in% input$timeperiod) %>% #!is.na(status) & 
        distinct(status, GeneralIdentifier, .keep_all = TRUE) %>% 
        mutate(status =  replace_na(status, "Not Evaluated"),
               status = ordered(status, levels = c("Not Evaluated", "MHSR-D", "MHSR-C", "MHSR-B", "MHSR-A"))) %>%
        group_by(status, Description) %>%
        summarize(personcount = n(), year = "Total") %>%
        ungroup() %>%
        group_by(status) %>%
        mutate(total_medstat=sum(personcount),
               Description = replace_na(Description, "None Specified"),
               Description = as.factor(Description)) %>%
        rename(Status = status) %>%
        group_by(Description, add=TRUE) %>%
        mutate(percent = round(personcount/total_medstat*100, 2))
      
      # graph
      release_smi <-
        ggplot(data,
               aes(x = Status, y = percent, fill = fct_rev(fct_infreq(Description)),
                   text = paste('Status: ', Status,
                                '<br>Type: ', Description,
                                '<br>Percent: ', percent))
        ) +
        geom_bar(stat = "identity",
                 colour = "white",
                 size = .1) + coord_flip() + theme_minimal() +
        theme(axis.title = element_blank(), legend.position = "bottom") +
        guides(fill = guide_legend(title = NULL)) + scale_fill_manual(values = release_color) #reverse = TRUE,

      ggplotly(release_smi, tooltip = "text", height = 650) %>%
        layout(title = 'Release Type (%) by Jail Mental Health Category', margin = pltl_margins) %>%
        layout(legend = list(
          traceorder = 'reversed',
          orientation = 'h',
          y = -0.3
        ))
    })
    
    
  }
