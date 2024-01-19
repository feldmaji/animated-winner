# Version 0.3.4
# Version Date: 1/12/2024
# Author: Jacob I Feldman, PhD, CCC-SLP

library(shiny)
library(rstudioapi)
library(stringr)
library(stringi)
library(tidycensus)
library(tidyverse)
library(viridis)
library(zipcodeR)
library(htmlTable)
library(tigris)
library(colorr)
library(ggpubr)
library(usmap)

#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(tidygeocoder)
library(plotly)

# Census API information
  #census_api_key(askForSecret("Census API token"), overwrite=F, install=T)

## Define variables of interest
variables = load_variables(year = 2020, dataset = "pl")
varlist <- c("P1_001N", "P1_003N", "P1_004N", "P1_005N", "P1_006N", "P1_007N", "P1_008N", "P1_009N", 
             "P2_001N", "P2_002N", "P2_003N", 
             "P2_005N", "P2_006N", "P2_007N", "P2_008N", "P2_009N", "P2_011N")
variables = variables[which(variables$name %in% varlist),]

# Get a list of states from the data source
states <- get_decennial(geography = "state", year = 2020, variables = variables$name) %>%
    select(NAME, GEOID) %>%
    distinct() %>% arrange(NAME)

addmdat = read.csv(url("https://raw.githubusercontent.com/feldmaji/animated-winner/main/addmdata.csv"))
rownames = c("1 American Indian / Alaska Native", "2 Asian", "3 Native Hawaiian or Other Pacific Islander", "4 Black or African American", "5 White", "6 Multiple")

axx <- list(linecolor = "black", linewidth = 0.5,mirror = T)

annotations = list( 
  list(
    x = .25,
    y = 1,
    text = "Non-Hispanic or Latino/a/x",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(size = 12)),
  
  list(
    x = .75,
    y = 1,
    text = "Hispanic or Latino/a/x",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = (1/7*6),
    text = "American Indian/\nAlaska Native",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = .725,
    text = "Asian",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = .525,
    text = "Native Hawaiian\nor OPI",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = .37,
    text = "Black or AA",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = .23,
    text = "White",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12)),
  
  list(
    x = -0.05,
    y = .05,
    text = "Multiple",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    textangle = 270,
    font = list(size = 12))
)

states_df   <- rbind(plot_usmap(region = "states")$data %>% rename(NAME = full),
                    read.csv(url("https://raw.githubusercontent.com/feldmaji/animated-winner/main/pr_state.csv")))

counties_df <- rbind(plot_usmap(regions = "counties")$data,
                    read.csv(url("https://raw.githubusercontent.com/feldmaji/animated-winner/main/pr_counties.csv")))

zip_code_db_jf = read.csv(url("https://raw.githubusercontent.com/feldmaji/animated-winner/main/zip_code_db_jf.csv"), colClasses = c(zipcode="character"))

# Define UI for application to define input for the table
ui <- fluidPage(

    # Application title
    titlePanel("Participant Demographics Estimate"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("Input your desired sample size, the data source you would like to use to generate your planned enrollment table, and input your planned male:female ratio and proportion of Hispanic or Latino/a/x participants."),
        p(""),
        numericInput("desiredn", 
                    label = "Desired sample size:",
                    value = 100,
                    min = 1,
                    step = 1),
        
        selectInput("source", 
                    label = "Choose the data source you would like to utilize:",
                    choices = c("Decennial Census Data - 2020", 
                                "Autism Developmental Disabilities Monitoring Network (ADDM) - 2020"#,
                              #  "Custom Input"
                              )
                    ),

        conditionalPanel(condition = "input.source == 'Autism Developmental Disabilities Monitoring Network (ADDM) - 2020'",
                         
                       # selectInput("addmsite", 
                       #             label = "Would you like to use national prevalence data or data from one (or more) specific sites in 2020?",
                       #             choices = c("National data", 
                       #                         "Specific site data")
                       # ),
                         helpText("Please note that ADDM reporting is not in line with Census standards (e.g., Asian, Native Hawaiian, and Other Pacific Islander are concatenated; ethnic origin is not distinctly reported for all races). When data are not reported, they are filled in from Census data.")

        ),
        
       #conditionalPanel(condition = "input.source == 'Autism Developmental Disabilities Monitoring Network (ADDM) - 2020' & input.addmsite == 'Specific site data'",
       #                 
       #                 checkboxGroupInput("selected_site", "Which sites(s)?", choiceNames = addmsites$NAME, choiceValues = as.numeric(addmsites$GEOID))
       #                 
       #),
        
        
        conditionalPanel(condition = "input.source == 'Decennial Census Data - 2020'",
                         
                         selectInput("geog", 
                                     label = "Would you like to utilize national data or specify a geographical region?",
                                     choices = c("National data", 
                                                 "State-level data",
                                                 "County-level data", 
                                                 "Catchment-area data")
                         )
      
        ),
        
        conditionalPanel(condition = "input.source == 'Decennial Census Data - 2020' & input.geog == 'State-level data'",
                         
                         checkboxGroupInput("selected_states", "Which state(s)?", choiceNames = states$NAME, choiceValues = as.numeric(states$GEOID))
                         
                         ),
        
        conditionalPanel(condition = "input.source == 'Decennial Census Data - 2020' & (input.geog == 'County-level data' | input.geog == 'Catchment-area data')",
                         
                         textInput("zipcode", "Enter the zipcode(s) of interest. You only need one zipcode per county, and if you are interested in more than one county, please separate them with a comma (e.g., 37069, 37212)")
                         
        ),

        conditionalPanel(condition = "input.source == 'Decennial Census Data - 2020' & input.geog == 'Catchment-area data'",
                         
                         numericInput("distance", 
                         label = "How far, in miles, from your zipcodes of interest is your catchment area?",
                         value = 50,
                         min = 1,
                         step = 1)
                         
        ),
        
        conditionalPanel(condition = "output.showmultisiteq",
                         
        selectInput("multisite_weight", 
                    label = "It appears that you will be recruiting from multiple sites. Would you like your planned enrollment table to reflect equal recruitment from all sites, or would you like the sites to be weighted according to the local population?", 
                    choices = c("Equal weights for all sites" = 1,
                                "Weight according to site population" = 2),
                    selected = 2)
 
        ),
              
        numericInput("mfpercent", 
                     label = "What proportion of your sample do you intend to be male?",
                     value = .5,
                     min = 0, max = 1,
                     step = .001),

        helpText(textOutput("ratio_text")),
        
        selectInput("ethnicity_source", 
                    label = "For the ethnic composition of your sample (i.e., percent Hispanic or Latino/a/x), would you like to use the same source or input your own ratio?",
                    choices = c("Same source",
                                "Custom"
                                )
        ),
        
        conditionalPanel(condition = "input.ethnicity_source == 'Custom'",
                         
                         numericInput("hispanic_percent", "What proportion of your sample do you intend to be Hispanic or Latino/a/x?",
                                      value = 0.1873,
                                      min = 0, max = 1,
                                      step = .0001)
        ),
        
        uiOutput("ready")
        
              ),
    
      
        # Show a plot of the generated distribution
      mainPanel(tabsetPanel(
        tabPanel("Generated Demographics Table",
          conditionalPanel(condition = "output.showstuff",
                           strong("Below is your planned enrollment table."),
                           htmlOutput('prettytable'))
        ),
        tabPanel("Raw Proportions",
                 conditionalPanel(condition = "output.showstuff",
                           strong("Below is a table of the percentages used to calculate your planned enrollment table."),
          htmlOutput('prettyproptable'),plotlyOutput("mapplots", width = "600px", height = "1400px")
        )
)
)
)
)
)

server <- function(input, output) {

  # Calculate ratio of males to females
  ratio <- reactive({
    (1-input$mfpercent)^(-1)/(input$mfpercent^(-1))
  })
  
  # Display the ratio as text
  output$ratio_text <- renderText({
    if (input$mfpercent != 1) {
    paste("Your ratio is ", format(ratio(), digits = 2), "male(s) to every 1 female")
    } else if (input$mfpercent == 1) {
    paste("Your ratio is 1 male to every 0 females")
    }
  })

  output$showmultisiteq = reactive({
    ifelse((length(input$selected_states) > 1 | length(str_split(input$zipcode, "[, ]+", simplify = T)) > 1),T,F)
    })
  outputOptions(output, "showmultisiteq", suspendWhenHidden = FALSE)
    
  output$ready <- renderUI({
    if(all(
        (input$geog == "National data" | 
        (input$geog == "State-level data" & length(input$selected_states) > 0) |
        ((input$geog == "County-level data" | input$geog == "Catchment-area data") & isTruthy(input$zipcode))),
        between(input$mfpercent,0,1), 
        ((between(input$hispanic_percent,0,1) & input$ethnicity_source == "Custom") | input$ethnicity_source == "Same source")
    )) {
    tagList(list(actionButton("estimate_demographics", "Create my demographics table"))) 
      } else {helpText("Ensure you have answered all questions completely and accurately!")}
    })
  
  output$showstuff = eventReactive(input$estimate_demographics,{TRUE})
  outputOptions(output, "showstuff", suspendWhenHidden = FALSE)
  
  # Generate all the things 
  observeEvent(input$estimate_demographics, {
    if (input$source == "Decennial Census Data - 2020") {
    if (input$geog == "National data") {
      geography = "us"
      
      estdat_out <- 
        get_decennial(geography = geography, variables = variables$name,
                                 year = 2020, summary_var = "P1_001N") %>%
                     mutate(pct = (value / summary_value))
      
        } else if (input$geog == "State-level data") {
          
          geography = "state"
         
          for (yoda in 1:length(input$selected_states)) {
            estdat_temp <- get_decennial(geography = geography, state = input$selected_states[yoda], variables = variables$name, year = 2020)
            if (yoda == 1) {estdat = estdat_temp} else {estdat = bind_rows (estdat, estdat_temp)} }
            estdat_out = estdat %>% group_by(variable) %>% summarise_at(vars(value),sum)
            estdat_out$summary_value = rep(max(estdat_out$value),nrow(estdat_out))
            
            estdat_bysite = estdat %>% group_by(variable, NAME) %>% summarise_at(vars(value),sum) %>% dplyr::rename(site = NAME)
            widedat_bysite = pivot_wider(estdat_bysite, id_cols="site", names_from="variable")
            
            uniquesites = unique(estdat_bysite$site)
        } else if ((input$geog == "County-level data" | input$geog == "Catchment-area data") & isTruthy(input$zipcode)) {
            
          geography = "county"
          zipcodes = str_split(input$zipcode, "[, ]+", simplify = T)
          coordinates = zip_code_db_jf[which(zip_code_db_jf$zipcode %in% zipcodes),]

          if (input$geog == "Catchment-area data") {
          
            for (luke in 1:nrow(coordinates)) {
              ziplist = search_radius_withcustomlist(lat = coordinates$lat[luke], lng = coordinates$lng[luke], radius = input$distance)
              site = paste(coordinates[luke,"major_city"], coordinates[luke,"state"], sep = ", ")
              ziplist$site = rep(site,nrow(ziplist))
              
              if (luke == 1) {ziplists = ziplist} else {ziplists = rbind(ziplists, ziplist)}
              }
                dattoget =  left_join((zip_code_db_jf %>%
                  filter(zipcode %in% ziplists$zipcode) %>%
                  distinct(county, state, .keep_all = T)),
                  ziplists, by = join_by(zipcode))
                
          } else {
          dattoget = coordinates
          dattoget$site = paste(coordinates$major_city, coordinates$state, sep = ", ")
          }
          
          for (han in 1:nrow(dattoget)) {
            tempdata = get_decennial(geography = geography, variables = variables$name, year = 2020, state = dattoget$state[han], county = dattoget$county[han])
            tempdata$site = rep(as.character(dattoget$site[han]),nrow(variables))
            
            if (han == 1) {estdat = tempdata
            }else {estdat = rbind(estdat, tempdata)}
          }
            
          estdat_out = estdat %>% group_by(variable) %>% summarise_at(vars(value),sum)
          estdat_out$summary_value = rep(max(estdat_out$value),nrow(estdat_out))

          estdat_bysite = estdat %>% group_by(variable, site) %>% summarise_at(vars(value),sum)
          widedat_bysite = pivot_wider(estdat_bysite, id_cols="site", names_from="variable")
          
          uniquesites = unique(dattoget$site)
        }
     if(isTruthy(estdat_out) & input$multisite_weight != 1) {
       percentage_table = generate_percentage_table_from_census(estdat_out, rownames, ethnicity_source=input$ethnicity_source, hispanic_percent = input$hispanic_percent)
       percentage_final = generate_percentage_final_from_percentage_table(percentage_table, mfpercent = input$mfpercent)
     }
     if (isTruthy(estdat_out) & exists("estdat_bysite")){
       
       percentage_table_list = list()
       percentage_final_list = list()

       for (chewy in 1:length(uniquesites)) {
         tempestdat = estdat_bysite %>% filter(site == uniquesites[chewy]) %>% ungroup()
         tempestdat$summary_value = rep(as.numeric(tempestdat[tempestdat$variable=="P1_001N","value"]), nrow(tempestdat))
         
         percentage_table_list[[chewy]] = generate_percentage_table_from_census(tempestdat, rownames, ethnicity_source=input$ethnicity_source, hispanic_percent = input$hispanic_percent)
         percentage_final_list[[chewy]] = generate_percentage_final_from_percentage_table(percentage_table_list[[chewy]], mfpercent = input$mfpercent)
       }
               
     }   
       if (input$multisite_weight != 1) {    
         percent_graph = percentage_table %>% 
           arrange(labels) %>% 
           column_to_rownames("labels") %>%
           mutate(percent_hl = hispanic_pop/sum(nonhispanic_pop,hispanic_pop), percent_nhl = nonhispanic_pop/sum(nonhispanic_pop,hispanic_pop)) %>%
           select(percent_hl, percent_nhl)
       } 
       if (exists("percentage_table_list")) {
         percent_graph_list = list()
         percent_graph_list[[1]] = "temp"
         percent_graph_all_sites = data.frame(percent_nhl = rep(0,6), percent_hl = rep(0, 6), row.names=rownames)
         
         for (chewy in 1:length(uniquesites)) {
           percent_graph_list[[chewy+1]] = percentage_table_list[[chewy]] %>% 
             arrange(labels) %>% 
             column_to_rownames("labels") %>%
             mutate(percent_hl = hispanic_pop/sum(nonhispanic_pop,hispanic_pop), percent_nhl = nonhispanic_pop/sum(nonhispanic_pop,hispanic_pop)) %>%
             select(percent_nhl, percent_hl)

           percent_graph_all_sites = percent_graph_all_sites + percent_graph_list[[chewy+1]]
         } 
         percent_graph_all_sites = percent_graph_all_sites/length(uniquesites)
         percent_graph_list[[1]] = percent_graph_all_sites
       }
      
    } else if (input$source == "Autism Developmental Disabilities Monitoring Network (ADDM) - 2020") {
      percentage_table = data.frame(percent_nhl = rep(NA,6),row.names = rownames)
      prep_nhldat_addm = pivot_longer(addmdat[,which(grepl("prop",names(addmdat)))], c(1:3,5:6), names_to="category")
      
      percentage_table["1 American Indian / Alaska Native","percent_nhl"] = prep_nhldat_addm[prep_nhldat_addm$category=="prop_AIAN","value"]
      percentage_table["4 Black or African American","percent_nhl"] = prep_nhldat_addm[prep_nhldat_addm$category=="prop_Black","value"]
      percentage_table["5 White","percent_nhl"] = prep_nhldat_addm[prep_nhldat_addm$category=="prop_White","value"]
      percentage_table["6 Multiple","percent_nhl"] = prep_nhldat_addm[prep_nhldat_addm$category=="prop_Multiple","value"]
      
      geography = "us"
      
      filldat <- 
        get_decennial(geography = geography, variables = variables$name,
                      year = 2020, summary_var = "P1_001N") %>%
        mutate(pct = (value / summary_value))
      
      race_joiners = data.frame(labels = rownames, variable = c("P1_005N", "P1_006N", "P1_007N", "P1_004N", "P1_003N", "P1_009N"))
      prep_race = inner_join(filldat,race_joiners,by="variable") %>% filter(labels %in% c("2 Asian", "3 Native Hawaiian or Other Pacific Islander"))
      ethnicity_joiners = data.frame(labels = rownames, variable = c("P2_007N", "P2_008N", "P2_009N", "P2_006N", "P2_005N", "P2_011N"))
      
      for (grogu in 1:2) {
        group = as.character(prep_race[grogu,"labels"])
        percentage_table[group,"percent_nhl"] = prep_nhldat_addm[prep_nhldat_addm$category=="prop_API","value"] * (prep_race[grogu,"value"]/sum(prep_race$value))
      }
      
      if(input$ethnicity_source == "Same source"){
        prop_hispanic = addmdat$prop_Hispanic
      } else if (input$ethnicity_source == "Custom") {prop_hispanic = input$hispanic_percent}
      
      prep_hispanic = inner_join((mutate(filldat) %>% filter(filldat$variable %in% c("P2_007N", "P2_008N", "P2_009N", "P2_006N", "P2_005N", "P2_011N")) %>% select(variable, value, summary_value)),
                                 ethnicity_joiners, by = join_by(variable))
      prep_hispanic$nonhispanic_pop = prep_hispanic$value                           
      prep_hispanic = inner_join(prep_hispanic %>% select(-value),
                                 right_join(filldat,race_joiners, join_by(variable)) %>% select(labels,value), 
                                 by = join_by(labels)) %>% arrange(labels)
      prep_hispanic$hispanic_pop = prep_hispanic$value-prep_hispanic$nonhispanic_pop
      prep_hispanic$percent_hl  = prep_hispanic$hispanic_pop / sum(prep_hispanic$hispanic_pop)
      
      prep_hispanic$percent_hl_out = prep_hispanic$percent_hl * prop_hispanic
      
      mfpercent <- input$mfpercent
      
      percentage_final = inner_join(rownames_to_column(percentage_table), 
                                    prep_hispanic %>% select(labels, percent_hl_out),
                                    by = c("rowname" = "labels")) %>%
                          column_to_rownames(var = "rowname") %>%
                          mutate(percent_hl = percent_hl_out) %>% select(-percent_hl_out)
      if (sum(percentage_final$percent_hl) != (1-sum(percentage_final$percent_nhl))) {
        current_nhlprop = sum(percentage_final$percent_nhl)
        percentage_final$percent_nhl = percentage_final$percent_nhl * ((1-prop_hispanic)/current_nhlprop)
      } 
      
      percentage_final$prop_nhl_f = (1-mfpercent)*percentage_final$percent_nhl
      percentage_final$prop_nhl_m = (mfpercent)*percentage_final$percent_nhl
      percentage_final$prop_hl_f = (1-mfpercent)*percentage_final$percent_hl
      percentage_final$prop_hl_m = (mfpercent)*percentage_final$percent_hl

      percent_graph = inner_join(rownames_to_column(percentage_table), 
                                 prep_hispanic %>% select(labels, percent_hl),
                                 by = c("rowname" = "labels")) %>% arrange(rowname) %>% 
                                 column_to_rownames("rowname")
      percent_graph$percent_hl = addmdat$prop_Hispanic * percent_graph$percent_hl
      percentage_final = percentage_final %>% select(-percent_hl, -percent_nhl) 
    }
    
if(input$multisite_weight != 1 & exists("percentage_final")) {
      final_outcounts = generate_demo_table_from_percentage_final(percentage_final, input$desiredn)

      htmltable = HTML(final_outcounts %>% 
                  addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
                  htmlTable(cgroup = c("Not Hispanic or Latino/a/x", "Hispanic or Latino/a/x"),
                  n.cgroup = c(2,2),
                  header = rep(c('Female','Male'),2),
                  rowlabel = 'Racial Categories'))    
      htmltable = str_replace(htmltable,"text-align: center;'>Racial Categories</th>", "text-align: left;'>Racial Categories</th>")
} else if (input$multisite_weight == 1 & exists("percentage_final_list")){
      desiredns = floor(rep(input$desiredn/length(uniquesites),length(uniquesites)))
      if(sum(desiredns) != input$desiredn) {
        desiredns[1] = desiredns[1] + (input$desiredn - sum(desiredns))
      } 

      html_table_list = list()
      html_table_list[1] = "temp"
      
      final_outcounts_list = list()
      for (chewy in 1:length(uniquesites)) {
        if(chewy == 1) {
          final_outcounts_list[[chewy]] = generate_demo_table_from_percentage_final(percentage_final_list[[chewy]], desiredns[chewy])
          final_outcounts = final_outcounts_list[[chewy]]} else {
        final_outcounts_list[[chewy]] = generate_demo_table_from_percentage_final_multisite(percentage_final_list[[chewy]], desiredns[chewy], final_outcounts, input$mfpercent)
        final_outcounts = final_outcounts + final_outcounts_list[[chewy]]}
        
        rowlabel = paste("Site ", chewy, ": ", uniquesites[chewy], sep="")
        
        html_table_list[chewy + 1] = HTML(final_outcounts_list[[chewy]] %>% 
                                  addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
                                  htmlTable(cgroup = c("Not Hispanic or Latino/a/x", "Hispanic or Latino/a/x"),
                                            n.cgroup = c(2,2),
                                            header = rep(c('Female','Male'),2),
                                            rowlabel = rowlabel))    
        }
                
      html_table_list[1] = HTML(final_outcounts %>% 
                         addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
                         htmlTable(cgroup = c("Not Hispanic or Latino/a/x", "Hispanic or Latino/a/x"),
                                   n.cgroup = c(2,2),
                                   header = rep(c('Female','Male'),2),
                                   rowlabel = 'Overall'))    
      html_table_list[1] = str_replace(html_table_list[1],"text-align: center;'>Overall</th>", "text-align: left;'>Overall</th>")
      htmltable = str_c(html_table_list, collapse="<p>")
      htmltable = str_replace_all(htmltable, "align: center;'>Site","align: left;'>Site") 
      }
            
      output$prettytable <- renderUI({
        HTML(htmltable)  
      })
    
if(exists("percent_graph") & input$multisite_weight != 1) {
  prop_table_out = data.frame(matrix(paste(round(c(percent_graph$percent_nhl,percent_graph$percent_hl),5)*100,"%",sep=""),
                                     ncol=2,nrow=6),
                              row.names=rownames)

  htmlproptable = HTML(prop_table_out %>% 
                     addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
                     htmlTable(cgroup = c("Not Hispanic or Latino/a/x", "Hispanic or Latino/a/x"),
                               n.cgroup = c(1,1), header = rep('Percent',2),
                               rowlabel = 'Racial Categories'))    
  htmlproptable = str_replace(htmlproptable,"text-align: center;'>Racial Categories</th>", "text-align: left;'>Racial Categories</th>")
  
}

if(exists("percent_graph_list") & input$multisite_weight == 1) {

  htmlproptablelist = list()
  
  for (chewy in 1:(length(uniquesites)+1)) {
      prop_table_out = data.frame(matrix(paste(round(c(percent_graph_list[[chewy]]$percent_nhl,percent_graph_list[[chewy]]$percent_hl),5)*100,"%",sep=""),
                                         ncol=2,nrow=6),
                                  row.names=rownames)

      title = ifelse(chewy==1,"Overall",paste("Site ", chewy-1, ": ", uniquesites[chewy-1], sep=""))
      
      htmlproptablelist[[chewy]] = HTML(
        prop_table_out %>% 
        addHtmlTableStyle(col.rgroup = c("none", "#F7F7F7")) %>% 
        htmlTable(cgroup = c("Not Hispanic or Latino/a/x", "Hispanic or Latino/a/x"),
        n.cgroup = c(1,1), header = rep('Percent',2),
        rowlabel = title))    
      htmlproptablelist[[chewy]] = str_replace(htmlproptablelist[[chewy]],paste("text-align: center;'>", title, sep=""), paste("text-align: left;'>", title, sep=""))
      
      }

  htmlproptable = str_c(htmlproptablelist, collapse="<p>")
}
      
output$prettyproptable <- renderUI({
  HTML(htmlproptable)
})   
  
if(exists("htmlproptable")) { 
  cats = c("AIAN","A","NHPI","BAA","W","M")
  intro = c("percent_nhl", "percent_hl")
  maplist_hl  = c("AIAN_hl" , "A_hl" , "NHPI_hl" , "BAA_hl" , "W_hl" , "M_hl")
  maplist_nhl = c("AIAN_nhl", "A_nhl", "NHPI_nhl", "BAA_nhl", "W_nhl", "M_nhl")
  mapvars = c(paste(intro,cats[1],sep="_"), 
              paste(intro,cats[2],sep="_"),
              paste(intro,cats[3],sep="_"),
              paste(intro,cats[4],sep="_"),
              paste(intro,cats[5],sep="_"),
              paste(intro,cats[6],sep="_"))
  pal = nfl.colors(set="eagles")[c(3,2,5,1)]
  plot_list = list()
  
    if(geography == "us") {
      max=plyr::round_any(max(c(percent_graph$percent_hl,percent_graph$percent_nhl)), .1, ceiling)
      breaks = c(0,max/2,max)
      loopnames = str_replace(rownames(percent_graph),"[123456] ","")
      
      percent_graph$cat = cats
      merge_for_national_graph = as.data.frame(matrix(c(percent_graph$percent_nhl, percent_graph$percent_hl),nrow=1,ncol=12,dimnames = list("national", c(paste("percent_nhl",percent_graph$cat,sep="_"),paste("percent_hl",percent_graph$cat,sep="_")))))

      for (padme in 1:length(mapvars)) {
        plotdeets = mapvars[padme]
        temp_merge = merge_for_national_graph %>% 
          select(all_of(plotdeets)) %>% 
          slice(rep(1, each = nrow(states_df))) %>% 
          dplyr::rename(fillvar = paste(plotdeets))
        temp_states_df = cbind(states_df, temp_merge)
        
        p = ggplot(data = temp_states_df, mapping = aes(x=x, y=y, group = group, fill = fillvar)) + 
          geom_polygon(color = "black", linewidth = 0.1) +
          scale_fill_gradientn(colors=pal, breaks = breaks, limits = c(0,max+.1), na.value="white") +
          theme_void(base_size = 12) +
          labs(#caption = title,
            fill = "Proportion") + 
          theme(plot.title = element_text(hjust = 1), 
                legend.position = "right", 
                panel.background=element_rect(colour="black", linewidth = 0.5),
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank())        
        
        u = ggplotly(p)
        
        keeptrace = numeric()
        removetrace = numeric()
        
        for  (vader in 1:length(u$x$data)) {
          if(!is.null(u$x$data[[vader]]$text)) {
            if(any(grepl("fillvar",u$x$data[[vader]]$text))) {
              u$x$data[[vader]]$text = str_replace(u$x$data[[vader]]$text, "fillvar: ", "")
              keeptrace = c(keeptrace, vader)
            } else if(any(grepl("<br",u$x$data[[vader]]$text))) {
              u$x$data[[vader]]$text = coordinate_points$post_office_city
              removetrace = c(removetrace,vader-1)
            } }
          else {removetrace = c(removetrace,vader)}
        }
        
        v = style(u, hoverinfo="none", traces = removetrace)
        
        plot_list[[padme]] = v  
        
        }
    }  

    if(geography == "state") {
      for (chewy in 1:length(uniquesites)) {
        percent_graph = percent_graph_list[[chewy+1]] %>% mutate(cats = cats, NAME = rep(uniquesites[chewy],6))
        percent_graph = inner_join(
          (pivot_wider(percent_graph,id_cols = NAME, 
                                        names_from = cats, 
                                        names_prefix = "percent_hl_",
                                        values_from = percent_hl)),
          (pivot_wider(percent_graph,id_cols = NAME, 
                       names_from = cats, 
                       names_prefix = "percent_nhl_",
                       values_from = percent_nhl)),
          by = join_by(NAME))
        if (chewy == 1) {
          percent_graph_out = percent_graph
        } else {percent_graph_out = rbind(percent_graph_out, percent_graph)}
      }

        max=plyr::round_any(max(percent_graph_out[,2:13]), .1, ceiling)
        breaks = c(0,max/2,max)
      
        states_df = inner_join(states_df, percent_graph_out, by=join_by(NAME))
        
        for (padme in 1:length(mapvars)) {
          plotdeets = mapvars[padme]
          temp_graph_out = rename(states_df,fillvar=plotdeets) 
        
          p = ggplot(data = temp_graph_out, mapping = aes(x=x, y=y, group = group, fill = fillvar)) + 
            geom_polygon(color = "black", linewidth = 0.1)
              
          p = p + theme_void() + 
             scale_fill_gradientn(colors=pal, breaks = breaks, limits = c(0,max+.1), na.value="white") +
             theme_bw(base_size = 12) +
             labs(#caption = title,
               fill = "Proportion") +
             theme(plot.title = element_text(hjust = 1), 
                   legend.position = "right", 
                   panel.background=element_rect(colour="black", linewidth = 0.5),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank())
         p
        
         u = ggplotly(p)
         
         keeptrace = numeric()
         removetrace = numeric()
         
         for  (vader in 1:length(u$x$data)) {
           if(!is.null(u$x$data[[vader]]$text)) {
             if(any(grepl("fillvar",u$x$data[[vader]]$text))) {
               u$x$data[[vader]]$text = str_replace(u$x$data[[vader]]$text, "fillvar: ", "")
               keeptrace = c(keeptrace, vader)
             } else if(any(grepl("<br",u$x$data[[vader]]$text))) {
               u$x$data[[vader]]$text = coordinate_points$post_office_city
               removetrace = c(removetrace,vader-1)
             } }
           else {removetrace = c(removetrace,vader)}
         }
         
         v = style(u, hoverinfo="none", traces = removetrace)
        
        plot_list[[padme]] = v  
        }
        }
      }  
        
    if(geography == "county") {
        
      for (yaddle in 1:length(unique(estdat$NAME))) {
        countyinfo = unique(estdat$NAME)[yaddle]
        
        tempestdat = estdat %>% filter(NAME == countyinfo) %>% ungroup()
        tempestdat$summary_value = rep(as.numeric(tempestdat[tempestdat$variable=="P1_001N","value"]), nrow(tempestdat))
        
        temppercentgraph = generate_percentage_table_from_census(tempestdat, rownames, ethnicity_source="Same source") %>% arrange(labels) %>% mutate(cats= cats, NAME=rep(countyinfo,6))
        
        percent_graph_wide = inner_join(
          (pivot_wider(temppercentgraph,id_cols = NAME, 
                       names_from = cats, 
                       names_prefix = "percent_hl_",
                       values_from = percent_hl)),
          (pivot_wider(temppercentgraph,id_cols = NAME, 
                       names_from = cats, 
                       names_prefix = "percent_nhl_",
                       values_from = percent_nhl)),
          by = join_by(NAME)) %>%
          mutate(full = str_split(countyinfo,", ")[[1]][2],
                 county = str_split(countyinfo,", ")[[1]][1],
                 region = str_to_lower(str_split(countyinfo,", ")[[1]][2]),
                 subregion = str_to_lower(str_split(countyinfo," County")[[1]][1]))
        
        if(yaddle == 1) {
          percent_graph_wide_allcounties = percent_graph_wide} else{
          percent_graph_wide_allcounties = rbind(percent_graph_wide_allcounties, percent_graph_wide)
          }
        
      }  

      fip_join = data.frame(estdat %>% distinct(GEOID, NAME))
      
      percent_graph_wide_allcounties = left_join(percent_graph_wide_allcounties,fip_join, by=join_by(NAME))
      fips = unique(percent_graph_wide_allcounties$GEOID)
      
      basemapdat = states_df[which(states_df$NAME %in% percent_graph_wide_allcounties$full),]
      coordinate_points = coordinates_for_map(coordinates)
      basemap = ggplot(data = basemapdat, mapping = aes(x = x, y = y, group = group)) + 
        geom_polygon(color = "black", fill = "white") 
      
      mapdat = counties_df[which(counties_df$fips %in% percent_graph_wide_allcounties$GEOID),]
      plotdata = inner_join(percent_graph_wide_allcounties, mapdat, by = join_by(GEOID==fips))
      
      loopnames = rep(str_replace(rownames,"[123456] ",""), 2)
      
      max=plyr::round_any(max(plotdata[,2:13]), .1, ceiling)
      breaks = c(0,max/2,max)
      annotations = list()
      y = rep((6:1/6)-.01,2)
      
        for (padme in 1:length(mapvars)) {
          plotdeets = mapvars[padme]
          temp_graph_out = rename(plotdata,fillvar=plotdeets) 
          
          t = ggplot(data = temp_graph_out, mapping = aes(x=x, y=y, group = group, fill = fillvar)) + 
            geom_polygon(color = "black", linewidth = 0.1) +
            scale_fill_gradientn(colors=pal, breaks = breaks, limits = c(0,max+.1), na.value="white") +
            theme_void(base_size = 12) +
            labs(#caption = title,
              fill = "Proportion") + 
            theme(plot.title = element_text(hjust = 1), 
                  legend.position = "right", 
                  panel.background=element_rect(colour="black", linewidth = 0.5),
                  panel.grid = element_blank()) +
            geom_polygon(data = basemapdat, mapping = aes(x = x, y = y, group = group), inherit.aes=F, color = "black", fill = NA, linewidth=0.5) +
            geom_point(data = coordinate_points, inherit.aes = F, mapping = aes(x = x, y = y), color = "#07c0f7")
          t
          u = ggplotly(t)
          
          keeptrace = numeric()
          removetrace = numeric()
          
          for  (vader in 1:length(u$x$data)) {
            if(!is.null(u$x$data[[vader]]$text)) {
              if(any(grepl("fillvar",u$x$data[[vader]]$text))) {
                u$x$data[[vader]]$text = str_replace(u$x$data[[vader]]$text, "fillvar: ", "")
                keeptrace = c(keeptrace, vader)
              } else if(any(grepl("<br",u$x$data[[vader]]$text))) {
                u$x$data[[vader]]$text = coordinate_points$post_office_city
                removetrace = c(removetrace,vader-1)
              } }
            else {removetrace = c(removetrace,vader)}
          }
          
          v = style(u, hoverinfo="none", traces = removetrace)
          v = v %>% layout(xaxis = axx, yaxis = axx)
          
          plot_list[[padme]] = v
        }
      }
    allgraphs = subplot(plot_list, nrows = 6) %>% layout(annotations = annotations, margin=list(l = 50, r = 50, b = 50, t = 100))
    output$mapplots <- renderPlotly({allgraphs})


})

}
        

  
# Create functions
generate_percentage_table_from_census <- function(estdat_out, rownames, ethnicity_source, hispanic_percent){
  P1_001N = as.numeric(estdat_out[which(estdat_out$variable=="P1_001N"), "value"])
  denominator =  P1_001N - as.numeric(estdat_out[which(estdat_out$variable=="P1_008N"), "value"])
  estdat_out$adjpercent = estdat_out$value/denominator
  
  race_joiners = data.frame(labels = rownames, variable = c("P1_005N", "P1_006N", "P1_007N", "P1_004N", "P1_003N", "P1_009N"))
  ethnicity_joiners = data.frame(labels = rownames, variable = c("P2_007N", "P2_008N", "P2_009N", "P2_006N", "P2_005N", "P2_011N"))
  prep_race = inner_join(estdat_out,race_joiners,by="variable") %>% select(value, labels, adjpercent, summary_value) 
  
  if(ethnicity_source == "Same source"){
    prop_hispanic = as.numeric(estdat_out[estdat_out$variable=="P2_002N", "value"]/P1_001N)
  } else if (ethnicity_source == "Custom") {prop_hispanic = hispanic_percent}
  
  prep_hispanic = mutate(estdat_out[estdat_out$variable %in% c("P2_007N", "P2_008N", "P2_009N", "P2_006N", "P2_005N", "P2_011N"),]) %>%
    select(variable, value) %>% dplyr::rename(nonhispanic_pop = value)

  percentage_table = inner_join(x = prep_race,
                                y = (inner_join((prep_hispanic), ethnicity_joiners, by=join_by(variable)) %>% select(-variable)),
                                by = join_by(labels))
  
  percentage_table$hispanic_pop = percentage_table$value-percentage_table$nonhispanic_pop
  percentage_table$percent_hl  = percentage_table$hispanic_pop / sum(percentage_table$hispanic_pop)
  percentage_table$percent_nhl = percentage_table$nonhispanic_pop / sum(percentage_table$nonhispanic_pop)
  percentage_table$percent_hl_est  = prop_hispanic *percentage_table$percent_hl
  
  ##ADJUST PROPORTIONS SO RACIAL COMPOSITION DOES NOT CHANGE AS A RESULT OF THE ETHNIC COMPOSITION
  percentage_table$percent_hl_out = ifelse(percentage_table$adjpercent-(percentage_table$percent_hl_est)<=0,
                                           percentage_table$adjpercent,
                                           percentage_table$percent_hl_est)
  while (round(sum(percentage_table$percent_hl_out),sapply(prop_hispanic, nchar) - 2) != prop_hispanic) {
    not_full = (percentage_table$adjpercent > percentage_table$percent_hl_out) 
    diff_hl = prop_hispanic - sum(percentage_table$percent_hl_out)
    extra_percent = percentage_table[not_full,] %>% select(hispanic_pop) %>% mutate(newprop = hispanic_pop/sum(hispanic_pop))
    extra_percent$add_prop = extra_percent$newprop*diff_hl
    percentage_table[not_full,"percent_hl_out"] = percentage_table[not_full,"percent_hl_out"] + extra_percent$add_prop
    
    if (all(percentage_table$adjpercent-percentage_table$percent_hl_out>=0)) {break} else {
      percentage_table$percent_hl_out = ifelse(percentage_table$adjpercent-(percentage_table$percent_hl_out)<=0,
                                               percentage_table$adjpercent,
                                               percentage_table$percent_hl_out)
    }
  }
  
  percentage_table$percent_nhl_out = percentage_table$adjpercent - percentage_table$percent_hl_out
  percentage_table = data.frame(percentage_table)
  
  return(percentage_table)
  }

generate_percentage_final_from_percentage_table <- function(percentage_table, mfpercent){
percentage_final = percentage_table %>% select(labels, percent_hl_out, percent_nhl_out) %>% arrange(labels)
percentage_final$prop_nhl_f = (1-mfpercent)*percentage_final$percent_nhl_out
percentage_final$prop_nhl_m = (mfpercent)*percentage_final$percent_nhl_out
percentage_final$prop_hl_f = (1-mfpercent)*percentage_final$percent_hl_out
percentage_final$prop_hl_m = (mfpercent)*percentage_final$percent_hl_out

percentage_final = percentage_final %>% select(-percent_hl_out, -percent_nhl_out) %>% column_to_rownames(var = "labels")
return(percentage_final)
}

generate_demo_table_from_percentage_final <- function(percentage_final, desiredn) {    
  raw_counts = percentage_final*desiredn
  initial_outcounts = round(raw_counts,0)
  
  error = desiredn - sum(initial_outcounts)
  
  if (error > 0) {   
    rounddiffs = raw_counts-initial_outcounts
    if (sum(ceiling(rounddiffs)) >= error) {
      for (leia in seq(0,1,.0001)) {
        add_nums = 1*(rounddiffs > leia)
        if (sum(add_nums) == error) {break}
        if (sum(add_nums) < error) {
          prior_nums = 1*(rounddiffs > (leia-.001))
          remainders = prior_nums - add_nums
          
          for (lola in 1:length(remainders)) {
            temp_add_nums = add_nums
            temp_add_nums[lola] = temp_add_nums[lola] + remainders[lola]
            
            if(sum(temp_add_nums) == error) {
              add_nums = temp_add_nums
              break
            }
          }
          break}}
    } else if (sum(ceiling(rounddiffs)) < error) {
      add_nums = ceiling(rounddiffs)
      left = error - sum(add_nums)
      extranums = left*percentage_final
      for (leia in seq(0,1,.001)) {
        extra_add_nums = 1*(rounddiffs > leia)
        if (sum(extra_add_nums) == left) {break} }
      add_nums = add_nums + extra_add_nums
    }
    final_outcounts = initial_outcounts+add_nums
  } else if (error == 0) {
    final_outcounts = initial_outcounts
  } else if (error < 0) {
    floor = floor(raw_counts)
    rounddiff = desiredn - sum(floor)
    rounddiffs = raw_counts - floor(raw_counts)
    for (leia in seq(0,.99,.001)) {
      add_nums = 1*(rounddiffs > leia)
      if (sum(add_nums) == rounddiff) {break}
    }
    if (sum(add_nums) != rounddiff) {
      for (leia in rev(seq(0.1,.99,.001))) {
        add_nums = 1*(rounddiffs > leia)
        if (sum(add_nums) >= rounddiff) {
          add_nums = 1*(rounddiffs > (leia+.001))
          break} }
      left = desiredn - sum(add_nums) - sum(floor) 
      remainders = rounddiffs-add_nums
      remainders$race = row.names(remainders)
      remainders = pivot_longer(remainders, cols=1:4, names_to="col") 
      for (ben in 1:left) {
        row_to_add = which.max(remainders$value)
        to_add = remainders[row_to_add,]
        add_nums[to_add$race,to_add$col] = add_nums[to_add$race,to_add$col] + 1
        remainders[row_to_add,"value"] = 0
      }
    } 
    final_outcounts = floor+add_nums    
  }
return(final_outcounts)
}

generate_demo_table_from_percentage_final_multisite <- function(percentage_final, desiredn, final_outcounts, mfpercent) {    
  raw_counts = percentage_final*desiredn
  initial_outcounts = round(raw_counts,0)
  
  error = desiredn - sum(initial_outcounts)
  
  checkmf = sum(initial_outcounts[,c("prop_nhl_m","prop_hl_m")],final_outcounts[,c("prop_nhl_m","prop_hl_m")])/sum(initial_outcounts,final_outcounts)
  
  if (error > 0) {   
    rounddiffs = raw_counts-initial_outcounts
    if (sum(ceiling(rounddiffs)) >= error) {
      for (leia in seq(0,1,.0001)) {
        add_nums = 1*(rounddiffs > leia)
        
        if (sum(add_nums) <= error) {break} }
        if (sum(add_nums) < error) {
          prior_nums = 1*(rounddiffs > (leia-.001))
          temp_add_nums = add_nums
          remainders = prior_nums - add_nums
          
          if (checkmf == mfpercent) {
            
            for (lola in 1:length(remainders)) {
            temp_add_nums[lola] = temp_add_nums[lola] + remainders[lola]
            
            if(sum(temp_add_nums) == error) {
              add_nums = temp_add_nums
              break
          } 
            } 
            }
          else  {
            textstring = ifelse(checkmf > mfpercent, "_f", "_m")
            remainders = data.frame(remainders[,which(grepl(textstring,dimnames(remainders)[[2]]))])
            for (lola in 1:12) {
              colname = dimnames(remainders)[[2]][ifelse(lola <= 6, 1, 2)]
              rowname = rep(dimnames(remainders)[[1]],2)[lola]
              temp_add_nums[rowname,colname] = temp_add_nums[rowname,colname] + (remainders[rowname,colname])
              
              if(sum(temp_add_nums) == error) {
                add_nums = temp_add_nums
                break
              }
            }
            }
        }
      }else if (sum(ceiling(rounddiffs)) < error) {
      add_nums = ceiling(rounddiffs)
      left = error - sum(add_nums)
      extranums = left*percentage_final
      for (leia in seq(0,1,.001)) {
        extra_add_nums = 1*(rounddiffs > leia)
        if (sum(extra_add_nums) == left) {break} }
      add_nums = add_nums + extra_add_nums
    }
    final_outcounts = initial_outcounts+add_nums
  } 
  
  if (error == 0) {
    final_outcounts = initial_outcounts
  } 
  
  if (error < 0) {
    floor = floor(raw_counts)
    rounddiff = desiredn - sum(floor)
    rounddiffs = raw_counts - floor(raw_counts)
    for (leia in seq(0,.99,.001)) {
      add_nums = 1*(rounddiffs > leia)
      if (sum(add_nums) == rounddiff) {break}
    }
    if (sum(add_nums) != rounddiff) {
      for (leia in rev(seq(0.1,.99,.001))) {
        add_nums = 1*(rounddiffs > leia)
        if (sum(add_nums) >= rounddiff) {
          add_nums = 1*(rounddiffs > (leia+.001))
          break} }
      left = desiredn - sum(add_nums) - sum(floor) 
      remainders = rounddiffs-add_nums
      remainders$race = row.names(remainders)
      remainders = pivot_longer(remainders, cols=1:4, names_to="col") 
      for (ben in 1:left) {
        row_to_add = which.max(remainders$value)
        to_add = remainders[row_to_add,]
        add_nums[to_add$race,to_add$col] = add_nums[to_add$race,to_add$col] + 1
        remainders[row_to_add,"value"] = 0
      }
    } 
    final_outcounts = floor+add_nums    
  }
  
  return(final_outcounts)
}

coordinates_for_map <- function(coordinates) {
  xmin = -67.945404
  ymin = 17.88328
  xmax = -65.220703
  ymax = 18.515683
  
  for (bb8 in 1:nrow(coordinates)) {
    if (reverse_zipcode(coordinates[bb8, "zipcode"])$state == "PR") {
      tempcoord = coordinates[bb8,]
      tempcoord$x = (((coordinates[bb8, "lng"] - xmin)/(xmax - xmin)) * 1000000*(3/5)) + 500000
      tempcoord$y = ((((coordinates[bb8, "lat"] - ymin)/(ymax - ymin))*0.2321) * 1000000*(3/5)) - 2200000
    }
    else {
      tempcoord = usmap_transform(data=coordinates[bb8,], input_names=c("lng","lat")) 
    }
    if (bb8 == 1) {
      outcoordinates = tempcoord
    } else {
      outcoordinates = rbind(outcoordinates, tempcoord)
    }
  }
  return(outcoordinates)
}

search_radius_withcustomlist <- function(lat, lng, radius = 1) {
  # Source: zipcodeR
  # Code for function obtained from: https://rdrr.io/cran/zipcodeR/src/R/zip_lookups.r

  # Create an instance of the ZIP code database for calculating distance,
  # filter to those with lat / lon pairs
  zip_data <- zip_code_db_jf %>%
    dplyr::filter(lat != "NA")
  
  # Calculate the distance between all points and the provided coordinate pair
  for (i in seq_len(nrow(zip_data))) {
    zip_data$distance[i] <- raster::pointDistance(c(lng, lat), c(zip_data$lng[i], zip_data$lat[i]), lonlat = TRUE)
  }
  
  # Convert meters to miles for distance measurement
  zip_data$distance <- zip_data$distance * 0.000621371
  
  # Get matching ZIP codes within specified search radius
  result <- zip_data %>%
    # Filter results to those less than or equal to the search radius
    dplyr::filter(distance <= radius) %>%
    dplyr::select(zipcode, distance) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(distance)
  
  # Warn if there is nothing found
  if (nrow(result) == 0) {
    warning(paste("No ZIP codes found for coordinates", paste0(lat, ",", lng), "with radius", radius, "mi"))
  }
  return(result)
}

# Run the application 
shinyApp(ui = ui, server = server)

  
