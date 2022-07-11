# -------------------------------------------------------------------------------------------------------------------------------
# Set Up
library(shiny)
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library (DT) # for data tables
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(rmarkdown)
library(flextable) #for tech document table
library(webshot) #to download plotly charts
library(rintrojs)
library(shinyhelper)
library(hubspot)
library(jsonlite)
library(rlist)
library(httr)
library(tidyverse)
library(vroom)
options(scipen=999)
# -------------------------------------------------------------------------------------------------------------------------------
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}


###############################################.
## Functions ----
###############################################.  

conOptionFunc <- function(x) {
  c <- list()
  for(i in 1:length(x)){
    name <- x[i] %>% list.select(name) %>% bind_rows()
    opt_value <- x[[i]]$options %>% list.select(value) %>% list.stack()
    ee <- tibble(name = name$name, opt_value = opt_value$value)
    c[[i]] <- ee}
  
  c <- c %>% bind_rows() %>% na.omit() %>% group_by(name) %>% mutate(rank = rank(opt_value)) %>% arrange(name, opt_value) %>% filter(rank <= 20)
  c <- c %>% group_by(name) %>% summarise(opt_value = paste(opt_value, collapse = '; ')) %>% rename(`HubSpot Internal Name` = name, `Options` = opt_value)
  return(c)}

miliSecond <- function(y){
  a <- as.Date(y)
  a <- as.POSIXct(a, tz = 'GMT')
  a <- as.numeric(a) * 1000
  a <- as.character.POSIXt(a) %>% trimws()
  a <- ifelse(a=='NA','',a)
  return(a)}

addCompanyFunction <- 
  function(x,y,z){
    aa <- list()
    for (i in 1:nrow(x)) {
      bb <- pivot_longer(x[i,], cols = -c('hubsBridgeStatus'), names_to = 'name', values_to = 'value', values_drop_na = T) %>% select(-1) %>% filter(value != '')
      cc <- toJSON(list(properties = bb), pretty = T, auto_unbox = T)
      dd <- POST(url = paste0('https://api.hubapi.com/companies/v2/companies?hapikey=', z),
                 body = cc,
                 add_headers(.headers = c("Content-Type"="application/json"))) %>% status_code()
      ee <- cbind(updateStatus = dd, y[i, ])# %>% mutate(updateStatus = ifelse(updateStatus == 202, 'Successful', 'Failed'))
      aa[[i]] <- ee
    }
    aa <- bind_rows(aa)
    return(aa)
  }

## JavaScript that dis/enables the ABILITY to click the tab (without changing aesthetics)
app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }"
## css snipit that makes it LOOK like we are/n't able click the tab (with outchanging functionality)
app_css <-
  ".nav li a.disabled {
    background-color: #aaa !important;
    color: #ebecf0 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  }"

isValidEmail <- function(x) {
  grepl("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$", as.character(x), ignore.case=TRUE)
}

# Selects the variables of interest and renames them so csv downloads are 
# more user friendly
format_csv <- function(reactive_dataset, extra_vars = NULL ) {
  
  techdoc <- techdoc %>%
    select(indicator_name, data_source)
  
  left_join(reactive_dataset, techdoc, by = c("indicator" = "indicator_name")) %>%
    select_at(c("indicator", "areaname", "code", "areatype", "year", "def_period", "numerator", "measure",
                "lowci", "upci", extra_vars, "type_definition", "data_source")) %>%
    rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
           period = def_period, definition = type_definition, area_code=code, area_name=areaname,area_type=areatype)
}

format_definitions_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    select(c(profile, domain,indicator_name, indicator_number, indicator_definition, inclusion_rationale, data_source,
             diagnostic_code_position, numerator, denominator, measure, disclosure_control, rounding, age_group, sex, year_type,
             trends_from, aggregation, update_frequency, available_geographies, confidence_interval_method, notes_caveats, 
             related_publications, supporting_information, last_updated, next_update
    )) 
}

#Download button for charts, just changing the icon
savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled=FALSE){
  
  if (disabled == TRUE){
    
    # Message to display when disabled button is clicked
    disabled_msg = list(p("A software update has disabled the save chart functionality. We are working on a replacement."),
                        p("In the meantime, you can:"),
                        tags$ul(
                          tags$li("Download the data with the Download data button and build new charts in tools like Excel"),
                          tags$li("Take a screenshot of the chart area using ",
                                  tags$a(href="https://support.microsoft.com/en-us/windows/open-snipping-tool-and-take-a-screenshot-a35ac9ff-4a58-24c9-3253-f12bac9f9d44",
                                         "Snipping Tool"),
                                  " or ",
                                  tags$a(href="https://blogs.windows.com/windowsexperience/2019/04/08/windows-10-tip-snip-sketch/",
                                         "Snip and Sketch."),
                                  "At least one of these tools is usually installed on recent versions of Windows."
                          )))
    
    # Create button without link
    disabled_button = tags$p(id = outputId, class = paste("btn btn-default shiny-download-link", class, "down_disabled"),
                             icon("image"), label)
    
    # Define popup message box
    disabled_popup = bsModal(paste0(outputId, "-disabled-modal"), "Save Chart Disabled", outputId, disabled_msg, size="small")
    
    # need to explicitly return both ui elements otherwise only the last will be returned
    return(tagList(disabled_button, disabled_popup))
    
    
  } else {
    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon("image"), label)
  }
  
  
}

#Function to wrap titles, so they show completely when saving plot in ggplot
title_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to create plot when no data available
plot_nodata <- function(height_plot = 450) {
  text_na <- list(x = 5, y = 5, text = "No data available" , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 

#Function to create plot when no data available for ggplot visuals
plot_nodata_gg <- function() {
  ggplot()+
    xlab("No data available")+
    scale_x_discrete(position = "top")+
    theme(panel.background = element_blank(),
          axis.title.x=element_text(size=20, colour ='#555555'))
}


# UI for heatmap and snapshot plots
sum_ui <- function(title, plot_name) {
  tagList(
    h5(title, style="color: black; text-align: center; font-weight: bold;"),
    div(align = "center", withSpinner(plotlyOutput(plot_name, height = "auto")))
  ) }

# Indicator definition boxes for indicator definition tab
ind_def_box <- function(label, text_output) {
  div(class="definitionbox",
      p(paste(label), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
      h5(style = "color: black", textOutput(text_output)))
}

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
  )
}


#Creating small boxes for further information in the landing page (see ui for formatting css)
lp_about_box <- function(title_box, image_name, button_name, description) {
  
  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      (actionButton(button_name, NULL,
                    class="landing-page-button",
                    icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}

###############################################.
## Data ----
###############################################.    
optdata <- readRDS("data/optdata.rds") #main dataset
depr_data <- readRDS("data/deprivation_data.rds") #deprivation/inequalities dataset
techdoc <- readRDS("data/techdoc.rds") #technical documents data including definitions

geo_lookup <- readRDS("data/geo_lookup.rds") #geography lookup
profile_lookup <- readRDS("data/profile_lookup.rds") #profile lookup

#Map-shapefile data
ca_bound<-readRDS("data/CA_boundary.rds") #Council area 
hb_bound<-readRDS("data/HB_boundary.rds") #Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds") #HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") #HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") #Intermediate zone

###############################################.
## Names ----
###############################################.   
#Geographies names
area_list <- sort(geo_lookup$areaname)
comparator_list <- sort(geo_lookup$areaname[geo_lookup$areatype %in% 
                                              c("Health board", "Council area", "Scotland")]) 
code_list <- unique(optdata$code)
parent_geo_list <- c("Show all", sort(as.character((unique(optdata$parent_area))[-1])))
parent_iz_list <- geo_lookup %>% filter(areatype=="Intermediate zone") %>% select(areaname,parent_area)
parent_hscl_list <- geo_lookup %>% filter(areatype=="HSC locality") %>% select(areaname,parent_area)
hb_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Health board"]) 
la_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Council area"]) 
adp_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Alcohol & drug partnership"]) 
intzone_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Intermediate zone"]) 
partnership_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC partnership"]) 
locality_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC locality"]) 
adp_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Alcohol & drug partnership"])

#year of indicators
min_year <- min(optdata$year)
max_year <- max(optdata$year)

#Area type names
areatype_list <- c("Alcohol & drug partnership", "Council area", "Health board",  
                   "HSC locality", "HSC partnership",  "Intermediate zone", "Scotland")
areatype_noscot_list <- c("Alcohol & drug partnership", "Council area", "Health board",  
                          "HSC locality", "HSC partnership",  "Intermediate zone")
areatype_depr_list <- c("Scotland", "Health board", "Council area") #for deprivation tab

#Indicator names
indicator_list <- sort(unique(optdata$indicator))
indicator_map_list <- sort(unique(optdata$indicator[optdata$interpret != 'O']))
indicators_updated <- techdoc %>% filter(days_since_update<60) %>% pull(indicator_name)
ind_depr_list <- sort(unique(depr_data$indicator)) #list of indicators
# Hsc deprivation indicators
ind_hsc_list <- c("Preventable emergency hospitalisation for a chronic condition",
                  "Repeat emergency hospitalisation in the same year",
                  "Mortality amenable to health care",                            
                  "All-cause premature mortality",
                  "Dying in hospital", "Mortality amenable to health care")

#Profile names
topic_list_filter <- as.factor(c("Show all",unique(sort(c(
  substr(optdata$profile_domain1, 5, nchar(as.vector(optdata$profile_domain1))), 
  substr(optdata$profile_domain2, 5, nchar(as.vector(optdata$profile_domain2)))))))) 

depr_measure_types <- c("Trend", "Gap", "Risk") #list of measure types

topic_list <- topic_list_filter[-1] #taking out show all from list

profile_list <- setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                         c('Health & wellbeing','Children & young people','Alcohol',
                           'Drugs','Mental Health', "Tobacco control", "Population"))

profile_list_filter <-c(setNames("Show all", "Show all"), sort(profile_list))

#Geography types available for each indicator
areatype_profile <- list(
  'Health & wellbeing' = c("Council area", "Health board", "HSC locality", 
                           "HSC partnership",  "Intermediate zone", "Scotland"),
  'Children & young people' = c("Council area", "Health board", "HSC locality", 
                                "HSC partnership",  "Intermediate zone", "Scotland"),
  'Alcohol' = c("Alcohol & drug partnership", "Council area", "Health board", "Scotland"),
  'Drugs' = c("Alcohol & drug partnership", "Council area", "Health board", "Scotland"),
  'Mental Health' = c("Council area", "Scotland"),
  "Tobacco control" = c("Council area", "Health board", "Scotland"), 
  "Population" = c("Alcohol & drug partnership", "Council area", "Health board",  
                   "HSC locality", "HSC partnership",  "Intermediate zone", "Scotland") 
)

profile_areatype <- list(
  "Scotland" = setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                        c('Health & wellbeing','Children & young people','Alcohol',
                          'Drugs','Mental Health', "Tobacco control", "Population")),
  "Health board" = setNames(c('HWB','CYP','ALC','DRG', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol',
                              'Drugs', "Tobacco control", "Population")),
  "Council area" = setNames(c('HWB','CYP','ALC', "DRG",'MEN', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol', "Drugs",
                              'Mental Health', "Tobacco control", "Population")),
  "HSC partnership" = setNames(c('HWB','CYP', "POP"),
                               c('Health & wellbeing','Children & young people', "Population")),
  "HSC locality" = setNames(c('HWB','CYP', "POP"),
                            c('Health & wellbeing','Children & young people', "Population")),
  "Intermediate zone" = setNames(c('HWB','CYP', "POP"),
                                 c('Health & wellbeing','Children & young people', "Population")),
  "Alcohol & drug partnership" = setNames(c('ALC','DRG', "POP"),
                                          c('Alcohol','Drugs', "Population"))
)


###############################################.
## Palettes ----
###############################################.   
#Palette for SIMD.
pal_simd_bar <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
pal_simd_trend <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031', '#FF0000')

#Palette for map
pal_map <- c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c')

##########.
#Cookie warning
cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0",
                  "This website places cookies on your device to help us improve our service 
      to you. To find out more, see our ",
                  tags$a(href='https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies',
                         " Privacy and Cookies"), "statement.",
                  HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'))

###############################################.
## Plot parameters ----
###############################################.

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                    tickfont = list(size=14), titlefont = list(size=14)) 

font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')


# ---------------------------------------------------------------------------------------------------------------------------------------
ui <- shiny::fluidPage(
  #Code to create ScotPHO's Shiny profile platform
  # This script includes the user-interface definition of the app.
  
  ###############################################.
  ## Header ---- 
  ###############################################.
  tagList( #needed for shinyjs
    useShinyjs(),  # Include shinyjs
    introjsUI(),   # Required to enable introjs scripts
    shinyjs::extendShinyjs(text = app_jscode, functions = c('disableTab','enableTab')),
    shinyjs::inlineCSS(app_css),
    
    tags$style(HTML(".tabbable > .nav > li[class=active] > a {background-color: #EBECF0;color: #000;}")),
    navbarPage(id = "intabset", #needed for landing page
               title = div(tags$a(img(src="Hubs Bridge Logo.png", width = 50, height = 50), 
                                  href= "https://www.linkedin.com/in/ckkusuma"),
                           style = "position: center;max-width: 100%; and height: auto;"), # Navigation bar
               windowTitle = "Hubs Bridge", #title for browser tab
               theme = shinytheme('cerulean'),
               # position = c("fixed-top"),
               collapsible = TRUE, #tab panels collapse into menu in small screens
               header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                  HTML("<html lang='en'>"),
                                  tags$link(rel="shortcut icon", 
                                            href="Hubs Bridge Logo.png"), #Icon for browser tab
                                  #Including Google analytics
                                  #includeScript("google-analytics.js"),
                                  HTML("<base target='_blank'>"),
                                  cookie_box),
               ###############################################.
               ## Landing page ----
               ###############################################.
               tabPanel(
                 title = " Home", value = 'home', icon = icon("home"),
                 mainPanel(width = 11, style="margin-left:4%; margin-right:4%; margin-bottom:1%; margin-top:1%;",
                           fluidRow(HTML('<h1 style = "text-align:center;color:grey;">Still managing <span style = "color:#f57722;">HubSpot</span> and <span style = "color:#67aded;">DOMO</span> manually? 
                                         Try <span style = "color:#ff781f;">Hubs</span><span style = "color:black;">-Bridge</span> for FREE! </h1>')), 
                           fluidRow(column(offset = 1, width = 5, 
                                           div(class = 'landing-page-words', 
                                               div(class = "landing-page-icon", img(src="Hubs Bridge Logo.png", style = 'align: center; max-width: 100%; and height: auto;')),
                                               actionButton(inputId = 'hubsBridgeLogoButton', NULL, class="landing-page-button"))),
                                    column(width = 5,
                                           div(class = 'landing-page-words',
                                               HTML('<h3 style = "text-align:center;color:grey; margin-top: 25%;"><span style = "color:#ff781f;">Hubs</span><span style = "color:black;">-Bridge</span> 
                                           is a FREE online tool to automate updates on <span style = "color:#f57722;">HubSpot</span> objects such as <span style = "text-decoration: underline;font-style: italic;">Contact</span>, 
                                           <span style = "text-decoration: underline;font-style: italic;">Deal</span>, and <span style = "text-decoration: underline;font-style: italic;">Company</span>.</h3>'),
                                               actionButton(inputId = 'hubspotButton4', NULL, class="landing-page-word-button")),
                                           br(),
                                           div(class = 'landing-page-words',
                                               HTML('<h3 style = "text-align:center;color:grey;"><span style = "color:#ff781f;">Hubs</span><span style = "color:black;">-Bridge</span> also provides FREE tools to automate 
                                           manual processes on <span style = "color:#67aded;">DOMO</span> such as managing <span style = "text-decoration: underline;font-style: italic;">PDP</span>, 
                                           <span style = "text-decoration: underline;font-style: italic;">User Information</span>, and <span style = "text-decoration: underline;font-style: italic;">Group</span>.</h3>'),
                                               actionButton(inputId = 'domoButton4', NULL, class="landing-page-word-button")))),
                           fluidRow(
                             column(offset = 2, width = 2, 
                                    div(class = 'landing-page-words',
                                        div(class = "landing-page-icon", img(src='hubspot logo.jpeg', style = 'max-width: 100%; height: auto;')),
                                        actionButton(inputId = 'hubspotLogoButton', NULL, class="landing-page-button"))),
                             column(width = 1, style = 'margin-left:50px; margin-top:-10px;',
                                    fluidRow(div(class = 'landing-page-words',
                                                 h1('Contact', style = "color: #ffc04c;"),
                                                 actionButton(inputId = 'hubspotButton1', NULL, class="landing-page-word-button")),
                                             br(),
                                             div(class = 'landing-page-words',
                                                 h1('Deal', style = "color: #ffc04c;"),
                                                 actionButton(inputId = 'hubspotButton2', NULL, class="landing-page-word-button")),
                                             br(),
                                             div(class = 'landing-page-words',
                                                 h1('Company', style = "color: #ffc04c;"),
                                                 actionButton(inputId = 'hubspotButton3', NULL, class="landing-page-word-button")))),
                             column(offset = 2, width = 2, 
                                    div(class = 'landing-page-words',
                                        div(class = "landing-page-icon", img(src='domo logo.png', style = 'max-width: 100%; height: auto;')),
                                        actionButton('domoLogoButton', NULL, class="landing-page-button"))),
                             column(width = 1, style = 'margin-left:50px; margin-top:-10px;',
                                    fluidRow(div(class = 'landing-page-words', 
                                                 h1('PDP', style = "color: #B7DAF5;"),
                                                 actionButton(inputId = 'domoButton1', NULL, class="landing-page-word-button")),
                                             br(),
                                             div(class = 'landing-page-words',
                                                 h1('User', style = "color: #B7DAF5;"),
                                                 actionButton(inputId = 'domoButton2', NULL, class="landing-page-word-button")),
                                             br(),
                                             div(class = 'landing-page-words',
                                                 h1('Group', style = "color: #B7DAF5;"),
                                                 actionButton(inputId = 'domoButton3', NULL, class="landing-page-word-button"))))),
                           br()
                 )),
               ###############################################.
               ## searchHubs
               ###############################################.
               tabPanel("HubSpot", icon = img(src="hubspot icon.webp", width = 17), value = "HubSpot",
                        mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
                                  wellPanel(
                                    fluidRow(column(3, div(fluidRow(passwordInput('hubspot_api_key', 'HubSpot API Key', value = '', placeholder = 'HubSpot API Key or Enter "demo" As A Test', width = 400)), style = "margin-left:15px;"), style = 'margin-left: 15px;'),
                                             column(1, actionButton("hubsAuthModalButton", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-top:20px;'))),
                                    div(fluidRow(column(1, disabled(actionButton('hubspot_submit_button', label = 'Submit', style = 'margin-right:0px;'))), column(1, textOutput('hubspot_authentication'), style = 'margin-top:12px;')), style = "margin-left:20px;"),
                                    style = "background: #FCCF84"),
                                  hr(),
                                  tabsetPanel(id = 'HubSpotPagePanel',
                                              tabPanel('Authentication', icon = img(src = 'key icon.png', width = 25),
                                                       br(),
                                                       h4('Why API Key?', style = 'text-decoration: underline; color: #ff8b3d;'),
                                                       p('Hubs Bridge makes API calls to your HubSpot instance for retrieving and updating HubSpot objects such as Contact, Deal, and Company.'),
                                                       br(), br(),
                                                       h4('How to get an API Key?', style = 'text-decoration: underline; color: #ff8b3d;'),
                                                       h5('Requirements:', style = 'color: #ff8b3d;'),
                                                       tags$ul(
                                                         tags$li('A HubSpot instance'),
                                                         tags$li('Super admin permissions')),
                                                       h5('Steps to generate an API key:', style = 'color: #ff8b3d;'),
                                                       tags$ol(
                                                         tags$li('In your HubSpot account, click the ',tags$strong('setting'), ' icon in the main navigation bar'),
                                                         tags$li('Navigate to ', tags$strong('integration')),
                                                         tags$li('API Key'),
                                                         tags$li('Generate API Key'),
                                                         tags$li(tags$a('Click here', style = 'text-decoration: none;', href = 'https://knowledge.hubspot.com/integrations/how-do-i-get-my-hubspot-api-key#:~:text=Access%20your%20API%20key&text=In%20your%20HubSpot%20account%2C%20click,Show%20to%20display%20your%20key.'), 
                                                                 'for documentation on HubSpot')),
                                                       br(), br(),
                                                       h4('What about data security?', style = 'text-decoration: underline; color: #ff8b3d;'),
                                                       p('Your data security is first and foremost.'),
                                                       p('Your API Key is only stored during the session which will be deleted upon refresh or when a new session is initiated.'),
                                                       p(tags$strong('OAuth 2.0'), 'for alternate authentication is ', tags$strong('under development'),' which will enable you to grant specific permissions to Hubs Bridge.'),
                                                       br(),br(),
                                                       h4('What is OAuth 2.0?', style = 'text-decoration: underline; color: #ff8b3d;'),
                                                       p('OAuth 2.0 allows a user to authorize an app to work with specific tools in their HubSpot account, designated by the authorization scopes you set.'),
                                                       p('We will let you know when this feature is available.', tags$a('Click here', style = 'text-decoration: none;', href ='https://legacydocs.hubspot.com/docs/methods/oauth2/oauth2-overview'), 'for more information about OAuth 2.0.')
                                              ),
                                              
                                              ## serchContact
                                              tabPanel(title = 'Update Contact', value = 'update_contact', icon = img(src = 'contact icon 2.png', width = 20),
                                                       br(),
                                                       h3('Current Properties:', style = 'color: #ff8b3d;'),
                                                       actionButton('show_cur_properties', 'Show Current Properties', style = 'background: #FCCF84; margin-bottom: 20px;'),
                                                       shinyjs::hidden(actionButton('hide_cur_properties', 'Hide Current Properties', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       shinyjs::hidden(downloadButton(outputId = 'hubsCurrentConData', label = 'Download Current Properties', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       br(),
                                                       shinyjs::hidden(htmlOutput('hubsConCurPropExp')),
                                                       br(),
                                                       shinyjs::hidden(dataTableOutput('cur_contact_properties')),
                                                       br(),
                                                       hr(),
                                                       br(),
                                                       h3('Add/Update Contacts:', style = 'color: #ff8b3d;'),
                                                       fluidRow(column(1, h5('Upload Contacts', style = 'color: #000;'), style = 'margin-left:10px; margin-bottom: -10px;', style = 'margin-right:-10px;'), 
                                                                column(1, actionButton("contactUploadInfo", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-left:-10px;'))),
                                                       fileInput(inputId = 'contactUpload', label = NULL, multiple = FALSE, accept = '.csv', buttonLabel = 'Browse', placeholder = 'No File Selected'),
                                                       tags$style(".btn-file {background: #FCCF84;} .progress-bar {background: #ff8b3d;}"),
                                                       shinyjs::hidden(htmlOutput('hubsConUpldPropExp')),
                                                       br(),
                                                       withSpinner(DT::dataTableOutput('prev_uploadedContact'), type = 6),
                                                       shinyjs::hidden(actionButton('updateAllContactWarning', 'Ready to Update', style = 'background: #EA4C46; color: #fff;')),
                                                       shinyjs::hidden(downloadButton(outputId = 'hubsPushedConData', label = 'Download Pushed Data', style = 'background: #FCCF84;')),
                                                       br(),
                                                       shinyjs::hidden(actionButton('conShowJSON', 'Show JSON', style = 'background: #d9dddc; margin-top:20px;')), 
                                                       shinyjs::hidden(actionButton('conHideJSON', 'Hide JSON', style = 'background: #d9dddc; margin-top:20px;')),
                                                       textOutput('json_output')
                                                       # br(),
                                                       # hr(),
                                                       # br(),
                                                       # h3('Delete Contacts:', style = 'color: #ff8b3d;'),
                                                       # fluidRow(column(1, h5('Upload Contacts', style = 'color: #000;'), style = 'margin-left:10px; margin-bottom: -10px;', style = 'margin-right:-10px;'), 
                                                       #          column(1, actionButton("contactDeleteInfo", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-left:-10px;'))),
                                                       # fileInput(inputId = 'contactDeleteUpload', label = NULL, multiple = FALSE, accept = '.csv', buttonLabel = 'Browse', placeholder = 'No File Selected'),
                                                       # shinyjs::hidden(htmlOutput('hubsConDelUpldPropExp'))
                                              ),
                                              tabPanel(title = 'Update Deals', value = 'update_deal', icon = img(src = 'deal icon.png', width = 25)),
                                              
                                              # searchCompany
                                              tabPanel(title = 'Update Company', value = 'update_company', icon = img(src = 'company icon.png', width = 25),
                                                       br(),
                                                       h3('Company Properties:', style = 'color: #ff8b3d;'),
                                                       actionButton('show_comp_properties', 'Show Current Properties', style = 'background: #FCCF84; margin-bottom: 20px;'),
                                                       shinyjs::hidden(actionButton('hide_comp_properties', 'Hide Current Properties', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       shinyjs::hidden(downloadButton(outputId = 'hubsCompPropData', label = 'Download Current Properties', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       br(), 
                                                       shinyjs::hidden(htmlOutput('hubsCompCurPropExp')),
                                                       br(),
                                                       shinyjs::hidden(dataTableOutput('cur_company_properties')),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       h3('Current Companies:', style = 'color: #ff8b3d;'),
                                                       actionButton('show_cur_companies', 'Show Current Companies', style = 'background: #FCCF84; margin-bottom: 20px;'),
                                                       shinyjs::hidden(actionButton('hide_cur_companies', 'Hide Current Companies', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       shinyjs::hidden(downloadButton(outputId = 'hubsCurrentCompData', label = 'Download Current Companies', style = 'margin-bottom: 20px; background: #d9dddc;')),
                                                       br(), 
                                                       shinyjs::hidden(htmlOutput('hubsCompCurExp')),
                                                       br(),
                                                       shinyjs::hidden(dataTableOutput('cur_company')),
                                                       br(),
                                                       hr(),
                                                       br(),
                                                       h3('Add Companies:', style = 'color: #ff8b3d;'),
                                                       fluidRow(column(2, h5('Upload Companies', style = 'color: #000;'), style = 'margin-left:10px; margin-bottom: -10px;', style = 'margin-right:-100px;'), 
                                                                column(1, actionButton("companyAddUploadInfo", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-left:-10px;'))),
                                                       fileInput(inputId = 'companyAddUpload', label = NULL, multiple = FALSE, accept = '.csv', buttonLabel = 'Browse', placeholder = 'No File Selected'),
                                                       tags$style(".btn-file {background: #FCCF84;} .progress-bar {background: #ff8b3d;}"),
                                                       shinyjs::hidden(htmlOutput('hubsCompAddExp')),
                                                       br(),
                                                       withSpinner(DT::dataTableOutput('prev_addCompany'), type = 6),
                                                       shinyjs::disabled(shinyjs::hidden(actionButton('updateAddCompanyWarning', 'Ready to Update', style = 'background: #EA4C46; color: #fff;'))),
                                                       shinyjs::hidden(downloadButton(outputId = 'hubsPushedAddCompanyData', label = 'Download Pushed Data', style = 'background: #FCCF84;')),
                                                       # br(), 
                                                       # shinyjs::hidden(actionButton('addCompShowJSON', 'Show JSON', style = 'background: #d9dddc; margin-top:20px;')), 
                                                       # shinyjs::hidden(actionButton('addCompHideJSON', 'Hide JSON', style = 'background: #d9dddc; margin-top:20px;')),
                                                       # textOutput('add_json_output'), 
                                                       br(),
                                                       hr(),
                                                       br(),
                                                       h3('Update Companies:', style = 'color: #ff8b3d;'),
                                                       fluidRow(column(2, h5('Upload Companies', style = 'color: #000;'), style = 'margin-left:10px; margin-bottom: -10px;', style = 'margin-right:-100px;'), 
                                                                column(1, actionButton("companyUpdUploadInfo", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-left:-10px;'))),
                                                       fileInput(inputId = 'companyUpdUpload', label = NULL, multiple = FALSE, accept = '.csv', buttonLabel = 'Browse', placeholder = 'No File Selected'),
                                                       tags$style(".btn-file {background: #FCCF84;} .progress-bar {background: #ff8b3d;}"),
                                                       br(),
                                                       hr(),
                                                       br(),
                                                       h3('Delete Companies:', style = 'color: #ff8b3d;'),
                                                       fluidRow(column(2, h5('Upload Companies', style = 'color: #000;'), style = 'margin-left:10px; margin-bottom: -10px;', style = 'margin-right:-100px;'), 
                                                                column(1, actionButton("companyDelUploadInfo", "", icon = icon("question-circle"), style = 'background: none; border: none; margin-left:-10px;'))),
                                                       fileInput(inputId = 'companyDelUpload', label = NULL, multiple = FALSE, accept = '.csv', buttonLabel = 'Browse', placeholder = 'No File Selected'),
                                                       tags$style(".btn-file {background: #FCCF84;} .progress-bar {background: #ff8b3d;}"),
                                              ))
                        ) 
               ), #Tab panel bracket
               ###############################################.
               ## searchdomo ----
               ###############################################.
               tabPanel("DOMO", icon = img(src = 'domo icon.png', width = 17), value = "DOMO",
                        mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
                                  wellPanel(
                                    
                                    div(fluidRow(
                                      column(3, passwordInput('domo-client-id', 'Client ID', value = '', placeholder = 'Client ID', width = 400)),
                                      column(3, passwordInput('domo-secret', 'Secret', value = '', placeholder = 'Secret', width = 400))), style = "margin-left:10px;"),
                                    
                                    div(fluidRow(actionButton('domo-submit-button', label = 'Submit')), style = "margin-left:20px;"),
                                    
                                    style = "background: #B7DAF5"),
                                  hr(),
                                  #tags$style(HTML(".tabbable > .nav > li[class=active] > a {background-color: #B7DAF5; color: #000;}")),
                                  tabsetPanel(
                                    tabPanel('Authentication', width = 100 , icon = img(src = 'key icon.png', width = 25)),
                                    tabPanel('Manage Update', width = 100, icon = img(src = 'pdp icon.png', width = 25)),
                                    tabPanel('Manage User', width = 100, icon = img(src = 'user icon.png', width = 25)),
                                    tabPanel('Manage Group', width = 100, icon = img(src = 'group icon.png', width = 25))
                                  )
                        )), #Tab panel bracket
               ###############################################.             
               ##############NavBar Menu----
               ###############################################.
               #Starting navbarMenu to have tab with dropdown list
               navbarMenu("Info", icon = icon("info-circle"),
                          ###############################################.
                          ## About ----
                          ###############################################.
                          tabPanel("About", value = "about",
                                   sidebarPanel(width=1),
                                   mainPanel(width=8,
                                             h4("About", style = "color:black;"),
                                             p("ScotPHO's profiles tool allows users to explore the various different profiles 
                                produced by the ", tags$a(href="http://www.scotpho.org.uk/about-us/about-scotpho/", "ScotPHO collaboration.", 
                                                          class="externallink")),
                                             p("The profiles are intended to increase understanding of local health issues 
                                and to prompt further investigation, rather than to be used as a performance 
                                management tool. The information needs to be interpreted within a local 
                                framework; an indicator may be higher or lower in one area compared to another, 
                                but local knowledge is needed to understand and interpret differences."),
                                             p("The Scottish Public Health Observatory (ScotPHO) collaboration is led 
                                by Public Health Scotland, and includes Glasgow Centre for Population Health, National Records of Scotland, 
                                the MRC/CSO Social and Public Health Sciences Unit and the Scottish Learning Disabilities Observatory."),
                                             p("We aim to provide a clear picture of the health of the Scottish population and the factors 
                                that affect it. We contribute to improved collection and use of routine data on health, 
                                risk factors, behaviours and wider health determinants. We take a lead in determining 
                                Scotland's future public health information needs, develop innovations in public health 
                                information and provide a focus for new routine public health information development 
                                where gaps exist."),
                                             p("Organisations may cite material included within the ScotPHO profiles tool subject to the following conditions:",
                                               tags$li("Quote the source as “Scottish Public Health Observatory”"),
                                               tags$li("Include the following URL -", 
                                                       tags$a(href ="https://scotland.shinyapps.io/ScotPHO_profiles_tool/", "https://scotland.shinyapps.io/ScotPHO_profiles_tool/", class = "externallink"))),
                                             p("If you have any trouble accessing any information on this site or have
                                any further questions or feedback relating to the data or the tool, then please contact us at: ",
                                               tags$b(tags$a(href="mailto:phs.scotpho@phs.scot", "phs.scotpho@phs.scot", class="externallink")),
                                               "and we will be happy to help.")),
                                   br()
                          ),#Tab panel
                          ###############################################.
                          ## Indicator definitions ----
                          ###############################################.
                          tabPanel("Indicator definitions", value = "definitions",
                                   #Sidepanel for filtering data
                                   fluidRow(style = "width:60%; margin-left: 2%; min-width: 350px",
                                            h4("Indicator definitions and technical information", style = "color:black;"),
                                            h5(style = "color:black", 
                                               "ScotPHO Profiles are made up of a collection of indicators related to a specific theme 
                                e.g. 'Alcohol' or 'Drugs'. Profiles are further divided into topic areas to group similar indicators together. 
                                 This page allows users to see available indicators and geographies as well as finding detailed technical information 
                                  about how indicators are created."),
                                            br(),
                                            div(title="Choose if you want to see a list of all available indicators or all the details for a specific indicator",
                                                radioGroupButtons("techdoc_selection", status = "primary",
                                                                  choices = c("List of available indicators", "Detailed information about single indicator"), 
                                                                  label= "Step 1. Select what you want to see:" )),
                                            br(),
                                            conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator"',
                                                             uiOutput("indicator_choices"),
                                                             br()
                                            ),
                                            uiOutput("profile_picked_ui"),
                                            br(),
                                            #conditional panel for profile summary
                                            conditionalPanel(condition = 'input.techdoc_selection == "List of available indicators"',
                                                             uiOutput("tecdoc_geographies"),
                                                             downloadButton("download_techdoc1_csv",'Download indicator summary (.csv)', class = "down")),
                                            #conditional panel for single indicator
                                            conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator"',
                                                             div(style="display:inline-block", 
                                                                 title="Filter indicator list from step 2 selecting only indicators from a specific domain", 
                                                                 selectizeInput("topic_defined", label = "Step 3b. Filter indicator list selecting a domain within a particular profile (optional)",
                                                                                width = "100%", choices = topic_list_filter, 
                                                                                selected = "Show all", multiple=FALSE)),
                                                             downloadButton("download_detailtechdoc_csv",'Download selected definition', class = "down"),
                                                             downloadButton("download_alltechdoc_csv",'Download all indicator definitions', class = "down")
                                            )),
                                   wellPanel(width = 11,
                                             # display flextable   
                                             conditionalPanel(condition = 'input.techdoc_selection == "List of available indicators"',
                                                              br(),
                                                              br(),
                                                              uiOutput("techdoc_display")),
                                             #techdoc single indicator
                                             conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator" & input.indicator_selection != null',
                                                              useShinydashboard(),
                                                              valueBoxOutput("indicator", width=12),
                                                              column(5,
                                                                     ind_def_box("Definition", "definition"),
                                                                     ind_def_box("Data source", "source"),
                                                                     ind_def_box("Numerator", "numerator"),
                                                                     ind_def_box("Measure", "measure"),
                                                                     ind_def_box("Rounding and imputation", "rounding"),
                                                                     ind_def_box("Year type", "year"),
                                                                     ind_def_box("Trends from", "trends_from"),
                                                                     ind_def_box("Geographies available", "geos"),
                                                                     ind_def_box("Notes,caveats and other info", "notes"),
                                                                     ind_def_box("Date last updated", "last_updated")),
                                                              column(5,
                                                                     ind_def_box("Rationale for inclusion", "rationale"),
                                                                     ind_def_box("Diagnostic codes & position", "diagnosis"),
                                                                     ind_def_box("Denominator", "denominator"),
                                                                     ind_def_box("Disclosure control", "disclosure"),
                                                                     ind_def_box("Age group", "age"),
                                                                     ind_def_box("Sex", "sex"),
                                                                     ind_def_box("Aggregation", "aggregation"),
                                                                     ind_def_box("Frequency of update", "update_frequency"),
                                                                     ind_def_box("Confidence interval method", "confidence_interval"),
                                                                     ind_def_box("Links to supporting information", "supporting_info"),
                                                                     ind_def_box("Next update due", "next_update") ))
                                   ) # well panel
                          ), #tab panel
                          ###############################################.             
                          ##############Resources----    
                          ###############################################.      
                          tabPanel("Resources", value = "resources",
                                   sidebarPanel(width=1),
                                   mainPanel(
                                     h4("Resources", style = "color:black;"),
                                     p("We list a number of resources that help you to understand better the profiles or to
                        carry out similar analysis to ours"),
                                     tags$ul( 
                                       #Link to user guide
                                       tags$li(class= "li-custom", tags$a(href="https://www.scotpho.org.uk/media/1880/scotpho-profiles-quick-reference-guide-sep2019.docx", 
                                                                          "User quick reference guide",  class="externallink"), 
                                               " - Learn how to use and get the most out of the tool"),
                                       #Link to overview reports
                                       tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/comparative-health/profiles/resources/",
                                                                          "Overview reports",  class="externallink"), 
                                               " - These provide context, narrative and analysis for each profile"),
                                       #Link to user guide
                                       tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/media/1026/explanation-of-statistics-used-in-profiles-v2.pptx", 
                                                                          "Statistics of the profiles",  class="externallink"), 
                                               " - A guide and explanation of the statistics used in the profiles"),
                                       #Link to timetable of updates
                                       tags$li(class= "li-custom", tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQUQMORMqe9RrMnS9WJSu51Q6ef0rubiF1M-QN3BYZIBueErtTvvbRe_kTZbWmnupiO_Uie80BoZCnK/pubhtml",
                                                                          "Timetable of updates", class="externallink"), 
                                               "- List of available indicators, date of last update and expected next update"),
                                       #Link to Github repositories
                                       tags$li(class= "li-custom", tags$a(href="https://github.com/ScotPHO/indicator-production",
                                                                          "Indicator production code", class="externallink"), 
                                               " and ",
                                               tags$a(href="https://github.com/ScotPHO/scotpho-profiles-tool",
                                                      "Profile tool code", class="externallink"), 
                                               "- Access the code used to produce the indicator data and this tool"),
                                       #Link to population lookups
                                       tags$li(class= "li-custom", tags$a(href="https://www.opendata.nhs.scot/dataset/population-estimates",
                                                                          "Population estimate", class="externallink"),  " and ",
                                               tags$a(href="                   https://www.opendata.nhs.scot/dataset/geography-codes-and-labels",
                                                      "geography names and codes", class="externallink"), 
                                               "- Where you can find the files with the populations and geographies
                                used for the analysis"),
                                       #Link to shapefiles
                                       tags$li(class= "li-custom", tags$a(href="https://data.gov.uk/publisher/scottish-government-spatial-data-infrastructure",
                                                                          "Shapefiles", class="externallink"), 
                                               "- Where you can find the shapefiles used for the map")
                                     ), #Bullet point list bracket
                                     br()
                                   ) # mainPanel bracket
                          ), #Tab panel bracket
                          ###############################################.             
                          ##############Evidence for action----    
                          ###############################################. 
                          tabPanel(a("Evidence for action", href="https://www.scotpho.org.uk/comparative-health/profiles/resources/evidence-for-action/", target="_blank")
                          ), #tabPanel bracket
                          ###############################################.             
                          ############## Tour of the tool----    
                          ###############################################.
                          tabPanel("Tour of the tool", value = "tour",
                                   sidebarPanel(width=1),
                                   mainPanel(width=10,
                                             fluidRow(p(h4("Welcome to the ScotPHO Profiles Tool"),
                                                        h5("This interactive tool provides access to a range of public
              health related indicators at different geographies including NHS boards, council areas and health and
              social care partnerships.", style = "color:black;"),
                                                        h5("There are different ways to navigate around the tool.", style = "color:black;"),
                                                        h5("Different visualisations can be opened using the menu bar (the blue strip) at the top of the screen.",
                                                           style = "color:black;"),
                                                        img(src='introjs_tabset_panel.PNG',width=300),
                                                        br(),
                                                        h5("The 'Home' option in the menu bar will return to the profiles tool homepage.",
                                                           style = "color:black;"),
                                                        style = "font-size:20px")),
                                             hr(),
                                             fluidRow(column(6,
                                                             h5("The profile summary allows you to look at multiple indicators within an area at the same time.",
                                                                style = "color:black;")),
                                                      column(6, img(src='tour_summary1.PNG'))),
                                             hr(),
                                             fluidRow(column(3,
                                                             h5("The trend and rank charts allow detailed exploration of one indicator at a time.",
                                                                style = "color:black;")),
                                                      column(9, img(src='tour_trendrank1.PNG'))),
                                             hr(),
                                             fluidRow(p(h5("Throughout the tool use the dropdown menus to change which indicators or geographies are displayed in the charts.",
                                                           style = "color:black;"),
                                                        img(src='tour_summary2.png', style = "vertical-align: middle; border-style: solid; border-color: black; border-width: 1px"),
                                                        column(6, h5("While using dropdown menus mouse click within a dropdown menu and press backspace on your keyboard ('<-') then start typing a word to quickly find the options you are looking for",
                                                                     style = "color:black;")),
                                                        column(6, img(src='introjs_how_to_select.png')))),
                                             hr(),
                                             br(),
                                             fluidRow(column(8,
                                                             p(h5("Throughout the tool look out for options in each window that provide",
                                                                  style = "color:black;"),
                                                               tags$ul( tags$li("indicator definitions or help to interpret a visualisation,"),
                                                                        tags$li("data download options for individual charts,"),
                                                                        tags$li("image downloads for individual charts.")))),
                                                      column(4, img(src='tour_rankmap2.PNG'))),
                                             hr(),
                                             br(),
                                             fluidRow(column(6,
                                                             h5("The 'Data' window can be used to filter and download profiles data.",
                                                                style = "color:black;")),
                                                      column(6, img(src='tour_data1.PNG'))),
                                             hr(),
                                             br(),
                                             fluidRow(column(6,
                                                             h5("The inequalities module allows exploration of deprivation effects for a selection of indicators from the main profiles tool.",
                                                                style = "color:black;")),
                                                      column(6, img(src='tour_ineq1.png'))),
                                             hr(),
                                             br(),
                                             fluidRow(h5("There are also options to find out information such as detailed descriptions of the profile indicators, indicator update schedules and links to evidence for action briefings.",
                                                         style = "color:black;"),
                                                      img(src='tour_about1.PNG', width="100%"))
                                   )#main panel bracket
                          ), #tab panel bracket
                          ###############################################.             
                          ##############Other profiles----    
                          ###############################################.
                          tabPanel("Other profiles", value = "others",
                                   sidebarPanel(width=1),
                                   mainPanel(
                                     h4("Alternative profiles & resources", style = "color:black;"),
                                     p("There are a number of organisations that provide local information relating to the wider determinants of health in Scotland.
             Below are links to some of alternative profiling products."),
                                     tags$ul( 
                                       #Link to GCPH
                                       tags$li(class= "li-custom", tags$a(href="http://www.nssdiscovery.scot.nhs.uk/",
                                                                          "NSS Discovery",  class="externallink")), 
                                       #Link to GCPH
                                       tags$li(class= "li-custom", tags$a(href="http://www.understandingglasgow.com/",
                                                                          "Glasgow Centre for Population Health (GCPH)",  class="externallink")), 
                                       #Link to Fife
                                       tags$li(class= "li-custom", tags$a(href="https://knowfife.fife.gov.uk/",
                                                                          "KnowFife Dataset",  class="externallink")), 
                                       #Link to IS
                                       tags$li(class= "li-custom", tags$a(href="http://www.improvementservice.org.uk/community-planning-outcomes-profile.html",
                                                                          "Improvement Service (IS) - Community planning outcomes profile (CPOP)",  class="externallink")), 
                                       #Link to NRS
                                       tags$li(class= "li-custom", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/council-area-profiles", 
                                                                          "National Records of Scotland (NRS) Council Area Profiles",  class="externallink")), 
                                       #Link to stats.gov.scot
                                       tags$li(class= "li-custom", tags$a(href="http://statistics.gov.scot/home", 
                                                                          "Statistics.gov.scot",  class="externallink")), 
                                       #Link to Scottish nation
                                       tags$li(class= "li-custom", tags$a(href="http://www.environment.gov.scot/", 
                                                                          "Scotland's Environment Hub",  class="externallink"))
                                     ), #Bullet point list bracket
                                     br()
                                   ) # mainPanel bracket
                          ) #tabPanel bracket
               )# NavbarMenu bracket
    ), #Bracket  navbarPage
    div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
    ###############################################.             
    ##############Footer----    
    ###############################################.
    #Copyright warning
    tags$footer(column(6, "© Scottish Public Health Observatory v2.0 2018"), 
                column(2, tags$a(href="mailto:phs.scotpho@phs.scot", tags$b("Contact us!"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                column(3, tags$a(href="https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies", 
                                 tags$b("Privacy & cookies"), 
                                 class="externallink", style = "color: white; text-decoration: none")), 
                column(1, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
                                     style= "color:white;", 
                                     onclick = sprintf("window.open('%s')", 
                                                       "https://twitter.com/intent/tweet?text=Check%out%ScotPHO's%profile%tool&url=https://scotland.shinyapps.io/ScotPHO_profiles_tool/"))), 
                style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #1995dc"
    ) 
    ################################################.
  ) #bracket tagList
  ###END
  
  
)






server <- function(input, output, session) {
  
  ####################################################################################################################################
  # Landing Page
  observeEvent(input$hubspotLogoButton, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'HubSpot') })
  
  observeEvent(input$domoLogoButton, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'DOMO') })
  
  observeEvent(input$hubspotButton1, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'HubSpot') })
  
  observeEvent(input$hubspotButton2, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'HubSpot') })
  
  observeEvent(input$hubspotButton3, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'HubSpot') })
  
  observeEvent(input$hubspotButton4, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'HubSpot') })
  
  observeEvent(input$domoButton1, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'DOMO') })
  
  observeEvent(input$domoButton2, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'DOMO') })
  
  observeEvent(input$domoButton3, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'DOMO') })
  
  observeEvent(input$domoButton4, {
    updateNavlistPanel(session = getDefaultReactiveDomain(), inputId = 'intabset', selected = 'DOMO') })
  
  js$disableTab("update_contact")
  js$disableTab("update_deal")
  js$disableTab("update_company")
  
  observeEvent(input$hubspot_submit_button, {
    if(is.null(current_property_table()) == FALSE ) 
    {js$enableTab("update_contact")
      js$enableTab("update_deal")
      js$enableTab("update_company")}
    else
    {js$disableTab("update_contact")
      js$disableTab("update_contact")
      js$disableTab("update_contact")}})
  
  ####################################################################################################################################
  # HubSpot Authentication
  observe({if(input$hubspot_api_key == '') {disable('hubspot_submit_button')}
    else {enable('hubspot_submit_button')}})
  
  observeEvent(input$hubsAuthModalButton, { # HubSpot API Key Modal
    showModal(modalDialog(
      title = "HubSpot API Key",
      'An API Key is used to retrieve and write data against a HubSpot Instance. 
      To learn more about API Key and how Hubs Bridge is using it, please refer to the "Authentication" section.',
      easyClose = TRUE,
      footer = modalButton('Close') ))})
  
  hubspotApiKey <- eventReactive(input$hubspot_submit_button, {
    req(input$hubspot_api_key)
    hubspotApiKey <- input$hubspot_api_key})
  
  hsCUrl <- eventReactive(input$hubspot_submit_button, {
    req(input$hubspot_api_key)
    hsCUrl <- paste0("https://api.hubapi.com/contacts/v1/contact/batch/?hapikey=", input$hubspot_api_key)})
  
  current_property_table_raw <- eventReactive(input$hubspot_submit_button, {
    current_property_table_raw <-hs_contact_properties_raw(apikey = input$hubspot_api_key) %>% list.filter(readOnlyValue == FALSE) })
  
  hubsConOpt <- eventReactive(input$hubspot_submit_button, {
    hubsConOpt <- conOptionFunc(current_property_table_raw()) })
  
  current_property_table <- eventReactive(input$hubspot_submit_button, {
    current_property_table <- current_property_table_raw() %>%  list.stack() %>% 
      mutate(sort = ifelse(fieldType %in% c('select', 'checkbox', 'radio'), 1, 2)) %>% 
      arrange(sort, fieldType, label) %>% select(name, label, fieldType) %>% rename(`HubSpot Internal Name` = name, `HubSpot Label` = label, `Field Type` = fieldType) %>% 
      unique() %>% left_join(hubsConOpt()) })
  
  curColName <- eventReactive(input$hubspot_submit_button, {
    curColName <- current_property_table() %>% rename(name = `HubSpot Internal Name`, fieldType = `Field Type`) %>% select(name, fieldType) %>% unique() %>%
      mutate(fieldType = case_when(fieldType == 'date' ~ 'date', fieldType == 'number' ~ 'numeric', TRUE ~ 'character')) })
  
  observeEvent(input$hubspot_submit_button, {
    req(input$hubspot_api_key)
    if(nrow(current_property_table()) > 0) 
    {output$hubspot_authentication <- renderText({Sys.sleep(1.5) 
      'Authenticated'})}
    else {output$hubspot_authentication <- renderText({Sys.sleep(1.5) 
      'API Key Not Recognized'})} })
  
  
  # Update Contact 
  output$hubsConCurPropExp <- 
    renderUI({HTML('
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Label:</span> The names of properties in HubSpot that is readily visible to the user.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Field Type:</span> The data type of each field Make sure that the data types of your uploaded file match the data type on this field</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Options:</span> For certain types of field such as "select", "radio", and "checkbox"; make sure that your values match one of the values in this field (Case sensitive)</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Internal Name:</span> The internal names of properties in HubSpot that is not readily available to the users. Your field names should match the values in this Options field (Case sensitive)</p>')})
  
  output$hubsConUpldPropExp <- 
    renderUI({HTML('
  <p> <span style = "color: #ff8b3d;font-style: italic;">Description:</span> The table below will show you the field(s) from your uploaded CSV file that match your current `HubSpot Internal Name`.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Email Field:</span> Make sure that one of your fields contains the email address of the contact. It is a required field to complete an update.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">What to do with the excluded fields:</span> Change the field names to match the `HubSpot Internal Name` as shown in the `Current Properties` section above.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Date Fields:</span> Try to change the format of all DATE fields to yyyy-mm-dd format for example "2022-01-01" for January 1, 2022.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Successful Update Status:</span> SUCCESSFULL updates depend on all records being correct. If a single record contains an invalid value for any properties, the whole upload process will be terminated. </p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Failed Update Status:</span> FAILED updates happen when invalid values are present in properties. Check the values for properties that have inflexible field types such as "select", "radio", "checkbox", and "booleancheckbox". </p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Unknown Failed Update Status:</span> UNKNOWN FAILED updates may happen in cases where HubSpot recognizes the request but fails to execute it. One common example is if you enter "demo" to the "HubSpot API Key". </p>',
                   paste0('<p> <span style = "color: #ff8b3d;font-style: italic;">Upload Overview:</span> ', ncol(valConData()), ' of ',  ncol(uploaded_contact()),' cols; ',  nrow(valConData()), ' of ', nrow(uploaded_contact()),  ' rows</p>'))})
  
  output$cur_contact_properties <- renderDataTable(current_property_table())
  output$hubsCurrentConData <- downloadHandler(filename = function(){'HubSpot Current Properties.csv'}, content = function(file){write_csv(current_property_table(), file, na = '')})
  
  observeEvent(input$show_cur_properties, {
    req(input$hubspot_api_key)
    req(current_property_table())
    shinyjs::show("hubsConCurPropExp")
    shinyjs::show("hide_cur_properties")
    shinyjs::show("cur_contact_properties")
    shinyjs::show("hubsCurrentConData")
    shinyjs::hide("show_cur_properties")})
  
  observeEvent(input$hide_cur_properties, {
    req(input$hubspot_api_key)
    req(current_property_table())
    shinyjs::hide("hubsConCurPropExp")
    shinyjs::hide("hide_cur_properties")
    shinyjs::hide("cur_contact_properties")
    shinyjs::hide("hubsCurrentConData")
    shinyjs::show("show_cur_properties")})
  
  observeEvent(input$contactUploadInfo, { # Upload Contact Modal
    showModal(modalDialog(
      title = "Upload Contacts",
      "Only upload CSV files with headers correspond to the property names on your HubSpot instance. Make sure that each of the header name matches the HubSpot Internal Name. 
      You can find the HubSpot Internal Name by clicking the 'Show Current Properties' button under the Current Properties section. 
      Hubs Bridge will only take and process the first 1,000 rows of each uploaded file. If you have more than 1,000 records to update/add to HubSpot, please separate them into multiple files and loads.
      email is a required field to add or update a contact.",
      easyClose = TRUE,
      footer = modalButton('Close')))})
  
  uploaded_contact <- reactive({
    req(input$contactUpload)
    shinyjs::show('updateAllContactWarning')
    shinyjs::show('hubsConUpldPropExp')
    shinyjs::show('conShowJSON')
    shinyjs::hide('hubsPushedConData')
    shinyjs::hide('json_output')
    output$prev_uploadedContact <- DT::renderDataTable({valConData()}, options = list(scrollX = TRUE))
    csv <- vroom(file = input$contactUpload$datapath, delim = ',', n_max = 1000)})
  
  uploadedColName <- reactive({
    req(input$contactUpload)
    uploadedColName <- tibble(name = colnames(uploaded_contact()))})
  
  valColName <- reactive({
    req(input$hubspot_api_key)
    req(input$contactUpload)
    valColName <- inner_join(curColName(), uploadedColName())})
  
  valConData <- reactive({
    req(input$hubspot_api_key)
    req(input$contactUpload)
    valConData <- uploaded_contact() %>% select(valColName()$name) %>% mutate(validEmail = isValidEmail(email)) %>% 
      filter(validEmail == TRUE) %>% select(-validEmail) %>% group_by(email) %>% filter(duplicated(email) | n()==1)})
  
  output$prev_uploadedContact <- DT::renderDataTable({NA})
  # output$prev_uploadedContact <- DT::renderDataTable({valConData()}, options = list(scrollX = TRUE))
  
  
  # Transform Dates to Epoch Format
  newConColOrder <- reactive({
    req(input$contactUpload)
    newConColOrder <- valColName() %>% mutate(order = ifelse(fieldType == 'date', 1,2)) %>% arrange(order)})
  
  nConDateCol <- reactive({
    req(input$contactUpload)
    nConDateCol <- newConColOrder() %>% filter(fieldType == 'date') %>% nrow()})
  
  orderedConData <-  reactive({
    req(input$contactUpload)
    orderedConData <- valConData() %>% select(newConColOrder()$name)})
  
  corConDateData <- reactive({
    req(input$contactUpload)
    if(nConDateCol()>0)
    {corConDateData <- cbind(apply(orderedConData()[,1:nConDateCol()], 2, miliSecond), orderedConData()[,(nConDateCol()+1):ncol(orderedConData())]) %>% 
      mutate(across(everything(), as.character))  %>% rownames_to_column()}
    else {corConDateData <- orderedConData() %>% mutate(across(everything(), as.character))  %>% rownames_to_column()}})
  
  
  # Push Contacts to HubSpot
  final_json <- reactive({
    req(input$hubspot_api_key)
    req(input$contactUpload)
    
    hsCLongList <- corConDateData()  %>% pivot_longer(cols = -c('email', 'rowname'), names_to = 'property', values_to = 'value') %>% filter(value != '') %>% tibble()
    
    comp_list <- list()
    for (hsC in 1:nrow(corConDateData())) {
      a <- list(
        email = corConDateData()[hsC, ]$email,
        properties = hsCLongList %>%
          filter(rowname == hsC) %>%
          select(c(property, value)))
      
      comp_list[[hsC]] <- a
    }
    final_json <- toJSON(comp_list, pretty = TRUE, auto_unbox = TRUE)
    return(final_json) })
  
  observeEvent(input$updateAllContactWarning, { # Upload Contact Modal
    showModal(modalDialog(
      title = "Warning",
      'Are you sure that you want to update all HubSpot contacts and their properties as shown by the table under the "Add/Update Contacts" section?',
      easyClose = TRUE,
      footer = tagList(modalButton('Cancel'),
                       actionButton('updateAllContact', 'Push To HubSpot', style = 'background: #FCCF84;')) ))})
  
  observeEvent(input$updateAllContact, {
    removeModal()
    pushConRes <- POST(url = hsCUrl(),
                       body = final_json(),
                       add_headers(.headers = c("Content-Type"="application/json")))
    
    conStatusData <- tibble(`Update Status` = if(status_code(pushConRes) == 202) {'SUCCESSFUL, Update has been successfully completed.'}
                            else if(status_code(pushConRes) == 400) {"FAILED, Update failed to be performed. 
                                                                                This can happen if you pass an invalid email address, 
                                                                                if a property in your request doesn't exist, or 
                                                                                if you pass an invalid property value."}
                            else {'FAILED, update failed to be performed for UNKNOWN reasons.'},
                            email = corConDateData()$email) %>% 
      left_join(valConData()) %>% rename(Email = email)
    
    output$prev_uploadedContact <- renderDataTable({Sys.sleep(1.5)
      conStatusData}) 
    
    output$hubsPushedConData <- downloadHandler(filename = function(){'Contact Pushed To HubSpot.csv'},
                                                content = function(file){write_csv(conStatusData, file, na = '')})
    shinyjs::show('hubsPushedConData')
  })
  
  output$json_output <- renderText({final_json()})   
  
  observeEvent(input$conShowJSON, {
    shinyjs::hide('conShowJSON')
    shinyjs::show('conHideJSON')
    shinyjs::show('json_output')})
  
  observeEvent(input$conHideJSON, {
    shinyjs::show('conShowJSON')
    shinyjs::hide('conHideJSON')
    shinyjs::hide('json_output')})
  
  ####################################################################################################################################
  # Update Deals
  
  ####################################################################################################################################
  # Companies
  
  # Company Properties
  current_comp_property_table_raw <- eventReactive(input$hubspot_submit_button, {
    current_comp_property_table_raw <-hs_company_properties_raw(apikey = input$hubspot_api_key) %>% list.filter(readOnlyValue == FALSE) })
  
  hubsCompOpt <- eventReactive(input$hubspot_submit_button, {
    hubsCompOpt <- conOptionFunc(current_comp_property_table_raw()) })
  
  comp_property_table <- eventReactive(input$hubspot_submit_button, {
    comp_property_table <- current_comp_property_table_raw() %>%  list.stack() %>% 
      mutate(sort = ifelse(fieldType %in% c('select', 'checkbox', 'radio'), 1, 2)) %>% 
      arrange(sort, fieldType, label) %>% select(name, label, fieldType) %>% rename(`HubSpot Internal Name` = name, `HubSpot Label` = label, `Field Type` = fieldType) %>% 
      unique() %>% left_join(hubsCompOpt()) })
  
  compColName <- eventReactive(input$hubspot_submit_button, {
    compColName <- comp_property_table() %>% rename(name = `HubSpot Internal Name`, fieldType = `Field Type`) %>% select(name, fieldType) %>% unique() %>%
      mutate(fieldType = case_when(fieldType == 'date' ~ 'date', fieldType == 'number' ~ 'numeric', TRUE ~ 'character')) })
  
  
  output$cur_company_properties <- renderDataTable(comp_property_table())
  output$hubsCompPropData <- downloadHandler(filename = function(){'HubSpot Current Company Properties.csv'}, content = function(file){write_csv(comp_property_table(), file, na = '')})
  
  observeEvent(input$show_comp_properties, {
    req(input$hubspot_api_key)
    req(comp_property_table())
    shinyjs::show("hubsCompCurPropExp")
    shinyjs::show("hide_comp_properties")
    shinyjs::show("cur_company_properties")
    shinyjs::show("hubsCompPropData")
    shinyjs::hide("show_comp_properties")})
  
  
  observeEvent(input$hide_comp_properties, {
    req(input$hubspot_api_key)
    req(current_property_table())
    shinyjs::hide("hubsCompCurPropExp")
    shinyjs::hide("hide_comp_properties")
    shinyjs::hide("cur_company_properties")
    shinyjs::hide("hubsCompPropData")
    shinyjs::show("show_comp_properties")})
  
  output$hubsCompCurPropExp <- 
    renderUI({HTML('
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Label:</span> The names of properties in HubSpot that is readily visible to the user.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Field Type:</span> The data type of each field Make sure that the data types of your uploaded file match the data type on this field</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Options:</span> For certain types of field such as "select", "radio", and "checkbox"; make sure that your values match one of the values in this field (Case sensitive)</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Internal Name:</span> The internal names of properties in HubSpot that is not readily available to the users. Your field names should match the values in this Options field (Case sensitive)</p>')})
  
  
  
  # Current Companies
  cur_comp <- eventReactive(input$hubspot_submit_button, {
    cur_comp <- (hs_companies_tidy(hs_companies_raw(apikey = input$hubspot_api_key)))
    if (nrow(cur_comp) > 0) {cur_comp <- hs_companies_tidy(hs_companies_raw(apikey = input$hubspot_api_key, properties = c('name', 'website'))) %>% left_join(hs_companies_tidy(hs_companies_raw(apikey = input$hubspot_api_key)))}
    else {cur_comp <- tibble(Status = 'No company records can be retrieved.')}
       })
  
  
  output$cur_company <- DT::renderDataTable({cur_comp()}, options = list(scrollX = TRUE))
  output$hubsCurrentCompData <- downloadHandler(filename = function(){'HubSpot Current Company.csv'}, content = function(file){write_csv(cur_comp(), file, na = '')})
  
  observeEvent(input$show_cur_companies, {
    req(input$hubspot_api_key)
    req(cur_comp())
    shinyjs::show("hubsCompCurExp")
    shinyjs::show("hide_cur_companies")
    shinyjs::show("cur_company")
    shinyjs::show("hubsCurrentCompData")
    shinyjs::hide("show_cur_companies")})
  
  
  observeEvent(input$hide_cur_companies, {
    req(input$hubspot_api_key)
    req(current_property_table())
    shinyjs::hide("hubsCompCurExp")
    shinyjs::hide("hide_cur_companies")
    shinyjs::hide("cur_company")
    shinyjs::hide("hubsCurrentCompData")
    shinyjs::show("show_cur_companies")})
  
  output$hubsCompCurExp <- 
    renderUI({HTML('
  <p> <span style = "color: #ff8b3d;font-style: italic;">Description:</span> The table below lists all the companies in your HubSpot instance.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Column Names:</span> The column names of the table correspond to the HubSpot Internal names which you must use in your uploaded file.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Properties:</span> Only filled properties will be shown on the table, other properties will not be included if they have no value or if they are not updatable.</p>')})
  

  
  uploaded_add_company <- reactive({
    req(input$companyAddUpload)
    shinyjs::show('updateAddCompanyWarning')
    shinyjs::show('hubsCompAddExp')
    shinyjs::show('addCompShowJSON')
    shinyjs::hide('hubsPushedAddCompanyData')
    shinyjs::hide('add_json_output')
    shinyjs::show('companyAddUploadInfo')
    output$prev_addCompany <- DT::renderDataTable({uploadedAddComp()}, options = list(scrollX = TRUE))
    csv <- vroom(file = input$companyAddUpload$datapath, delim = ',', n_max = 1000) })
  
  cur_comp_prop_tidy <- reactive({
    req(input$companyAddUpload | input$companyUpdUpload)
    cur_comp_prop_tidy <- current_comp_property_table_raw() %>% list.stack() %>% arrange(fieldType, name) %>% select(name, fieldType) %>% unique() })
  
  exist_comp <- reactive({
    req(input$companyAddUpload)
    if(nrow(cur_comp()) > 1) {exist_comp <- cur_comp() %>% mutate(name_website = paste(name, replace_na(website, '')), hubsBridgeStatus = 'Existing Company') %>% select(name_website, hubsBridgeStatus)}
    else {exist_comp <- tibble(name = 'No company record.')} })
  
  valColNameComp <- reactive({
    req(input$companyAddUpload)
    valColNameComp <- cur_comp_prop_tidy() %>% inner_join(tibble(name = colnames(uploaded_add_company()))) })
  
  uploaded_add_company_cleaned <- reactive({
    req(input$companyAddUpload)
    if(nrow(cur_comp()) > 1) {uploaded_add_company_cleaned <- uploaded_add_company() %>% select(valColNameComp()$name) %>% mutate(name_website = paste(name, replace_na(website, ''))) %>% 
      filter(name != '') %>% group_by(name_website) %>% filter(duplicated(name_website) | n()==1)}
    else {uploaded_add_company_cleaned <- uploaded_add_company() %>% select(valColNameComp()$name) %>% filter(name != '') %>% group_by(name) %>% filter(duplicated(name) | n()==1)} })
  
  uploadedAddComp <- reactive({
    req(input$companyAddUpload)
    if(nrow(cur_comp()) > 1) {uploadedAddComp <- exist_comp() %>% right_join(uploaded_add_company_cleaned()) %>% mutate(hubsBridgeStatus = replace_na(hubsBridgeStatus, 'New Company')) %>% select(-name_website) %>% arrange(desc(hubsBridgeStatus), name)}
    else {uploadedAddComp <- uploaded_add_company_cleaned() %>% mutate(hubsBridgeStatus = 'New Company') %>% arrange(name)} })
  
  output$prev_addCompany <- DT::renderDataTable({uploadedAddComp()}, options = list(scrollX = TRUE))
  
  addCompColOrder <-  reactive({
    req(input$companyAddUpload)
    addCompColOrder <- valColNameComp() %>% mutate(order = ifelse(fieldType == 'date', 1,2)) %>% arrange(order) })
  
  nAddCompDateCol <- reactive({
    req(input$companyAddUpload)
    nAddCompDateCol <- addCompColOrder() %>% filter(fieldType == 'date') %>% nrow() })
  
  orderedAddCompData <- reactive({
    req(input$companyAddUpload)
    orderedAddCompData <- uploadedAddComp() %>% select(addCompColOrder()$name) })
  
  corAddCompDateData1 <- reactive({
    req(input$companyAddUpload)
    corAddCompDateData1 <- cbind(apply(orderedAddCompData()[,1:nAddCompDateCol()], 2, miliSecond), orderedAddCompData()[,(nAddCompDateCol()+1):ncol(orderedAddCompData())]) %>% 
    mutate(across(everything(), as.character)) })
  
  corAddCompDateData2 <-  reactive({
      req(input$companyAddUpload)
      corAddCompDateDataFinal <- cbind(hubsBridgeStatus = uploadedAddComp()$hubsBridgeStatus, corAddCompDateData1()) })
  
  corAddCompDateDataFinal <-  reactive({
    req(input$companyAddUpload)
    corAddCompDateDataFinal <- corAddCompDateData2() %>% filter(hubsBridgeStatus == 'New Company') })
  
  observe({if (nrow(corAddCompDateDataFinal())>0) {shinyjs::enable('updateAddCompanyWarning')} })
  
  observeEvent(input$updateAddCompanyWarning, { # Upload New Company Modal
    showModal(modalDialog(
      title = "Warning",
      'Are you sure that you want to add all companies and their properties as shown by the table under the "Add Companies" section?',
      easyClose = TRUE,
      footer = tagList(modalButton('Cancel'),
                       actionButton('addAllCompanies', 'Push To HubSpot', style = 'background: #FCCF84;')) ))})
  
  observeEvent(input$addAllCompanies, {
    removeModal()
    shinyjs::disable('updateAddCompanyWarning')
    pushedAddComp <- addCompanyFunction(corAddCompDateDataFinal(), uploadedAddComp(), hubspotApiKey())
    
    output$prev_addCompany <- renderDataTable({Sys.sleep(1.5)
      pushedAddComp}) 
    
    output$hubsPushedAddCompanyData <- downloadHandler(filename = function(){'Added Companies Pushed To HubSpot.csv'},
                                                content = function(file){write_csv(pushedAddComp, file, na = '')})
    
    shinyjs::show('hubsPushedAddCompanyData') })
  
  
  
  output$hubsCompAddExp <- 
    renderUI({HTML('
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Label:</span> The names of properties in HubSpot that is readily visible to the user.</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Field Type:</span> The data type of each field Make sure that the data types of your uploaded file match the data type on this field</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">Options:</span> For certain types of field such as "select", "radio", and "checkbox"; make sure that your values match one of the values in this field (Case sensitive)</p>
  <p> <span style = "color: #ff8b3d;font-style: italic;">HubSpot Internal Name:</span> The internal names of properties in HubSpot that is not readily available to the users. Your field names should match the values in this Options field (Case sensitive)</p>')})
  
  
  
  # Update Companies
  uploaded_update_company <- reactive({
    req(input$companyUpdUpload)
    shinyjs::show('updateUpdCompanyWarning')
    shinyjs::show('hubsCompUpdExp')
    shinyjs::show('updCompShowJSON')
    shinyjs::hide('hubsPushedUpdCompanyData')
    shinyjs::hide('upd_json_output')
    shinyjs::show('companyUpdUploadInfo')
    output$prev_addCompany <- DT::renderDataTable({uploadedUpdComp()}, options = list(scrollX = TRUE))
    csv <- vroom(file = input$companyUpdUpload$datapath, delim = ',', n_max = 1000) })
  
                                                                # End
  #################################################################################################################################### 
  
}


shinyApp(ui, server)

