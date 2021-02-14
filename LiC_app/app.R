# Packages
library(shinythemes)
library(shiny)
library(shinyjs)
library(dplyr)
library(digest)
library(DT)
library(aws.s3)
library(stringi)
library(xml2)
library(V8)
library(leaflet)  
library(spData)   
library(sf)
library(dplyr)
library(ggplot2)
library(shinycssloaders)  
require(maps)
library(RColorBrewer)
library(readr)
library(plyr)

##############################################################
######################## Setting up ##########################
##############################################################

#### Shiny AWS S3 ####
# SET AWS settings 
s3BucketName <- "XXX"
Sys.setenv("AWS_ACCESS_KEY_ID" = "XXX",
           "AWS_SECRET_ACCESS_KEY" = "XXX",
           "AWS_DEFAULT_REGION" = "XXX")

#################################################

#### Mandatory labels
# Which fields are mandatory for progression
fieldsMandatory_C <- c("C1","age_year", "repeat_sur")
fieldsMandatory_1 <-  c("SO_11")
fieldsMandatory_2 <-  c("ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10", "ADD1", "ADD2", "ADD3", "ADD4", "ADD5")
fieldsMandatory_4 <- c("SO_coun", "SO_1", "SO_2", "SO_3","SO_4","SO_5","SO_6","SO_7","SO_8","SO_9","SO_10", "LOTR_1","LOTR_2", "LOTR_3", "LOTR_4", "LOTR_5", "LOTR_6") # Situational optimism questions
fieldsMandatory_5 <-  c("K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9","K10_10")
fieldsMandatory_6 <- c("PS_1", "PS_2", "PS_3", "SS1", "SS2", "SS3", "health")


# Function to label the field as mandatory
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}


# add an asterisk to an input label
labelPlease <- function(label) {
  tagList(
    label,
    span("*", class = "please_star")
  )
}

#### Colouring the star, error messages red, and load screen
appCSS <- ".mandatory_star{color:red;}
#error {color:red;}
#loading-content {
position: absolute;
background: #fcfcfc;
opacity: 1;
z-index: 0;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #fcfcfc;}"

# CSS to use in the app
appCSS_please <-
  ".please_star { color: purple; }
  "

#### Saving the responses
# The fields to be saved
fieldsAll <- c("C1","age_year","repeat_sur", "Conservation", "gender", "Nation", "W_coun","education", "position","ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10",
               "ADD1","ADD2","ADD3","ADD4","ADD5",
               "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9","K10_10","SO_coun", "SO_1", "SO_2", "SO_3","SO_4","SO_5","SO_6","SO_7","SO_8","SO_9","SO_10", "SO_11", 
               "LOTR_1","LOTR_2", "LOTR_3", "LOTR_4", "LOTR_5", "LOTR_6","PS_1", "PS_2", "PS_3", "SS1", "SS2", "SS3", "WH", "health","years_cons" ) # Fields to save - make sure to add SO

# Save data 
saveAll <- c("C1","age_year", "repeat_sur", "Conservation", "gender", "Nation", "W_coun","education", "position", "years_cons",
             "GP_1_a", "GP_2_a", "GP_3_a","GP_4_a","GP_5_a","GP_6_a","GP_7_a","GP_8_a","GP_9_a","GP_10_a",
             "GP_1_b","GP_2_b","GP_3_b","GP_4_b","GP_5_b","GP_6_b","GP_7_b","GP_8_b","GP_9_b","GP_10_b",
             "ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10",
             "ADD1","ADD2","ADD3","ADD4","ADD5",
             "K10_1","K10_2","K10_3","K10_4","K10_5","K10_6","K10_7","K10_8","K10_9","K10_10",
             "SO_1", "SO_2", "SO_3","SO_4","SO_5","SO_6","SO_7","SO_8","SO_9","SO_10", "SO_11", 
             "LOTR_1","LOTR_2", "LOTR_3", "LOTR_4", "LOTR_5", "LOTR_6",
             "PS_1", "PS_2", "PS_3" ,  "SS1", "SS2" , "SS3", "WH", "health",  "SO_coun", "areacontext", "endcomment") # Fields to save - make sure to add SO

# Data to be presented back to respondents
save_sample <- c("age_year","Conservation" ,"gender","Nation","W_coun","SO_1","SO_2", "SO_3","SO_4","SO_5","SO_6",
                 "SO_7","SO_8","SO_9", "SO_10","SO_11","LOTR_1","LOTR_2","LOTR_3","LOTR_4","LOTR_5", "LOTR_6", "position" ,
                 "ERI_1","ERI_2","ERI_3","ERI_4","ERI_5","ERI_6","ERI_7","ERI_8","ERI_9","ERI_10",
                 "ADD1","ADD2","ADD3","ADD4","ADD5", "SO_coun")

# Anoymous comment 
save_comment <- c("comment" )



# Refresh function
jscode <- "shinyjs.refresh_1 = function() { location.reload(); }"

# JS codes 
jsCode_txt <- "
shinyjs.TextCol1 = function(params){$('#Conservation').css('color', params);};
shinyjs.TextCol2 = function(params){$('#age_year').css('color', params);}
shinyjs.TextCol3 = function(params){$('#gender').css('color', params);}
shinyjs.TextCol4 = function(params){$('#Nation').css('color', params);}
shinyjs.TextCol5 = function(params){$('#W_coun').css('color', params);}
shinyjs.TextCol6 = function(params){$('#education').css('color', params);}
shinyjs.TextCol7 = function(params){$('#position').css('color', params);}
shinyjs.TextCol28 = function(params){$('#ERI_1').css('color', params);}
shinyjs.TextCol29 = function(params){$('#ERI_2').css('color', params);}
shinyjs.TextCol30 = function(params){$('#ERI_3').css('color', params);}
shinyjs.TextCol31 = function(params){$('#ERI_4').css('color', params);}
shinyjs.TextCol32 = function(params){$('#ERI_5').css('color', params);}
shinyjs.TextCol33 = function(params){$('#ERI_6').css('color', params);}
shinyjs.TextCol34 = function(params){$('#ERI_7').css('color', params);}
shinyjs.TextCol35 = function(params){$('#ERI_8').css('color', params);}
shinyjs.TextCol36 = function(params){$('#ERI_9').css('color', params);}
shinyjs.TextCol37 = function(params){$('#ERI_10').css('color', params);}
shinyjs.TextCol38 = function(params){$('#ADD1').css('color', params);}
shinyjs.TextCol39 = function(params){$('#ADD2').css('color', params);}
shinyjs.TextCol40 = function(params){$('#ADD3').css('color', params);}
shinyjs.TextCol41 = function(params){$('#ADD4').css('color', params);}
shinyjs.TextCol42 = function(params){$('#ADD5').css('color', params);}
shinyjs.TextCol44 = function(params){$('#K10_1').css('color', params);}
shinyjs.TextCol45 = function(params){$('#K10_2').css('color', params);}
shinyjs.TextCol46 = function(params){$('#K10_3').css('color', params);}
shinyjs.TextCol47 = function(params){$('#K10_4').css('color', params);}
shinyjs.TextCol48 = function(params){$('#K10_5').css('color', params);}
shinyjs.TextCol49 = function(params){$('#K10_6').css('color', params);}
shinyjs.TextCol50 = function(params){$('#K10_7').css('color', params);}
shinyjs.TextCol51 = function(params){$('#K10_8').css('color', params);}
shinyjs.TextCol52 = function(params){$('#K10_9').css('color', params);}
shinyjs.TextCol53 = function(params){$('#K10_10').css('color', params);}
shinyjs.TextCol54 = function(params){$('#SO_1').css('color', params);}
shinyjs.TextCol55 = function(params){$('#SO_2').css('color', params);}
shinyjs.TextCol56 = function(params){$('#SO_3').css('color', params);}
shinyjs.TextCol57 = function(params){$('#SO_4').css('color', params);}
shinyjs.TextCol58 = function(params){$('#SO_5').css('color', params);}
shinyjs.TextCol59 = function(params){$('#SO_6').css('color', params);}
shinyjs.TextCol60 = function(params){$('#SO_7').css('color', params);}
shinyjs.TextCol61 = function(params){$('#SO_8').css('color', params);}
shinyjs.TextCol62 = function(params){$('#SO_9').css('color', params);}
shinyjs.TextCol63 = function(params){$('#SO_10').css('color', params);}
shinyjs.TextCol64 = function(params){$('#SO_11').css('color', params);}
shinyjs.TextCol65 = function(params){$('#LOTR_1').css('color', params);}
shinyjs.TextCol66 = function(params){$('#LOTR_2').css('color', params);}
shinyjs.TextCol67 = function(params){$('#LOTR_3').css('color', params);}
shinyjs.TextCol68 = function(params){$('#LOTR_4').css('color', params);}
shinyjs.TextCol69 = function(params){$('#LOTR_5').css('color', params);}
shinyjs.TextCol70 = function(params){$('#LOTR_6').css('color', params);}
shinyjs.TextCol71 = function(params){$('#PS_1').css('color', params);}
shinyjs.TextCol72 = function(params){$('#PS_2').css('color', params);}
shinyjs.TextCol73 = function(params){$('#PS_3').css('color', params);}
shinyjs.TextCol74 = function(params){$('#SS1').css('color', params);}
shinyjs.TextCol75 = function(params){$('#SS2').css('color', params);}
shinyjs.TextCol77 = function(params){$('#WH').css('color', params);}
shinyjs.TextCol78 = function(params){$('#health').css('color', params);}
shinyjs.TextCol80 = function(params){$('#SO_coun').css('color', params);}

shinyjs.TextCol86 = function(params){$('#SS3').css('color', params);}
shinyjs.TextCol91 = function(params){$('#GP_1_b').css('color', params);}
shinyjs.TextCol92 = function(params){$('#GP_2_b').css('color', params);}
shinyjs.TextCol93 = function(params){$('#GP_3_b').css('color', params);}
shinyjs.TextCol94 = function(params){$('#GP_4_b').css('color', params);}
shinyjs.TextCol95 = function(params){$('#GP_5_b').css('color', params);}
shinyjs.TextCol96 = function(params){$('#GP_6_b').css('color', params);}
shinyjs.TextCol97 = function(params){$('#GP_7_b').css('color', params);}
shinyjs.TextCol98 = function(params){$('#GP_8_b').css('color', params);}
shinyjs.TextCol99 = function(params){$('#GP_9_b').css('color', params);}
shinyjs.TextCol100 = function(params){$('#GP_10_b').css('color', params);}
shinyjs.TextCol101 = function(params){$('#repeat_sur').css('color', params);}
shinyjs.TextCol102 = function(params){$('#years_cons').css('color', params);}
"

# # Twitter share button 
# # http://tech.cymi.org/tweet-intents
url_t <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Flivedataoxford.shinyapps.io%2Flifeinconservation%2F&text=Check%20out%20this%20online%20survey%20exploring%20the%20challenges%20and%20rewards%20faced%20by%20those%20in%20conservation%20by%20@ICCS_updates&hashtags=lifeinconservation"
url_t2 <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Flivedataoxford.shinyapps.io%2Flifeinconservation%2F&text=This%20online%20survey%20uses%20the%20%22effort-reward%20imbalance%22%20model%20to%20show%20the%20balance%20of%20challenges%20and%20rewards%20faced%20by%20those%20in%20conservation%20@ICCS_updates&hashtags=lifeinconservation"
url_t3 <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Flivedataoxford.shinyapps.io%2Flifeinconservation%2F&text=The%20global%20distribution%20of%20%22conservation%20optimism%22%2C%20from%20an%20online%20survey%20by%20@ICCS_updates%3B%20can%20you%20help%20fill%20the%20gaps%3F&hashtags=lifeinconservation"

# Countries in the questions 
country <- read.csv("www/country.csv",  sep = ",", encoding = "UTF-8" )
nationality <- read.csv("www/nation.csv",  sep = ",", encoding = "UTF-8")


##############################################################
################  Initiate the user interface  ################
##############################################################

ui <- fluidPage(fluidRow(
  tags$head(tags$style(".rightAlign{float:right;}")),
  
  # Image width adjusts to window
  tags$head(tags$style(
    type = "text/css",
    "#frog img {max-width: 100%; width: 100%; height: auto}
    #fish img {max-width: 100%; width: 100%; height: auto}
    #monkey img {max-width: 100%; width: 100%; height: auto}
    #dive img {max-width: 100%; width: 100%; height: auto}
    #lion img {max-width: 100%; width: 100%; height: auto}
    #turt img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  column(10, offset = 1,
         tagList(
           # Call shinyjs
           useShinyjs(),
           extendShinyjs(text = jscode, functions = "refresh_1"),
           extendShinyjs(text = jsCode_txt),
          
           # Allowing us to use our custon CSS to colour * and potential errors red
           shinyjs::inlineCSS(appCSS),
           shinyjs::inlineCSS(appCSS_please),
           
           # Applying formatting to the navbar
           tags$style(
             HTML(
               
               "
               /* The minimum header size is 120px */
               nav.navbar.navbar-default {
               min-height: 120px;
               }
               
               .navbar .navbar-nav {
               float: right; /*  Making the navbar float on the right */
               
               /* My attempts to vertically align centrally - it does not work */
               display: inline-block;
               vertical-align: middle;
               text-align: right;
               
               /* Font size 24 */
               font-size: 24px;
               }
               
               /* The navbar header */
               .navbar .navbar-header {
               float: left; /* The logo floats on the left */
               
               /* Again, attempting to align vertically central */
               vertical-align: middle;
               display: inline-block;
               text-align:center;
               }
               
               /* Allowing the navbar to collapse into a menu when it's smaller than 840px - this was a bit of a hack, because the navbar was going onto the text */
               /* This combined with the issue I had with centering suggests that somehow although the navbar height appears to be 120px, everything is getting aligned with the original navbar height */
               @media (max-width: 800px) {
               .navbar-collapse.collapse {
               display: none !important;
               }
               .navbar-collapse.collapse.in {
               display: block !important;
               }
               .navbar-header .collapse, .navbar-toggle {
               display:block !important;
               }
               .navbar-header {
               float:none;
               }
               }
               "
             )),
           
           # Text has line-height 1.5
           tags$style(HTML("div {
                           line-height: 1.5;
                           font-size: 16px;
                           
                           }")), # color: black;

           # Bullet points have spacing between each line. 
           tags$style(HTML("ul {
                           line-height: 1.5;
                           font-size: 16px;
                           }
                           ul li { 
                           padding: 5px 0px; 
                           }")),
           
           tags$style(HTML("h1 {
                           color: #558618;
                           }")),
           
           tags$style(HTML("h3 {
                           color: #E88F0E;
                           }")),
           
           tags$style(HTML("h4 {
                           font-size: 18px;
                           color: black;
                           font-weight: 600;
                           line-height: 1.5;
                           padding: 2px 0px;

           }")),
           
           tags$style(HTML("hr {
                           display: block; height: 2px;
                          order: 0; 
border-top: 1px solid #ccc;
                           margin: 1em 0; padding: 0;
                           
                           }")),
           

           tags$style(HTML("h5 {
                           font-size: 20px;
                           color: green;
                           font-weight: 600;
                           line-height: 1.5;
                           padding: 2px 0px;

           }")),
           
           # Invalid input for age
           tags$style(HTML("input:invalid {
                           background-color: #FFCCCC;

           }")),

           # The favicon 
           list(tags$head(HTML('<link rel="icon", href="favcon.png", 
                      type="image/png" />'))
                ),
           div(style="padding: 1px 0px; width: '100%'",
               titlePanel(
                 title="", windowTitle="Life in conservation"
               )
           ),

           # # The image is used on the Navbar
           navbarPage(
                 title=div(img(src="ICCS_logo_Colour.png"), ""),

             # This means the navbar collapses into a smaller menu (set to 800px)
             collapsible = T,
             
             # Navbar id
             id = "navbar",
             
             # Set the theme to "simplex"
             theme = shinytheme("spacelab"),
             
             ################ Tab 1 ################
             # The secound tab - the survey itself, including multiple hidden pages
             
             tabPanel(
               id = "p2",
               
               # This is the title of the survey tab
               title = "Take the survey",
               
               # For reloading
               value = "refresh_1",
               
               ### Questions ###
               div(id = "loading-content"),
               
               ### If wanting to leave ###
               shinyjs::hidden(div(id = "leave_page",
                                   h3("Thank you!"),
                                   div("If you found any of the survey questions upsetting or distressing, we encourage you to speak with friends or family, or a health professional. If you have any questions, comments, complaints, or concerns please email Thomas Pienkowski (thomas.pienkowski@zoo.ox.ac.uk) or Professor E.J. Milner-Gulland (ej.milner-gulland@zoo.ox.ac.uk). If you still have concerns or complaints you can contact the Medical Sciences Interdivisional Research Ethics Committee at ethics@medsci.ox.ac.uk including the reference 'R62487/RE001'."),
                                   br(),
                                   
                                   fluidRow(column(width = 10, offset = 1,
                                                   br(),
                                                   uiOutput("turt"))), 
                                   br(),br())),
               
               hidden(
                 div(
                   id = "form",
                   
                   # Panels are hidden, and shown when certain conditions are met. These conditions relate to whether or not the next buttons have been pressed or not. 
                   
                   ### Page one ###
                   # Info and consent #
                   conditionalPanel(
                     id = "p1",
                     value = "refresh_1",
                     condition = "input.nextBtn_1 == 0",
                     
                     fluidRow(column(2, offset = 10,
                                     h3("Language"),
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation/", "English"))), 
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation-es/", "Española"))),
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation-fr/", "Française"))),
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation-ks/", "Kiswahili"))),
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation-pt/", "Portuguesa"))),
                                     tags$li(tags$b(tags$a(href="https://livedataoxford.shinyapps.io/lifeinconservation-Khm/", "ខ្មែរ។")))
                     )),
                     
                     fluidRow(                     
                       h1("About the survey and consent"),
                              hr(),
                              
                              # Project information
                              "We are collecting information for a research project exploring the rewards and challenges faced by people working in conservation practice or science.",
                              br(),
                              br(),
                              "This project is conducted by the Universities of Oxford, Edinburgh, and Royal Holloway, in the UK. You can find more about the research team on the 'About the study' page.",
                              br(),
                              br(),
                              "We are looking for people over the age of 18 to volunteer to take this survey, which will take around 10-15 minutes.",
                     br(),
                     br(),
                     "At the end of the survey, we will show you some of your results, compared to previous responses."),
                     
                     # Frog picture 
                     fluidRow(column(width = 10, offset = 1,
                                     br(),
                                     uiOutput("frog")
                     )),
                     
                     br(),

                     fluidRow(
                       
                       # The consent boxes
                       wellPanel(h3("Consent"),
                                 style = "background: #DCEEFE", 
                                 "Please read through the following points before you agree to take part. I understand that…",
                                 tags$ul(
                                   tags$li("Participation is voluntary."),
                                   tags$li("We won’t collect or share any personally identifiable information, including IP addresses."), 
                                   tags$li(tags$a(href="https://www.shinyapps.io/",  "shinyapps.io", target="_blank") ,"hosts the survey platform and", tags$a(href="https://www.rstudio.com/about/rstudio-and-the-gdpr-what-you-need-to-know/", "states that it is GDPR compliant.", target="_blank"), 
                                   "shinyapps.io collects and stores the IP addresses of visitors for 30 days before deleting them. shinyapps.io states that it does not control or process any information you provide in the survey. Please see their", tags$a(href="https://www.rstudio.com/about/privacy-policy/", "privacy policy", target="_blank"), "for more details."), 
                                   tags$li("The results will be published online, along with the anonymous data."), 
                                   tags$li("Some of the questions measure symptoms of distress."),
                                   tags$li("You are free to leave the survey at any time; each page has a button “leave the survey”. If you click this button, none of your responses will be saved."), 
                                   tags$li("Your responses are submitted at the end of the survey. Once submitted, you will not be able to withdraw since we have no way of identifying your response."), 
                                   tags$li("The study has been reviewed and approved by the", tags$a(href= "https://researchsupport.admin.ox.ac.uk/governance/ethics",  "University of Oxford Central University Research Ethics Committee", "(CUREC).", target="_blank")), 
                                   tags$li("For questions, comments, or complaints, please email Thomas Pienkowski (thomas.pienkowski@zoo.ox.ac.uk) or Professor E.J. Milner-Gulland (ej.milner-gulland@zoo.ox.ac.uk), and we will aim to get back to you within 10 working days."),
                                   tags$li("If you remain unhappy or wish to make a formal complaint, please email the Medical Sciences Interdivisional Research Ethics Committee (ethics@medsci.ox.ac.uk) quoting 'R62487/RE001'.")),
                                 radioButtons(inputId = "C1",
                                              tags$b("Do you consent to take part?"),
                                              choices = c("Yes"),
                                              selected = ""),
                                 br(),
                               
                                 # Age filter - numeric input (mandatory) 
                                   conditionalPanel(condition = "input.nextBtn_1 == 0 && input.age_prefernot != 1",
                                                    numericInput("age_year", min = 0, max = 110, label = labelMandatory(tags$b("How old are you?", id = "age_year")), value =  NA )),
                                    checkboxInput(inputId = "age_prefernot", (tags$b("Prefer not to say but over 18")), value = NULL, width = NULL),
                                 br(),
                                 radioButtons(
                                   inputId = "repeat_sur",
                                   labelMandatory(tags$b("Have you completed this survey (to the end) before?")),
                                   choices = c("Yes", "No"),
                                   selected = ""
                                 )

                       ),
                       
                       # Next page button
                       div(actionButton("nextBtn_1", tags$b("Next"),  class = "btn-success")),
                       
                       # Jump to top of page 
                       useShinyjs(), 
                       extendShinyjs(text = "shinyjs.nextBtn_1 = function() {window.scrollTo(0, 0);}"),
                       
                       # Next page button
                       fluidRow(br())
                     )),
                   
                   ### Page two ###
                   # Filter page #
                   conditionalPanel(
                     condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1",
                    
                  #########################################################################################################   
                  # GHQ1 - checkbox (mandatory)
                  fluidRow(wellPanel(style = "background: #DCEEFE", 
                                     
                                     tags$h5("Your personal goals"),
                                     tags$h4(labelPlease("Please think about your personal goals at work. Which of the following goals are important to you? Please select any or all that apply to you.")),
                                     
                                     
                                     ### Goal 7 
                                     checkboxInput(
                                       inputId = "GP_1_a",
                                       ("… making a meaningful contribution to conservation"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_1_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_1_b",labelPlease("How satisfied are you with your progress to these personal work goals?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 8 
                                     checkboxInput(
                                       inputId = "GP_2_a",
                                       ("… being a leader"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_2_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_2_b", labelPlease("How satisfied are you with your progress to these personal work goals?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 9 
                                     checkboxInput(
                                       inputId = "GP_3_a",
                                       ("… influencing other people’s behaviour"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_3_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_3_b", labelPlease("How satisfied are you with your progress to these personal work goals?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 10 
                                     checkboxInput(
                                       inputId = "GP_4_a",
                                       ("… earning money"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_4_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_4_b", labelPlease("How satisfied are you with your progress to these personal work goals?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     br(),
                                     br(),
                                     tags$h5("Conservation goals"),
                                     
                                     ### In a few words please describe in general terms the context or area that you were filling in these questions about”
                                     
                                     tags$h4("Please think about the conservation area or context that you are most familiar with. For example, this could be a place, a situation, or your focus of work, policy, or research."), 
                                     textAreaInput(inputId="areacontext", label="In a few words, please describe in general terms the context or area that you are thinking about (optional).", height="40px", value="", placeholder = "Type here (up to 150 characters)..."),
                                     br(),
                                     br(),
                                     
                                     tags$h4(labelPlease("When thinking about the conservation area or context you are most familiar with, which of the following goals are important for you? Please select any or all that apply to you.")),
                                     
                                     ### Goal 1 
                                     checkboxInput(
                                       inputId = "GP_5_a",
                                       ("… stopping human-driven species loss"),
                                       value = 0,
                                       width = NULL),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_5_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_5_b", labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 2 
                                     checkboxInput(
                                       inputId = "GP_6_a",
                                       ("… ensuring people benefit from nature in a sustainable way"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_6_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_6_b", labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 3 
                                     checkboxInput(
                                       inputId = "GP_7_a",
                                       ("… making sure people are treated equally and fairly"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_7_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_7_b", labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 4 
                                     checkboxInput(
                                       inputId = "GP_8_a",
                                       ("… avoiding conflict between people and conservation"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_8_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_8_b", labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 5 
                                     checkboxInput(
                                       inputId = "GP_9_a",
                                       ("… stopping damage to the natural world"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_9_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_9_b",labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     
                                     ### Goal 6 
                                     checkboxInput(
                                       inputId = "GP_10_a",
                                       ("… creating a more sustainable world"),
                                       value = 0,
                                       width = NULL
                                     ),
                                     
                                     conditionalPanel(condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year >= 18 && input.leave < 1 && input.GP_10_a == 1",
                                                      wellPanel(
                                                        radioButtons("GP_10_b", labelPlease("How satisfied are you with the progress that you think is being made by conservationists to achieving this goal?"), choices = c("Very dissatisfied" ,	"Dissatisfied", 	"Neutral",	"Satisfied" ,	"Very satisfied"), selected = character(0),
                                                                     inline = T))),
                                     br(),
                                     br(),
                                     tags$h5("The future of the area or context you are most familiar with"),
                                     radioButtons("SO_11", labelMandatory(tags$b("Thinking about the area or context you are most familiar with, how likely do you think it is that the most important conservation goals will be met in the next ten years?")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                  inline = T)

                  )),
                  ###############################################################################################################################################
                  
                     # Next page button
                     div("Page 1 of 6",
                         br(),
                         actionButton("nextBtn_2", tags$b("Next"), class = "btn-success")),
                     br(), 
                  textOutput("Unans_2_text", inline = T), uiOutput("GP1_text", inline = T), textOutput("Unans_2_and", inline = T), uiOutput("GP2_text", inline = T), uiOutput("Unans_2_quest", inline = T),
                  br(),
                  uiOutput("Unans_0_text", inline = T), 
                     extendShinyjs(text = "shinyjs.nextBtn_2 = function() {window.scrollTo(0, 0);}"),
                  # GP1_text
                  
                  shinyjs::hidden(span(id = "submit_msg_page1", "Progressing... please wait."),
                                  div(id = "errorpg1",
                                      div(
                                        br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg_pg1")
                                      ))),

                     br(),
                     fluidRow(br())
                   ),

                   ### If under the age of 18 ###
                   conditionalPanel(
                     condition = "input.nextBtn_1 == 1 && input.nextBtn_2 == 0 && input.age_year < 18",
                     h3("Thank you! Unfortunately, you have to be over the age of 18 to complete the survey."),
                     br(),
                     div("If you have any questions, comments, or concerns please email Thomas Pienkowski (thomas.pienkowski@zoo.ox.ac.uk) or Professor E.J. Milner-Gulland (ej.milner-gulland@zoo.ox.ac.uk). If you still have concerns or complaints you can contact the Medical Sciences Interdivisional Research Ethics Committee at ethics@medsci.ox.ac.uk including the reference 'R62487/RE001'."),
                     br(),
                     
                     fluidRow(column(width = 10, offset = 1,
                                     br(),
                                     uiOutput("lion"))), 
                     br(),br()
                   ),
                   
                   ### Page three ###
                   # Goal progress # 
                   
                   conditionalPanel(
                     condition = "input.nextBtn_2 == 1 && input.nextBtn_3 == 0 && input.age_year >= 18 && input.leave < 1",
                     
                     
                     #########################################################################################################   
                     # ERI 
                     fluidRow(wellPanel(style = "background: #DCEEFE",
                                        
                                        tags$h5("The challenges you face"),
                                        tags$h4("How much do you agree or disagree with the following statements?"),
                                        br(),
                                        uiOutput("ERI_1"),
                                        br(),
                                        uiOutput("ERI_2"),
                                        br(),
                                        
                                        uiOutput("ERI_3"),
                                        br(),
                                        
                                        uiOutput("ADD1"),
                                        br(),
                                        
                                        uiOutput("ADD2"),
                                        br(),
                                        
                                        uiOutput("ADD3"),
                                        br(),
                                        
                                        uiOutput("ERI_4"),
                                        br(),
                                        
                                        uiOutput("ERI_5"),
                                        br(),
                                        
                                        uiOutput("ERI_6"),
                                        br(),
                                        
                                        uiOutput("ERI_7"),
                                        br(),
                                        br(),
                                        tags$h5("The rewards you gain"),
                                        tags$h4("How much do you agree or disagree with the following statements?"),
                                        uiOutput("ERI_8"),
                                        br(),
                                        
                                        uiOutput("ERI_9"),
                                        br(),
                                        
                                        uiOutput("ERI_10"),
                                        br(),
                                        uiOutput("ADD4"),
                                        br(),
                                        uiOutput("ADD5")
                     )),

                     #########################################################################################################   
                     
                     
                     div("Page 2 of 6",
                         br(), actionButton("nextBtn_3", tags$b("Next"), class = "btn-success" )),
                     extendShinyjs(text = "shinyjs.nextBtn_3 = function() {window.scrollTo(0, 0);}"),
                     br(),
                     uiOutput("Unans_1_text", inline = T), 
                     
                     shinyjs::hidden(span(id = "submit_msg_page2", "Progressing... please wait."),
                                     div(id = "errorpg2",
                                         div(
                                           br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg_pg2")
                                         ))),

                     br(),
                     fluidRow(br())
                   ),
                   
                   ### Page four ###
                   # ERI #
                   conditionalPanel(
                     condition = "input.nextBtn_3 == 1 && input.nextBtn_4 == 0 && input.leave < 1",
                       
                     #########################################################################################################      
                     
                     fluidRow(wellPanel(style = "background: #DCEEFE",
                                        
                                        # Situational optimism 
                                        
                                        tags$h5("Thinking about the future of conservation"),
                                        
                                        ### SO country familiar  ###
                                        uiOutput("SO_coun"),
                                        br(),

                                        tags$h4("Thinking about the future of conservation in the country you are most familiar with, how probable is it that the following conservation goals will be achieved?"),
                                        br(),
                                        
                                        radioButtons("SO_1", labelMandatory(tags$b("Public support for conservation will grow over the next ten years")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_2", labelMandatory(tags$b("Government spending on conservation will grow over the next ten years")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_3", labelMandatory(tags$b("The harmful impact of people on nature will be less in ten years' time than it is now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_4", labelMandatory(tags$b("Human society will be more environmentally sustainable in ten years' time than it is now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will" ), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_5", labelMandatory(tags$b("There will be more wildlife in ten years' time than there is today")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will" ), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_6", labelMandatory(tags$b("There will be more natural areas and habitats in ten years' time than there are today")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_7", labelMandatory(tags$b("People will spend more recreational time in nature in ten years' time than they do now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_8", labelMandatory(tags$b("Nature will be able to provide the same benefits to people in ten years' time as now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_9", labelMandatory(tags$b("There will be more local participation in conservation in ten years' time than now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will"), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        
                                        radioButtons("SO_10", labelMandatory(tags$b("Conservationists will have better tools and knowledge in ten years' time than now")), choices = c("Definitely won't"	, "Probably won't"	,	"Probably will"	,"Definitely will" ), selected =  character(0),
                                                     inline = T),
                                        br(),
                                        br(),
                                        

                                        # Dispositional optimism 
                                        tags$h5("Thinking about the future in general"),
                                        tags$h4("How much do you agree or disagree with the following statements?"),
                                        br(),
                                        radioButtons("LOTR_1", labelMandatory(tags$b("In uncertain times, I usually expect the best")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree"), selected = character(0),
                                                     inline = T),
                                        
                                        br(),
                                        radioButtons("LOTR_2", labelMandatory(tags$b("If something can go wrong for me, it will")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree" ), selected = character(0),
                                                     inline = T),
                                        
                                        br(),
                                        radioButtons("LOTR_3", labelMandatory(tags$b("I'm always optimistic about my future")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree" ), selected = character(0),
                                                     inline = T),
                                        
                                        br(),
                                        radioButtons("LOTR_4", labelMandatory(tags$b("I hardly ever expect things to go my way")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree" ), selected = character(0),
                                                     inline = T),
                                        
                                        br(),
                                        radioButtons("LOTR_5", labelMandatory(tags$b("I rarely count on good things happening to me")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree" ), selected = character(0),
                                                     inline = T),
                                        
                                        br(),
                                        radioButtons("LOTR_6", labelMandatory(tags$b("Overall, I expect more good things to happen to me than bad")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree" ), selected = character(0),
                                                     inline = T))
                              
                     ),

                     #########################################################################################################   
                     
                     # Next page button
                     div("Page 3 of 6",
                         br(), actionButton("nextBtn_4", tags$b("Next"), class = "btn-success" )),
                     br(), 
                     extendShinyjs(text = "shinyjs.nextBtn_4 = function() {window.scrollTo(0, 0);}"),
                     uiOutput("Unans_3_text", inline = T),
                     
                     # Error 
                     shinyjs::hidden(span(id = "submit_msg_page3", "Progressing... please wait."),
                                     div(id = "errorpg3",
                                         div(
                                           br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg_pg3")
                                         ))),
                
                     br(),
                     fluidRow(br())
                   ),
                   
                   ################################
                   ### Page five ###
                   # Kessler social support 
                   conditionalPanel(condition = "input.nextBtn_4  == 1 && input.nextBtn_5 == 0 && input.leave < 1",
                                    
                                    
                                    
                                    #########################################################################################################                     
                                    ################################
                                    ################################
                                    # K10 - drop down
                                    fluidRow(wellPanel(style = "background: #DCEEFE",
                                                       
                                                       tags$h5("How you've been feeling"),
                                                       tags$h4("The following questions are about how you have been feeling during the past 30 days. About how often during the past 30 days did you feel…"),
                                                       br(),
                                                       radioButtons("K10_1", labelMandatory(tags$b("… tired out for no good reason?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected =  character(0),
                                                                    inline = T),
                                                       br(),
                                                       radioButtons("K10_2", labelMandatory(tags$b("…nervous?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected = character(0),
                                                                    inline = T),
                                                       
                                                       conditionalPanel(condition = "input.nextBtn_4  == 1 && input.leave < 1 && (input.K10_2 == 'A little of the time' || input.K10_2 == 'Some of the time' || input.K10_2 == 'Most of the time' || input.K10_2 == 'All of the time')",
                                                                        wellPanel(
                                                                          radioButtons("K10_3",labelMandatory("…so nervous that nothing could calm you down?"), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time"), selected = character(0),
                                                                                       inline = T))),
                                                       br(),
                                                       radioButtons("K10_4", labelMandatory(tags$b(" …hopeless?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time"), selected = character(0),
                                                                    inline = T) ,
                                                       
                                                       br(),
                                                       radioButtons("K10_5", labelMandatory(tags$b("…restless or fidgety?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected = character(0),
                                                                    inline = T),
                                                       
                                                       conditionalPanel(condition = "input.nextBtn_4  == 1 && input.leave < 1 && (input.K10_5 == 'A little of the time' || input.K10_5 == 'Some of the time' || input.K10_5 == 'Most of the time' || input.K10_5 == 'All of the time')",
                                                                        wellPanel(
                                                                          radioButtons("K10_6",labelMandatory("…so restless that you could not sit still?"), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time"), selected = character(0),
                                                                                       inline = T))),
                                                       br(),
                                                       radioButtons("K10_7", labelMandatory(tags$b("…depressed?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected = character(0),
                                                                    inline = T),
                                                       
                                                       conditionalPanel(condition = "input.nextBtn_4  == 1 && input.leave < 1 && (input.K10_7 == 'A little of the time' || input.K10_7 == 'Some of the time' || input.K10_7 == 'Most of the time' || input.K10_7 == 'All of the time')",
                                                                        wellPanel(
                                                                          radioButtons("K10_8",labelMandatory("…so depressed that nothing could cheer you up?"), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time"), selected = character(0),
                                                                                       inline = T))),
                                                       br(),
                                                       radioButtons("K10_9", labelMandatory(tags$b("…that everything was an effort?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected = character(0),
                                                                    inline = T),
                                                       br(),
                                                       radioButtons("K10_10", labelMandatory(tags$b("…worthless?")), choices = c("None of the time" ,	"A little of the time", 	"Some of the time",	"Most of the time" ,	"All of the time" ), selected = character(0),
                                                                    inline = T)
                                                       
                                    )),
                                    
                                    ################################
                                    #########################################################################################################   

                                    # Next page button
                                    div("Page 4 of 6",
                                        br(), disabled(actionButton("nextBtn_5", tags$b("Next") , class = "btn-success")) ),
                                    br(), 
                                    extendShinyjs(text = "shinyjs.nextBtn_5 = function() {window.scrollTo(0, 0);}"),
                                    uiOutput("Unans_4_text", inline = T),
                                    
                                    # Error 
                                    shinyjs::hidden(span(id = "submit_msg_page4", "Progressing... please wait."),
                                                    div(id = "errorpg4",
                                                        div(
                                                          br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg_pg4")
                                                        ))),
                                    br(),
                                    fluidRow(br())
                   ), 

                   ################################
                   
                   conditionalPanel(condition = "input.nextBtn_5  == 1 && input.nextBtn_6 == 0 && input.leave < 1",
                                    
                                    
                                    
                                    #########################################################################################################   
                                    #########################################################################################################   
                                    fluidRow(wellPanel(style = "background: #DCEEFE",
                                                       
                                                       tags$h5("Health and well-being"),
                                                       radioButtons("health", labelMandatory(tags$b("How is your physical health in general?")), choices = c("Very bad", "Bad ", "Fair", "Good", "Very good" ), selected = character(0),
                                                                    inline = T),
                                                       br(),
                                                       
                                                       radioButtons("SS1", labelMandatory(tags$b("How satisfied are you with your personal relationships?")), choices = c("Very dissatisfied", "Dissatisfied", "Neither", "Satisfied", "Very satisfied"), selected = character(0),
                                                                    inline = T),
                                                       br(),
                                                       radioButtons("SS2", labelMandatory(tags$b("How satisfied are you with the support you get from your friends and family?")), choices = c("Very dissatisfied", "Dissatisfied", "Neither", "Satisfied", "Very satisfied"), selected = character(0),
                                                                    inline =T),
                                                       br(),
                                                       radioButtons("SS3", labelMandatory(tags$b("How satisfied are you with the amount of time you are able to spend with friends and family?")), choices = c("Very dissatisfied", "Dissatisfied", "Neither", "Satisfied", "Very satisfied"), selected = character(0),
                                                                    inline =T),
                                                       br(),
                                                       br(),
                                                       tags$b("Thinking about where you spend most of your time, how much do you agree or disagree with the following statements?"),
                                                       br(),
                                                       radioButtons("PS_1", labelMandatory(tags$b("It is dangerous to go outside at night alone")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree"), selected = character(0),
                                                                    inline = T),
                                                       br(),
                                                       radioButtons("PS_2", labelMandatory(tags$b("My work puts me in dangerous situations")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree"), selected = character(0),
                                                                    inline = T),
                                                       br(),
                                                       radioButtons("PS_3", labelMandatory(tags$b("I do not feel safe, even where I live")), choices = c("Strongly disagree"	,	"Disagree", "Neither" ,	"Agree",	"Strongly agree"), selected = character(0),
                                                                    inline = T)
                                                       
                                                       
                                                      
                                    )),
                                    #########################################################################################################   

                                    # Next page button
                                    div("Page 5 of 6",
                                        br(), actionButton("nextBtn_6", tags$b("Next"), class = "btn-success" )) ,
                                    extendShinyjs(text = "shinyjs.nextBtn_6 = function() {window.scrollTo(0, 0);}"),
                                    br(),
                                    uiOutput("Unans_5_text", inline = T),
                                    # Error 
                                    shinyjs::hidden(span(id = "submit_msg_page5", "Progressing... please wait."),
                                                    div(id = "errorpg5",
                                                        div(
                                                          br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg_pg5")
                                                        ))),
                                    
                                    br(),
                                    fluidRow(br())
                                    
                   ),
                   conditionalPanel(condition = "input.nextBtn_6  == 1 && input.leave < 1",
                                    
                                    fluidRow( wellPanel(style = "background: #DCEEFE",
                                                        
                                                        tags$h5("Some questions about you"),
                                                        
                                                        # Conservation - checkbox (mandatory)   
                                                        radioButtons(
                                                          inputId = "Conservation",
                                                          labelMandatory(tags$b("Do you work or conduct research in nature conservation?")),
                                                          choices = c("Yes", "No"),
                                                          selected = ""
                                                        ),
                                                        br(),
                                                        
                                                        conditionalPanel(condition = "input.nextBtn_6  == 1 && input.leave < 1 && input.Conservation  == 'Yes' " ,
                                                                         wellPanel(numericInput("years_cons", min = 0, max = 100, label = labelMandatory(tags$b("How many years have you worked or conducted research in conservation?", id = "years_cons")), value =  NA ),
                                                        br(),
                                                        
                                                        # occup position # 
                                                        # education # 
                                                        radioButtons(
                                                          inputId = "position",
                                                          labelMandatory(tags$b("What is your main role?")),
                                                          choices = c( "Ranger",
                                                                       "Fieldworker",
                                                                       "Manager",
                                                                       "Administration" ,
                                                                       "Graduate student", 
                                                                       "Bachelors student" ,
                                                                       "Researcher",
                                                                       "Consultant/self-employed" ,
                                                                       "Policymaker" ,
                                                                       "Intern" ,
                                                                       "Other" 
                                                          ),
                                                          selected = character(0)
                                                        ))),
                                                        
                                                        # br(),
                                                        
                                                        ### Gender ###
                                                        radioButtons(
                                                          inputId = "gender",
                                                          labelMandatory(tags$b("What is your gender?")),
                                                          choices = c("Female", "Male","Prefer not to say", "Other"),
                                                          selected =  character(0)
                                                        ),
                                                        br(),
                                                        
                                                        # education # 
                                                        radioButtons(
                                                          inputId = "education",
                                                          labelMandatory(tags$b("What is your highest level of education?")),
                                                          choices = c("None", "Primary school", "Secondary school", "College", "University", "Prefer not to say"),
                                                          selected =  character(0)
                                                        ),
                                                        br(),
                                                        
                                                        ### Nation ###
                                                        uiOutput("Nation") ,
                                                        br(),
                                                        
                                                        ### Country of work ###
                                                        uiOutput("W_coun") ,
                                                        br(),
                                                        
                                                        conditionalPanel(condition = "input.nextBtn_6  == 1 && input.leave < 1 && input.WH_prefernot != 1",
                                                                         numericInput("WH", min = 0, max = 168, label = labelMandatory(tags$b("Approximately how many hours do you normally work each week?", id = "WH")), value =  NA )),
                                                        checkboxInput(inputId = "WH_prefernot", (tags$b("Prefer not to say")), value = 0, width = NULL),

                                                        br(),
                                                        br(), 
                                                        textAreaInput(inputId="endcomment", label="Do you have any thoughts or comments on the challenges and rewards experienced by those in conservation (optional)?", height="40px", value="", placeholder = "Type here...")
                                                        
                                                        ) )

                                    ) )),
               

               #########################################################################################################   
               
               ### What to do when questions are completed ###
               conditionalPanel(
                 condition = "input.nextBtn_6 == 1  && input.leave < 1",
                 # Submit button
                 # "Page 6 of 6",br(), 
                 actionButton("submit", "Submit", class = "btn-danger"), # Submit button is red (btn-primary)
                 extendShinyjs(text = "shinyjs.submit = function() {window.scrollTo(0, 0);}"),
                 br(),
                 br(),
                 uiOutput("Unans_6_text", inline = T),
                 # Saying the form is being submitted
                 shinyjs::hidden(span(id = "submit_msg", "Submitting... please wait a moment for your results!"),
                                 div(id = "error",
                                     div(
                                       br(), tags$b("Error: sorry please try again. If this issue persists please contact thomas.pienkowski@zoo.ox.ac.uk quoting error:"), span(id = "error_msg")
                                     ))),
                 br(),
                 br(),
                 
                 ### Thank you message saying the form was submitted ###
                 shinyjs::hidden(

                  ##################################################################
                  div(
                    id = "thankyou_msg",
                    
                    ############################################################################ 
                    ############################################################################ 
                    ############################################################################ 
                    
                    h3("Thank you for taking the survey!"),
                    h5("Your results have been saved, so you are free to close this window. Below are some results from the study so far."), 
                    br(), 
                    "The answers you provided will help us better understand the challenges and rewards faced by those in conservation. It is hoped that these findings will help inform efforts to protect those working in conservation.",

                    br(),

                    fluidRow(column(width = 12,offset = 0,
                                    tags$h5("Conservation and general optimism"),                 
                    "The figure below shows the relationship between 'general optimism' and 'conservation optimism' among all previous respondents. The black dot shows your results. These results are a snapshot based on how people were feeling when they were answering these questions. The blue lines show the average score of respondents so far, and so your results are relative to other people.")),
                    br(),
                    fluidRow(column(width = 4,offset = 0,
                                    "You can also see how the results vary by a persons job role or position, gender, or age bracket.",
                                    br(), br(),
                                    selectInput(inputId = "x", label = "Select variables",
                                                choices = c("Gender", "Position", "Age"),
                                                selected = ""),
                                    tags$h5("Conservation optimism"), 
                                    "Here we define conservation optimism as the expectation of positive conservation outcomes in an uncertain future.", 
                                    br(),
                                    tags$h5("General optimism"), 
                                    "General optimism is technically termed 'dispositional optimism', which is ”defined as the tendency to believe that one will generally experience good vs bad outcomes in life”", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/9547031", "(Scheier and Carver 1992).", target="_blank")
                    ),

                    column(width = 7,offset = 1,
                           withSpinner(plotOutput(outputId = "scatterplot", width = "600px", height = "600px"), type= 5),
                           downloadButton(outputId = "down", label = "Download figure", class = "btn-info"),
                           actionButton("twitter_share1",
                                        label = "Tweet",
                                        icon = icon("twitter"),
                                        onclick = sprintf("window.open('%s')", url_t),
                                        class = "btn-info"),
                           actionButton("more_info_1",
                                        label = "More info?",
                                        class = "btn-info")
                           )), 
                    
                    br(),

                    conditionalPanel(condition = "input.more_info_1 > 0",
                    fluidRow(column(width = 12,offset = 0,
                                    wellPanel(style = "background: #F0F0F0; display:inline-block",
                                    tags$h5("More information about this graph"),
                                    "This graph is based on questions about peoples expectations of positive outcomes in life in general and in the context of conservation. There are many contextual factors that mean the results may not be accurate for any given individual. Also, here we simply added the scores for each question together, but some questions might be better at measuring general and conservation optimism than others (in our study, we will take this into account). Here we made up four categories. 'All-round confident' answered more optimistically to both the general and conservation optimism questions than previous respondents. ‘Confident conservationist’ answered more optimistically to the conservation optimism questions, but less optimistically to the general optimism questions, than previous respondents. ‘Cautious conservationist’ answered less optimistically to the conservation optimism questions, but more optimistically to the general optimism questions, than previous respondents. ‘All-round cautious’ answered less optimistically to both the general and conservation optimism questions than previous respondents. (A small amount of random variation has been added so points are not overlapping.)"
                                    ),
                                    br()
                                    ))),


                    fluidRow(column(width = 12,offset = 0,
                                    tags$h5("The balance of 'challenges' and 'rewards'"),  
                                    "The figure below shows the relationship balance 'challenges' and 'reward' among all previous respondents. The black dot shows your results. These red areas suggest that someone might have been experiencing greater ‘challenges’ than ‘rewards’ when they were responding to the survey. However, this is only a snapshot of how people felt when they answered the questions.")),
                    br(),
                    
                    fluidRow(column(width = 4,offset = 0,
                                    "You can also see how the results vary by a persons job role or position, gender, or age bracket.",
                                    br(), br(),
                                    selectInput(inputId = "x2", label = "Select variables",
                                                choices = c("Gender", "Position", "Age"),
                                                selected = ""),
                                    tags$h5("Effort-reward imbalance model"), 
                                    "The effort-reward imbalance model (ERI) has been useful for understanding the well-being of workers in many sectors. However, it is worth remembering that although the results may be accurate on average across a group of people, they can be inaccurate for any specific person. Also, we have not yet tested how our modifications to the ERI (adding questions specific to conservation) affect the results. Because of this, you should not take these results too seriously.", 
                                    br()
                    ),

                    column(width = 7,offset = 1,
                           withSpinner(plotOutput(outputId = "scatterplot_ERI", width = "600px", height = "600px"), type= 5),
                           downloadButton(outputId = "down_eri", label = "Download figure", class = "btn-info"),
                           actionButton("twitter_share2",
                                        label = "Tweet",
                                        icon = icon("twitter"),
                                        onclick = sprintf("window.open('%s')", url_t2),
                                        class = "btn-info"),
                           actionButton("more_info_2",
                                        label = "More info?",
                                        class = "btn-info")
                           )
                    ), 
                    br(),
                    
                    conditionalPanel(condition = "input.more_info_2 > 0",
                                     fluidRow(column(width = 12,offset = 0,
                                                     wellPanel(style = "background: #F0F0F0; display:inline-block",
                                                               tags$h5("More information about this graph"),
                                                               "The effort-reward imbalance model (ERI)", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/9547031", "(Siegrist 1996)", target="_blank"), "describes the balance of challenges and rewards faced by people at work. We modified the ERI to add 'efforts' and 'rewards' specific to conservation. Since we modified the ERI to include some additional questions, we do not yet know if this is a good measure of how well efforts and rewards are balanced. Additionally, although we the red area indicates an 'imbalance' of efforts and rewards, this is a simplification. Within our study, the results of the ERI will be treated as continuous, rather than binary, with higher scores indicating greater probability of 'imbalance'. (A small amount of random variation has been added so points are not overlapping.)"
                                                     ),
                                                     br()
                                     ))),
                    
                    
                    fluidRow(column(width = 12,offset = 0,
                                    "This map shows the average (mean) conservation optimism (CO) scores across countries. These countries are those that respondents were thinking about when evaluating how optimistic they were about future conservation outcomes. Here we simply scored the responses for each CO question and added them together. Light yellow (0) shows the lowest conservation optimism scores so far, and dark blue (1) shows the highest. This may indicate in the levels of optimism about conservation in different countries. The table shows the total number of responses in each continent so far. This information is important for knowing which areas are better represented than others.")),
                    br(),
                    

                    fluidRow(
                    column(width = 6, offset = 1,
                           withSpinner(leafletOutput(outputId = "map", width = "600px", height = "400px")),
                                    downloadButton(outputId = "down_map", label = "Download figure", class = "btn-info"),
                                    actionButton("twitter_share3",
                                                 label = "Tweet",
                                                 icon = icon("twitter"),
                                                 onclick = sprintf("window.open('%s')", url_t3),
                                                 class = "btn-info")) 
                    
                    ,column(width = 4,offset = 1,
                           wellPanel(style = "background: #F0F0F0; display:inline-block",
                           tableOutput('response_table')))
                    ),
                    br(),
                    br(),
                    
                    fluidRow(column(width = 12,offset = 0,
                                    "If you found any of the survey questions upsetting or distressing, we encourage you to speak with friends or family, or a health professional. If you have any questions, comments, or complaints, please email Thomas Pienkowski (thomas.pienkowski@zoo.ox.ac.uk) or E.J. Milner-Gulland (ej.milner-gulland@zoo.ox.ac.uk). If you still have concerns or complaints, you can contact the Medical Sciences Interdivisional Research Ethics Committee at ethics@medsci.ox.ac.uk including the reference 'R62487/RE001'. Alternatively, if you would like to submit a message anonymously you can type it below and submit; however, we will not be able to respond to these messages.",
                                    textAreaInput(inputId="comment", label="", height="100px", value="", placeholder = "Type here..."),
                                    actionButton("submit_anonymous", label = "Submit comment"), 
                                    br(),
                                    shinyjs::hidden(span(id = "submit_msg_anonymous", "Submitted - thank you.")) # 

                                    
                                    )), 
                    br()
                    )
                  
                  ############################################################################ 
                  ############################################################################ 
                  ############################################################################ 
                  

                 )),
               
               # Restart or leave the survey 
               conditionalPanel(condition = "input.nextBtn_1 != 0 && input.submit == 0",
                                hr(),
                                fluidRow(column(width = 8, offset = 2, 
                               wellPanel(style = "background: #F0F0F0; display:inline-block",
                                         actionButton("leave", "I want to leave the survey", class = "btn-outline-danger"), actionButton("restart", "I want to re-start the survey", class = "btn-outline-danger"))
               ))) 
             ),
             
             ################ Tab 2 ################
             tabPanel(
               id = "p1_1",
               value ="ab_v",
               title = "About the study",
               fluidRow( h1("About the project"),
                         hr(),
                         
                         "This project explores the challenges and rewards that those working or conducting research in conservation face. The online survey aims to build a broad picture of these challenges and rewards. This will be accompanied by more in-depth surveys in selected conservation organisations.", 
                         br(),
                         br(),
                         "Some of the questions used in this survey are established psychological instruments. However, other questions were developed and piloted during this study. The results of this study will be published in peer-reviewed articles, alongside the anonymous data, and contribute to the Doctoral thesis of Thomas Pienkowski. We will also be providing a publically available summary of the results.",
                         br(),
                         fluidRow(column(width = 10, offset = 1,
                                         br(),
                                         uiOutput("fish")
                         )),
                         br(),
                         br(),
                         h3("About us"),
                         "This project is a collaboration between researchers at the Universities of Oxford, Edinburgh, and Royal Holloway. These researchers are based at the", tags$a(href="https://www.iccs.org.uk/", "Interdisciplinary Centre for Conservation Science", target="_blank"),"(ICCS),",
                         tags$a(href="https://edinburghconservationscience.com/", "Conservation Science", target="_blank"), "and",  tags$a(href="https://www.conservationbehaviour.com/", "Conservation and Behaviour", target="_blank"), "research groups.",
                         
                         fluidRow(column(width = 10, offset = 1,
                                         br(),
                                         uiOutput("monkey") )),
                         br(),
                         br(),
                         h3("Contact"),
                         tags$b("Interdisciplinary Centre for Conservation Science"),
                         br(), 
                         tags$a(href="https://www.iccs.org.uk/person/thomas-pienkowski", "Thomas Pienkowski", target="_blank"), 
                         br(), 
                         "Email: thomas.pienkowski@zoo.ox.ac.uk",
                         br(),
                         br(),
                         tags$a(href="https://www.iccs.org.uk/person/sofia-castello-y-tickell","Sofia Castello y Tickell", target="_blank"), 
                         
                         br(),
                         "Email: sofia.castelloytickell@zoo.ox.ac.uk",
                         br(),
                         br(),
                         tags$a(href="https://www.iccs.org.uk/person/ej-milner-gulland","E.J. Milner-Gulland", target="_blank"), 
                         br(),
                         "Email: ej.milner-gulland@zoo.ox.ac.uk",
                         br(),
                         br(),
                         "Address:",
                         br(),
                         "Department of Zoology, University of Oxford, Zoology Research and Administration Building, 11a Mansfield Rd, Oxford OX1 3SZ",
                         br(),
                         br(),
                         tags$b("Conservation Science"),
                         br(),
                         tags$a(href="https://edinburghconservationscience.com/2014/10/01/aidan-keane/", "Aidan Keane", target="_blank"), 
                         br(),
                         "Email: aidan.keane@ed.ac.uk",
                         br(),
                         br(),
                         "Address:",
                         br(),
                         "School of GeoSciences, University of Edinburgh, Crew Building, The King's Buildings, Alexander Crum Brown Road, Edinburgh EH9 3FF",
                         br(),
                         br(),
                         tags$b("Conservation and Behaviour"),
                         br(),
                         
                         tags$a(href="https://www.conservationbehaviour.com/current-members",  "Sarah Papworth", target="_blank"), 

                         br(),
                         "Email: sarah.papworth@rhul.ac.uk",
                         br(),
                         br(),
                         "Address:", 
                         br(),
                         "School of Biological Sciences, Royal Holloway, University of London, Egham Hill, Egham TW20 0EX",
                         br(),
                         fluidRow(column(width = 10, offset = 1,
                                         br(),
                                         uiOutput("dive") )),
                         br(),
                         br()
                         
               ))
             )))
  ))

##############################################################
################  Define the server processs  ################
##############################################################

server <- function(input, output, session) {
  
  # Setting the seed based on the system time (when an instance is initialised, it has the same seed every time)
  set.seed(as.integer(Sys.time()))

  # This is the random id of the observation
  randomID <- reactiveValues(ID_random = stri_rand_strings(1, 10 )) # round(runif(1,5,14),0)
  
  # Function for saving data 
  saveData <- function(data, time_duration, ID) {
    # Create a temporary file to hold the data
    data <- t(data)
    data <- cbind(data, time_duration) # add the duration 
    
    # The day
    date <- as.character(Sys.Date())
    data <- cbind(data, date) # add the duration 
    
    # File name 
    file_name <- paste0(ID,".csv")
    
    # Path 
    file_path <- file.path(tempdir(), file_name)
    write.csv2(data ,file_path, row.names = FALSE, quote = TRUE)
    
    # Upload the file to S3
    put_object(file = file_path, object = file_name, bucket = s3BucketName)
  }
  
  # add randomID$ID_random to summary df 
  
  
  # Load save function 
  load_save_Data <- function(data2, ID) {
    # 
    object <- get_object("summary_df.csv", s3BucketName)
    object_data <- readBin(object, "character")
    sample_data <- read.csv(text = object_data, stringsAsFactors = FALSE, encoding = "UTF-8")
    sample_data <- as.data.frame(sample_data)
    
    # Create a temporary file to hold the data
    data_df <- t(data2)
    data_df <- cbind(data_df, ID) 
    
    # rbind 
    total_df <- rbind(sample_data,data_df)
    
    # filename
    file_name <- "summary_df.csv"
    
    # Where to save
    file_path <- file.path(tempdir(), file_name)
    write.csv(total_df ,file_path, row.names = FALSE, quote = TRUE,  fileEncoding = "UTF-8") 
    
    # Upload the file to S3
    put_object(file = file_path, object = file_name, bucket = s3BucketName)
    
    # return the df
    return(total_df)
  }
  
  anonymous_save_Data  <- function(data3) {
    # 
    object <- get_object("comments_df.csv", s3BucketName)
    object_data <- readBin(object, "character")
    sample_data <- read.csv(text = object_data, stringsAsFactors = FALSE, encoding = "GBK")
    sample_data <- as.data.frame(sample_data)
    
    # Create a temporary file to hold the data
    data_df <- t(data3)
    
    # rbind 
    total_df <- rbind(sample_data,data_df)
    
    # filename
    file_name <- "comments_df.csv"
    
    # Where to save
    file_path <- file.path(tempdir(), file_name)
    write.csv(total_df ,file_path, row.names = FALSE, quote = TRUE)
    
    # Upload the file to S3
    put_object(file = file_path, object = file_name, bucket = s3BucketName)
    
    # return the df
    return(total_df)
  }
  

  # Simulate work being done for .4 second
  Sys.sleep(.1)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content",
       anim = TRUE,
       animType = "fade")
  show("form") # Show the form panel
  
  # The initial reactive value
  rv <- reactiveValues(page = -1)
  
  # The function that adds 1 to the reactive value
  navPage_1 <- function(direction) {
    rv$page <- rv$page + direction
    
  }
  
  # When navigating to the navpage, 1 is added to rv$page
  observeEvent(input$navbar == "refresh_1" ,  {
    rv$page <- navPage_1(1)
    # print(rv$page)
  })
  
  # if refresh_1 is pressed and rv is greater than 1
  observe({
    if (input$navbar == "refresh_1" && rv$page > 1) {
      js$refresh_1()
    }
  })
  
  # if refresh_1 is pressed and rv is greater than 1
  observeEvent(input$restart, {
    js$refresh_1()
  })
  
  # Show the option of leaving the survey
  observeEvent(input$leave, {
    shinyjs::show("leave_page")
    shinyjs::hide("form")
  })
  
  # Show the option of leaving the survey
  observeEvent(input$leave_page, {
    shinyjs::hide("leave")
  })

  ################ Page progression and question formating  ################
  ### Mandatory fields on page must be completed for progression ###
  
  # This reactive value is set to 1 as default
  #rv_2 <- reactiveValues(page = 1) # The initial reactive value
  
  # The function for adding the page value
  navPage_2 <- function(input, direction) {
    input <- input + direction
  }
  
  # When nextBtn is pressed, one is added to page number
  observeEvent(input$nextBtn_1, navPage_2(input$nextBtn_1, 1))
  observeEvent(input$nextBtn_2, navPage_2(input$nextBtn_2, 1))
  observeEvent(input$nextBtn_3, navPage_2(input$nextBtn_3, 1))
  observeEvent(input$nextBtn_4, navPage_2(input$nextBtn_4, 1))
  observeEvent(input$nextBtn_5, navPage_2(input$nextBtn_5, 1))
  observeEvent(input$nextBtn_6, navPage_2(input$nextBtn_6, 1))
  
# calculating time 
  time_dur <- reactiveValues(start_time = NA, end_time = NA, duration = NA)
  
  # When next page is pressed, go to top of next page (and measure start, end and duration time)
  # page 1
  observeEvent(input$nextBtn_1, {
    
    
    js$nextBtn_1()
    time_dur$start_time <- Sys.time()
    })
  
  #####################################################
  # page 2
  observeEvent(input$nextBtn_2, {
    js$nextBtn_2()
    
    # Duration 
    time_dur$end_time <- Sys.time()
    time_dur$duration <- difftime(time_dur$end_time, time_dur$start_time, units = "secs")
    
    # show progress
    shinyjs::disable("nextBtn_2")
    shinyjs::show("submit_msg_page1")
    shinyjs::hide("errorpg1")
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    },
    # If savng doesn't work, show the following error
  error = function(err) {
    shinyjs::html("error_msg_pg1", err$message)
    shinyjs::show(id = "errorpg1", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable("nextBtn_2")
  })
})
#####################################################  
  
  # page 3
  observeEvent(input$nextBtn_3, {
    js$nextBtn_3()
    
    # Duration 
    time_dur$end_time <- Sys.time()
    time_dur$duration <- difftime(time_dur$end_time, time_dur$start_time, units = "secs")
    
    # show progress
    shinyjs::disable("nextBtn_3")
    shinyjs::show("submit_msg_page2")
    shinyjs::hide("errorpg2")
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    },
    # If savng doesn't work, show the following error
    error = function(err) {
      shinyjs::html("error_msg_pg2", err$message)
      shinyjs::show(id = "errorpg2", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("nextBtn_3")
    })
  })
  

  # page 4
  observeEvent(input$nextBtn_4, {
    js$nextBtn_4()
    
    # Duration 
    time_dur$end_time <- Sys.time()
    time_dur$duration <- difftime(time_dur$end_time, time_dur$start_time, units = "secs")
    
    # show progress
    shinyjs::disable("nextBtn_4")
    shinyjs::show("submit_msg_page3")
    shinyjs::hide("errorpg3")
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    },
    # If savng doesn't work, show the following error
    error = function(err) {
      shinyjs::html("error_msg_pg3", err$message)
      shinyjs::show(id = "errorpg3", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("nextBtn_4")
    })
  })
  
  # page 5
  observeEvent(input$nextBtn_5, {
    js$nextBtn_5()
    
    # Duration 
    time_dur$end_time <- Sys.time()
    time_dur$duration <- difftime(time_dur$end_time, time_dur$start_time, units = "secs")
    
    # show progress
    shinyjs::disable("nextBtn_5")
    shinyjs::show("submit_msg_page4")
    shinyjs::hide("errorpg4")
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    },
    # If savng doesn't work, show the following error
    error = function(err) {
      shinyjs::html("error_msg_pg4", err$message)
      shinyjs::show(id = "errorpg4", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("nextBtn_5")
    })
  })

  
  # page 6
  observeEvent(input$nextBtn_6, {
    js$nextBtn_6()
    
    # Duration 
    time_dur$end_time <- Sys.time()
    time_dur$duration <- difftime(time_dur$end_time, time_dur$start_time, units = "secs")
    
    # show progress
    shinyjs::disable("nextBtn_6")
    shinyjs::show("submit_msg_page5")
    shinyjs::hide("errorpg5")
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    },
    # If savng doesn't work, show the following error
    error = function(err) {
      shinyjs::html("error_msg_pg5", err$message)
      shinyjs::show(id = "errorpg5", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("nextBtn_6")
    })
  })
  
  # submit 
  observeEvent(input$submit, {js$submit()})

  ### Page 1 completed ###
  observe({
    mandatoryFilled_C <- vapply(fieldsMandatory_C,
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]]  )
                                },
                                logical(1))
    mandatoryFilled_C <- all(mandatoryFilled_C)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_1", condition = mandatoryFilled_C)
  })
  
  # Nationality #
  
  # If selecting "prefer not to say" - then 999 is entered
  observe({
    if (is.null(input$age_prefernot)==T  || is.na(input$age_prefernot)==T   ) {
      updateRadioButtons(session, "age_year",  selected = NA)
    }
    else if(input$age_prefernot == 0) { 
      updateRadioButtons(session, "age_year",  selected = NA)
    } 
    else if(input$age_prefernot == 1 ) { 
      updateRadioButtons(session, "age_year",  selected = 999)
    }
  })
  
  # gender
  observeEvent(input$repeat_sur,{
    if(!(input$repeat_sur == "")){
      js$TextCol101("lightgrey")
    }
    else{
      return()
    }
  })

  # What is your nationality widget
  output$Nation <- renderUI({
    selectizeInput("Nation",
                   labelMandatory(tags$b("What is your nationality?")),
                   choices = nationality$name,
                   options = list(
                     placeholder = 'Please type here',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  # What is the main country you work in?
  output$W_coun <- renderUI({
    selectizeInput("W_coun",
                   labelMandatory(tags$b("In which country do you spend most of your time at the moment?")),
                   choices = country$name,
                   options = list(
                     placeholder = 'Please type here',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })

  # If selecting "prefer not to say" - then 999 is entered
  observe({
    if (is.null(input$WH_prefernot)==T  || is.na(input$WH_prefernot)==T   ) {
      updateRadioButtons(session, "WH",  selected = NA)
    }
    else if(input$WH_prefernot == 0) { 
      updateRadioButtons(session, "WH",  selected = NA)
    } 
    else if(input$WH_prefernot == 1 ) { 
      updateRadioButtons(session, "WH",  selected = 999)
    }
  })
  
  # Page 1
  output$Unans_0_text <- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  
  # Is page 2 and should you hide "Unanswered questions:" 
  observe({
    mandatoryFilled <- vapply(fieldsMandatory_1,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_2", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_0_text")} # We are no longer able to submit (again)
  })
  
  ### Page 2 ### 
  # Show "Unanswered questions:"   
  output$Unans_1_text <- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  

  # Is page 2 and should you hide "Unanswered questions:" 
  observe({
    mandatoryFilled <- vapply(fieldsMandatory_2,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_3", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_1_text")} # We are no longer able to submit (again)
  })
  
  # Conservation
  observeEvent(input$Conservation,{
    if(!(input$Conservation == "")){
      js$TextCol1("lightgrey")
    }
    else{
      return()
    }
  })
  
  # age_year
  observeEvent(input$age_year,{
    if(is.null(input$age_year) == T  || is.na(input$age_year)==T ){
      return()
    } else if(input$age_year > 0 && input$age_year <= 110) {
      js$TextCol2("lightgrey")
    } else{
      return()
    }
  })
  
  # gender
  observeEvent(input$gender,{
    if(!(input$gender == "")){
      js$TextCol3("lightgrey")
    }
    else{
      return()
    }
  })

  # education
  observeEvent(input$education,{
    if(!(input$education == "")){
      js$TextCol6("lightgrey")
    }
    else{
      return()
    }
  })
  
  # Nation
  observeEvent(input$Nation,{
    if(!(input$Nation == "")){
      js$TextCol4 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  
  # W_coun
  observeEvent(input$W_coun,{
    if(!(input$W_coun == "")){
      js$TextCol5("lightgrey")
    }
    else{
      return()
    }
  })
  
  # position
  observeEvent(input$position,{
    if(!(input$position == "")){
      js$TextCol7("lightgrey")
    }
    else{
      return()
    }
  })
  
  # WH
  observeEvent(input$WH,{
    if(is.null(input$WH) == T  || is.na(input$WH)==T ){
      return()
    } else if(input$WH > 0 && input$WH <= 160) {
      js$TextCol77("lightgrey")
    } else{
      return()
    }
  })
  
  # Years conservation 
  observeEvent(input$years_cons,{
    if(is.null(input$years_cons) == T  || is.na(input$years_cons)==T ){
      return()
    } else if(input$years_cons > 0 && input$years_cons <= 100) {
      js$TextCol102("lightgrey")
    } else{
      return()
    }
  })
  

  ### Page 3 ### 
  
  # Maximum text input 
  shinyjs::runjs("$('#areacontext').attr('maxlength', 150)")

  # Unanswered questions 
  output$Unans_2_text <- renderText("Are you sure you don't want to select any")
  #output$Unans_2_quest <- renderText(labelPlease("?")) 
  output$Unans_2_quest<- renderUI({labelPlease(tagList(tags$strong("?")))})  
  

  observe({
    if ((is.null(input$GP_1_a) == T && is.null(input$GP_2_a) == T && is.null(input$GP_3_a) == T && is.null(input$GP_4_a) == T && is.null(input$GP_5_a) == T && is.null(input$GP_6_a) == T) || ( is.null(input$GP_7_a) == T)&& is.null(input$GP_8_a) == T && is.null(input$GP_9_a) == T && is.null(input$GP_10_a) == T) {output$Unans_2_text <- renderText("Are you sure you don't want to select any")
    } else if((input$GP_1_a == 0 && input$GP_2_a == 0 && input$GP_3_a == 0 && input$GP_4_a == 0) || (  input$GP_5_a == 0 && input$GP_6_a == 0 && input$GP_7_a == 0 && input$GP_8_a == 0 && input$GP_9_a == 0 && input$GP_10_a == 0)  ) {output$Unans_2_text <- renderText("Are you sure you don't want to select any")
    } else { output$Unans_2_text <- renderText("")}
  })
  
  observe({
    if ((is.null(input$GP_1_a) == T && is.null(input$GP_2_a) == T && is.null(input$GP_3_a) == T && is.null(input$GP_4_a) == T && is.null(input$GP_5_a) == T && is.null(input$GP_6_a) == T) && ( is.null(input$GP_7_a) == T)&& is.null(input$GP_8_a) == T && is.null(input$GP_9_a) == T && is.null(input$GP_10_a) == T) {output$Unans_2_and <- renderText("&")
    } else if((input$GP_1_a == 0 && input$GP_2_a == 0 && input$GP_3_a == 0 && input$GP_4_a == 0) && (  input$GP_5_a == 0 && input$GP_6_a == 0 && input$GP_7_a == 0 && input$GP_8_a == 0 && input$GP_9_a == 0 && input$GP_10_a == 0)  ) {output$Unans_2_and <- renderText("&")
    } else { output$Unans_2_and <- renderText("")}
  })
  
  observe({
    if ((is.null(input$GP_1_a) == T && is.null(input$GP_2_a) == T && is.null(input$GP_3_a) == T && is.null(input$GP_4_a) == T && is.null(input$GP_5_a) == T && is.null(input$GP_6_a) == T) || ( is.null(input$GP_7_a) == T)&& is.null(input$GP_8_a) == T && is.null(input$GP_9_a) == T && is.null(input$GP_10_a) == T) {output$Unans_2_quest <- renderUI({labelPlease(tagList(tags$strong("?")))})  
    } else if((input$GP_1_a == 0 && input$GP_2_a == 0 && input$GP_3_a == 0 && input$GP_4_a == 0) || (  input$GP_5_a == 0 && input$GP_6_a == 0 && input$GP_7_a == 0 && input$GP_8_a == 0 && input$GP_9_a == 0 && input$GP_10_a == 0)  ) {output$Unans_2_quest <- renderUI({labelPlease(tagList(tags$strong("?")))})  
    } else { output$Unans_2_quest <- renderUI({(tagList(tags$strong("")))})  }
  })
  
  output$GP1_text <- renderUI({tagList(tags$strong("personal goals"))})
  
  observe({
    if (is.null(input$GP_1_a) == T && is.null(input$GP_2_a) == T && is.null(input$GP_3_a) == T && is.null(input$GP_4_a) == T ) {output$GP1_text <- renderUI({tagList(tags$strong("personal goals"))})
    } else if(input$GP_1_a == 0 && input$GP_2_a == 0 && input$GP_3_a == 0 && input$GP_4_a == 0  ) {output$GP1_text <- renderUI({tagList(tags$strong("personal goals"))})
    } else { output$GP1_text <- renderText("")}
  })
  
  output$GP2_text <- renderUI({tagList(tags$strong("conservation goals"))})
  observe({
    if (is.null(input$GP_5_a) == T && is.null(input$GP_6_a) == T && is.null(input$GP_7_a) == T && is.null(input$GP_8_a) == T && is.null(input$GP_9_a) == T && is.null(input$GP_10_a) == T ) {output$GP2_text <-  renderUI({tagList(tags$strong("conservation goals"))})
    } else if(input$GP_5_a == 0 && input$GP_6_a == 0 && input$GP_7_a == 0 && input$GP_8_a == 0 && input$GP_9_a == 0 && input$GP_10_a == 0 ) {output$GP2_text <- renderUI({tagList(tags$strong("conservation goals"))})
    } else { output$GP2_text <- renderText("")}
  })
  
 
  # GP
  observeEvent(input$GP_1_b,{
    if(!(input$GP_1_b == "")){
      js$TextCol91("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_2_b,{
    if(!(input$GP_2_b == "")){
      js$TextCol92("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_3_b,{
    if(!(input$GP_3_b == "")){
      js$TextCol93("lightgrey")
    }
    else{
      return()
    }
  })

  observeEvent(input$GP_4_b,{
    if(!(input$GP_4_b == "")){
      js$TextCol94("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_5_b,{
    if(!(input$GP_5_b == "")){
      js$TextCol95("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_6_b,{
    if(!(input$GP_6_b == "")){
      js$TextCol96("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_7_b,{
    if(!(input$GP_7_b == "")){
      js$TextCol97("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_8_b,{
    if(!(input$GP_8_b == "")){
      js$TextCol98("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_9_b,{
    if(!(input$GP_9_b == "")){
      js$TextCol99("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$GP_10_b,{
    if(!(input$GP_10_b == "")){
      js$TextCol100("lightgrey")
    }
    else{
      return()
    }
  })
  # GP_10_a

  ### Page 4 ### 
  ### ERI 
  choi <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
  choi2 <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree", "Not applicable")
  output$ERI_1 <- renderUI({
    radioButtons("ERI_1", labelMandatory(tags$b("I have constant time pressure due to a heavy work load")), choices = choi, selected =  character(0),
                 inline = T) })
  
  output$ERI_2 <- renderUI({
    radioButtons("ERI_2", labelMandatory(tags$b("I have many interruptions and disturbances while performing my job")), choices = choi, selected = character(0),
                 inline = T)})
  
  output$ERI_3 <- renderUI({
    radioButtons("ERI_3", labelMandatory(tags$b("Over the past few years, my job has become more and more demanding")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ADD1 <- renderUI({
    radioButtons("ADD1", labelMandatory(tags$b("I do not have the resources I need to achieve my work goals")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ADD2 <- renderUI({
    radioButtons("ADD2", labelMandatory(tags$b("The organisation I work for does not have enough funding to achieve its main aims")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ADD3 <- renderUI({
    radioButtons("ADD3", labelMandatory(tags$b("The organisation I work for may not exist in five years’ time")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ERI_4 <- renderUI({
    radioButtons("ERI_4", labelMandatory(tags$b("I receive the respect I deserve from my boss and work colleagues")), choices = choi2, selected = character(0),
                 inline = T)})
  
  output$ERI_5 <- renderUI({
    radioButtons("ERI_5", labelMandatory(tags$b("My job promotion or advancement prospects are poor")), choices = choi2, selected = character(0),
                 inline = T)})
  
  output$ERI_6 <- renderUI({
    radioButtons("ERI_6", labelMandatory(tags$b("I have experienced or I expect to experience an undesirable change in my work situation")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ERI_7 <- renderUI({
    radioButtons("ERI_7", labelMandatory(tags$b("My job security is poor")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ERI_8 <- renderUI({
    radioButtons("ERI_8", labelMandatory(tags$b("Considering all my efforts and achievements, I receive the respect and prestige I deserve at work")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ERI_9 <- renderUI({
    radioButtons("ERI_9", labelMandatory(tags$b("Considering all my efforts and achievements, my job promotion or advancement prospects are adequate")), choices = choi2, selected = character(0),
                 inline = T) })
  
  output$ERI_10 <- renderUI({
    radioButtons("ERI_10", labelMandatory(tags$b("Considering all my efforts and achievements, my salary or income is alright")), choices = choi, selected = character(0),
                 inline = T) })

  output$ADD4 <- renderUI({
    radioButtons("ADD4", labelMandatory(tags$b("I am satisfied with the contribution I make to conservation")), choices = choi, selected = character(0),
                 inline = T) })
  
  output$ADD5 <- renderUI({
    radioButtons("ADD5", labelMandatory(tags$b("My friends and family are proud that I work in conservation")), choices = choi, selected = character(0),
                 inline = T) })

  # Complete? #
  output$Unans_3_text <- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory_4,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_4", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_3_text")} # We are no longer able to submit (again)
    
  })

  # Unanswered 
  # ERI_1
  observeEvent(input$ERI_1,{
    if(!(input$ERI_1 == "")){
      js$TextCol28("lightgrey")
    }
    else{
      return()
    }
  })

  observeEvent(input$ERI_2,{
    if(!(input$ERI_2 == "")){
      js$TextCol29("lightgrey")
    }
    else{
      return()
    }
  })

  observeEvent(input$ERI_3,{
    if(!(input$ERI_3 == "")){
      js$TextCol30("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ADD1,{
    if(!(input$ADD1 == "")){
      js$TextCol38("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ADD2,{
    if(!(input$ADD2 == "")){
      js$TextCol39("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ADD3,{
    if(!(input$ADD3 == "")){
      js$TextCol40("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_4,{
    if(!(input$ERI_4 == "")){
      js$TextCol31 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_5,{
    if(!(input$ERI_5 == "")){
      js$TextCol32 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_6,{
    if(!(input$ERI_6 == "")){
      js$TextCol33 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_7,{
    if(!(input$ERI_7 == "")){
      js$TextCol34 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_8,{
    if(!(input$ERI_8 == "")){
      js$TextCol35 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ERI_9,{
    if(!(input$ERI_9 == "")){
      js$TextCol36 ("lightgrey")
    }
    else{
      return()
    }
  })

  observeEvent(input$ERI_10,{
    if(!(input$ERI_10 == "")){
      js$TextCol37 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ADD4,{
    if(!(input$ADD4 == "")){
      js$TextCol41 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$ADD5,{
    if(!(input$ADD5 == "")){
      js$TextCol42 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  ### Page 5 ### 
  # Page 5 complete? 
  output$Unans_4_text <- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory_5,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_5", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_4_text")} # We are no longer able to submit (again)
  })
  
  #################################################################################################
  # What is the main country you work in?
  output$SO_coun <- renderUI({
    selectizeInput("SO_coun",
                   labelMandatory(tags$b("Which country’s conservation context are you most familiar with? If you are familiar with multiple countries, please select one representative country.")),
                   choices = country$name,
                   options = list(
                     placeholder = 'Please type here',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })

  # Unanswered # 
  observeEvent(input$SO_coun,{
    if(!(input$SO_coun == "")){
      js$TextCol80 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_1,{
    if(!(input$SO_1 == "")){
      js$TextCol54 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_2,{
    if(!(input$SO_2 == "")){
      js$TextCol55 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_3,{
    if(!(input$SO_3 == "")){
      js$TextCol56 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_4,{
    if(!(input$SO_4 == "")){
      js$TextCol57 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_5,{
    if(!(input$SO_5 == "")){
      js$TextCol58 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_6,{
    if(!(input$SO_6 == "")){
      js$TextCol59 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_7,{
    if(!(input$SO_7 == "")){
      js$TextCol60 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_8,{
    if(!(input$SO_8 == "")){
      js$TextCol61 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_9,{
    if(!(input$SO_9 == "")){
      js$TextCol62 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_10,{
    if(!(input$SO_10 == "")){
      js$TextCol63 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SO_11,{
    if(!(input$SO_11 == "")){
      js$TextCol64 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_1,{
    if(!(input$LOTR_1 == "")){
      js$TextCol65 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_2,{
    if(!(input$LOTR_2 == "")){
      js$TextCol66 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_3,{
    if(!(input$LOTR_3 == "")){
      js$TextCol67 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_4,{
    if(!(input$LOTR_4 == "")){
      js$TextCol68 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_5,{
    if(!(input$LOTR_5 == "")){
      js$TextCol69 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$LOTR_6,{
    if(!(input$LOTR_6 == "")){
      js$TextCol70 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  #################################################################################################

  ### Page 6 ###
  # Page complete
  output$Unans_5_text<- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory_6,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Proceed button
    shinyjs::toggleState(id = "nextBtn_6", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_5_text")} # We are no longer able to submit (again)
    
  })
  


  # Assign value "None of the time" for K10_3 if "None of the time for K10_2 is selected
  observe({
    if (is.null(input$K10_2)==T  || is.na(input$K10_2)==T  ) {
      return()
    }
    else if(input$K10_2 == "A little of the time" || input$K10_2 == 	"Some of the time" ||input$K10_2 ==	"Most of the time" ||input$K10_2 ==	"All of the time"  ) { 
      updateRadioButtons(session, "K10_3",  selected = character(0))
    } 
    else if(input$K10_2 == "None of the time" ) { 
      updateRadioButtons(session, "K10_3",  selected = "None of the time")
    }
    
  })
  
  # Assign value "None of the time" for K10_6 if "None of the time" for K10_5 is selected
  observe({
    if (is.null(input$K10_5)==T  || is.na(input$K10_5)==T  ) {
      return()
    }
    else if(input$K10_5 == "A little of the time" || input$K10_5 == 	"Some of the time" ||input$K10_5 ==	"Most of the time" ||input$K10_5 ==	"All of the time"  ) { 
      updateRadioButtons(session, "K10_6",  selected = character(0))
    } 
    else if(input$K10_5 == "None of the time" ) { 
      updateRadioButtons(session, "K10_6",  selected = "None of the time")
    }
  })
  
  # Assign value "None of the time" for K10_8 if "None of the time" for K10_7 is selected
  observe({
    if (is.null(input$K10_7)==T  || is.na(input$K10_7)==T  ) {
      return()
    }
    else if(input$K10_7 == "A little of the time" || input$K10_7 == 	"Some of the time" ||input$K10_7 ==	"Most of the time" ||input$K10_7 ==	"All of the time"  ) { 
      updateRadioButtons(session, "K10_8",  selected = character(0))
    } 
    else if(input$K10_7 == "None of the time" ) { 
      updateRadioButtons(session, "K10_8",  selected = "None of the time")
    }
    
  })
  


  #################################################################################################
  # Unanswered # 
  observeEvent(input$K10_1,{
    if(!(input$K10_1 == "")){
      js$TextCol44("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_2,{
    if(!(input$K10_2 == "")){
      js$TextCol45("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_3,{
    if(!(input$K10_3 == "")){
      js$TextCol46("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_4,{
    if(!(input$K10_4 == "")){
      js$TextCol47("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_5,{
    if(!(input$K10_5 == "")){
      js$TextCol48("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_6,{
    if(!(input$K10_6 == "")){
      js$TextCol49("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_7,{
    if(!(input$K10_7 == "")){
      js$TextCol50("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_8,{
    if(!(input$K10_8 == "")){
      js$TextCol51("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_9,{
    if(!(input$K10_9 == "")){
      js$TextCol52("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$K10_10,{
    if(!(input$K10_10 == "")){
      js$TextCol53("lightgrey")
    }
    else{
      return()
    }
  })
  
  #################################################################################################
   
  # page 7 # 
  # Unanswered # 
  observeEvent(input$PS_1,{
    if(!(input$PS_1 == "")){
      js$TextCol71 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$PS_2,{
    if(!(input$PS_2 == "")){
      js$TextCol72 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$PS_3,{
    if(!(input$PS_3 == "")){
      js$TextCol73 ("lightgrey")
    }
    else{
      return()
    }
  })
  

  observeEvent(input$health,{
    if(!(input$health == "")){
      js$TextCol78 ("lightgrey")
    }
    else{
      return()
    }
  })

  observeEvent(input$SS1,{
    if(!(input$SS1 == "")){
      js$TextCol74 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SS2,{
    if(!(input$SS2 == "")){
      js$TextCol75 ("lightgrey")
    }
    else{
      return()
    }
  })
  
  observeEvent(input$SS3,{
    if(!(input$SS3 == "")){
      js$TextCol86 ("lightgrey")
    }
    else{
      return()
    }
  })

  ################  Mandatory fields must be completed before submission ################
  ### All completed ###
  output$Unans_6_text<- renderUI({labelMandatory(tagList(tags$strong("Please complete all the questions to proceed")))})  
  observe({
    mandatoryFilled <- vapply(fieldsAll,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    if (mandatoryFilled == T) {shinyjs::hide("Unans_6_text")} # We are no longer able to submit (again)
    
  })
  
  #### Conservation questions 
  # Assign value "None of the time" for K10_8 if "None of the time" for K10_7 is selected
  observe({
    if (is.null(input$Conservation)==T  || is.na(input$Conservation)==T  ) {
      return()
    }
    else if(input$Conservation == "Yes" ) { 
      updateRadioButtons(session, "position",  selected = character(0))
      updateRadioButtons(session, "years_cons",  selected = NA)
      
      
    } 
    else if(input$Conservation == "No" ) { 
      updateRadioButtons(session, "position",  selected = "Other")
      updateRadioButtons(session, "years_cons",  selected = 999)
    }
  })
  
  
  ################   Saving the form data and thanking respondents  ################
  ### What will be saved ###
  formData <- reactive({
    data <- sapply(saveAll, function(x) input[[x]])
    data
  })
  
  formData2 <- reactive({
    data <- sapply(save_sample, function(x) input[[x]])
    data
  })
  
  formData3 <- reactive({
    data <- sapply(save_comment, function(x) input[[x]])
    data
  })
  
  # Function to set all values to NULL if somenoe leaves
  delete_funct <- function(x) {
    x <- NULL
  }
  
  # Observe if people leave 
  observeEvent(input$leave, {
    tryCatch({
      formData <- reactive({
        data <- sapply(saveAll, delete_funct)
        data
      })
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
    })
  })

  ### When the submit button is pressed, the following occurs ###
  observeEvent(input$submit, {
    shinyjs::hide("submit") # We are no longer able to submit (again)
    shinyjs::show("submit_msg") # The submit_msg is shown
    shinyjs::hide("error") # Any error messages are hidden
    
    # Add a try catch - to try and save the data but not crash everything if it doens't work.
    tryCatch({
      saveData(formData(), time_dur$duration, randomID$ID_random) # Try to save the data
      
      ################### Load the summary data ############################ 
      df_sample <- load_save_Data(formData2(), randomID$ID_random)

      # Shapefile #########################################################
      # Shapefile 
      world_n = st_read("www/shape/world_n.shp")
      
      #############################################
      ######## Revert back to English #############
      #############################################
      
      # Need to match SO_count on any of the original, french, or spanish names 
      # As charecter 
      df_sample$SO_coun <- as.character(df_sample$SO_coun)
      country[,1:4] <-sapply(country, as.character)
      
      # Translation function 
      translate_fun<- function(test_df, df_coun ){
        coun_name <- paste(df_coun)
        df_so <- ifelse(test_df %in% coun_name, coun_name[1], test_df)
        return(df_so)
      }
      
      # Recode for each country 
      for (i in seq_along(1:nrow(country))){
        df_sample$SO_coun <- translate_fun(test_df = df_sample$SO_coun , df_coun = country[i,]  )
      }
      
      ##### The survey results  ##### 
      # Change nation to factor
      df_sample$SO_coun <- as.factor(df_sample$SO_coun)
      df_sample$obser <- 1 # for count of countries
      
      
      # SO recode 
      backSO <- function(x) {
        x <- as.numeric(ifelse(x %in% c("Definitely won't", "Définitivement aucun", "Definitivamente no se cumplirán", "Não serão de todo", "Hayatafikiwa kabisa"), 1, 
                               ifelse(x %in% c("Probably won't", "Probablement aucun", "Probablemente no se cumplirán", "Pouco provável que sejam" , "Labda hayatafikiwa"), 2, 
                                      ifelse(x %in% c( "Probably will", "Probablement certain", "Probablemente se cumplirán", "Provavelmente serão", "Labda yatafikiwa"), 3,
                                             ifelse(x %in% c( "Definitely will", "Définitivement certain", "Definitivamente se cumplirán", "Serão certamente", "Yatafikiwa kabisa"), 4, NA)))))
      }
      
      # LOTR recode positive
      backLOTR <- function(x) {
        x <- as.numeric(ifelse(x %in% c("Strongly disagree", "Absolument pas d'accord", "Totalmente en desacuerdo", "Discordo inteiramente", "Sikubaliani kabisa"), 1, 
                               ifelse(x %in% c("Disagree" , "Pas d’accord", "En desacuerdo", "Discordo", "Sikubaliani"), 2, 
                                      ifelse(x %in% c( "Neither", "Jamais", "Ninguna de las opciones" , "Nenhuma", "Wala au hata"), 3,
                                             ifelse(x %in% c( "Agree", "D'accord", "De acuerdo", "Concordo", "Nakubaliana"), 4,
                                                    ifelse(x %in% c( "Strongly agree", "Tout à fait d'accord", "Totalmente de acuerdo", "Concordo inteiramente", "Nakubaliana kabisa"), 5, NA))))))
      }
      # LOTR recode negative
      backLOTR_neg  <- function(x) {
        x <- as.numeric(ifelse(x %in% c("Strongly agree",  "Tout à fait d'accord", "Totalmente de acuerdo", "Concordo inteiramente", "Nakubaliana kabisa"), 1, 
                               ifelse(x %in% c("Agree", "D'accord" , "De acuerdo", "Concordo", "Nakubaliana"), 2, 
                                      ifelse(x %in% c("Neither" , "Jamais", "Ninguna de las opciones", "Nenhuma", "Wala au hata"), 3,
                                             ifelse(x %in% c("Disagree" , "Pas d’accord", "En desacuerdo", "Discordo", "Sikubaliani"), 4,
                                                    ifelse(x %in% c("Strongly disagree", "Absolument pas d'accord", "Totalmente en desacuerdo", "Discordo inteiramente", "Sikubaliani kabisa"), 5, NA))))))
      }
      
      #############################################
      ######## Revert back to English #############
      #############################################
      
      # Transform back to numeric 
      df_sample[, c("SO_1","SO_2","SO_3","SO_4","SO_5","SO_6" ,"SO_7","SO_8","SO_9","SO_10","SO_11")]<- lapply(df_sample[, c("SO_1","SO_2","SO_3","SO_4","SO_5","SO_6" ,"SO_7","SO_8","SO_9","SO_10","SO_11")], function(x){ backSO(x) })
      
      # Transform back to numeric 
      df_sample[, c("LOTR_1","LOTR_3","LOTR_6")] <- lapply(df_sample[, c("LOTR_1","LOTR_3","LOTR_6")], function(x){ backLOTR(x) } )
      df_sample[, c("LOTR_2","LOTR_4","LOTR_5")] <- lapply(df_sample[, c("LOTR_2","LOTR_4","LOTR_5")], function(x){ backLOTR_neg(x) } )
      
      # summing up the optimism variable 
      df_sample$SO_sum <- rowSums(df_sample[, c("SO_1","SO_2","SO_3","SO_4","SO_5","SO_6" ,"SO_7","SO_8","SO_9","SO_10","SO_11")])
      df_sample$LOTR_sum <- rowSums(df_sample[, c("LOTR_1","LOTR_2","LOTR_3","LOTR_4","LOTR_5","LOTR_6")])
      

      # sum value and count of observatin per country 
      df_sample_sub <- df_sample %>%
        dplyr::select(SO_coun, SO_sum, LOTR_sum, obser) %>%
        dplyr::group_by(SO_coun) %>%
        dplyr::summarise(SO_sum_c = sum(SO_sum), LOTR_sum_c = sum(LOTR_sum), count_c = sum(obser))
      
      # mean observation per country 
      df_sample_sub$SO_mean_c <- df_sample_sub$SO_sum_c/df_sample_sub$count_c
      df_sample_sub$LOTR_mean_c <- df_sample_sub$LOTR_sum_c/df_sample_sub$count_c
      
      # Change mismatch countries 
      world_n$name_long <- revalue(world_n$name_long, c("United States"="United States of America", "The Gambia" = "Gambia",
                                                        "Dem. Rep. Korea"="North Korea", "Russian Federation" = "Russia", 
                                                        "Republic of Korea" = "South Korea"))
      
      
      
      # bind spatial data for countries with observations 
      merged_df <- merge(df_sample_sub,world_n,by.x ="SO_coun", by.y = "name_long", all.y=T )
      
      # Copy spatial data
      merged_df_2 <- st_as_sf(merged_df)
      merged_df_2<- merged_df_2[!merged_df_2$count == 0, ] %>% na.omit()
      
      # Jitter the data a bit 
      df_sample$SO_sum <- jitter(df_sample$SO_sum,2.5)
      df_sample$LOTR_sum <- jitter(df_sample$LOTR_sum,2.5)
      
      # Ensure its a numeric variable
      df_sample$age_year <- as.numeric(as.character(df_sample$age_year))
      
      # Age brackets 
      df_sample$Age <- as.ordered(ifelse((df_sample$age_year <29), "18-29",
                                         ifelse((df_sample$age_year >=30 & df_sample$age_year <=41), "30-41",
                                                ifelse((df_sample$age_year >=42 & df_sample$age_year <=53), "42-53", 
                                                       ifelse((df_sample$age_year >=54 & df_sample$age_year <=65), "54-65", 
                                                              ifelse((df_sample$age_year >=65 & df_sample$age_year <=110), "65+", 
                                                      "Prefer not to say"))))))
      
      # Mean range 0-1
      range01 <- function(x){
        range_inf <- (x-min(x))/(max(x)-min(x))
        return(range_inf)
        #range_sorted <- ifelse( (range_inf== Inf || range_inf == -Inf || is.na(range_inf)), 1, range_inf)
        #return(range_sorted)
        }
      merged_df_2$SO_mean_c <- range01(merged_df_2$SO_mean_c)
      merged_df_2$LOTR_mean_c <- range01(merged_df_2$LOTR_mean_c)
      
      # DF
      merged_df_2_DF <- data.frame("SO_coun" = merged_df_2$SO_coun, "count" = merged_df_2$count, "SO_mean" = merged_df_2$SO_mean_c, "LOTR_mean" = merged_df_2$LOTR_mean_c, "SO_sum"=merged_df_2$SO_sum_c,"LOTR_sum"=merged_df_2$LOTR_sum_c,"continent"=merged_df_2$continent)
      
      # sum by continent
      merged_df_3_DF <- merged_df_2_DF %>%
        dplyr::select(continent,LOTR_sum, SO_sum, count) %>%
        dplyr::group_by(continent) %>%
        dplyr::summarise(count = sum(count), SO_total =  sum(SO_sum), LOTR_total =  sum(LOTR_sum))
      
      merged_df_3_DF$count <- as.integer(merged_df_3_DF$count)
      merged_df_3_DF$SO_mean <- merged_df_3_DF$SO_total/merged_df_3_DF$count
      merged_df_3_DF$LOTR_mean <- merged_df_3_DF$LOTR_total/merged_df_3_DF$count
      merged_df_3_DF$SO_total <- NULL
      merged_df_3_DF$LOTR_total <- NULL

      ##########################################
      binpal <- colorBin("YlGnBu", merged_df_2$SO_mean_c, 9, pretty = FALSE)
      
      # Create map
      output$map = renderLeaflet({
        leaflet() %>% addTiles(options = providerTileOptions(noWrap = F)) %>%
          setView(0, 0, zoom=.7) %>%
          addPolygons(data = merged_df_2[,c("SO_mean_c") ],
                      fillColor = ~binpal(merged_df_2$SO_mean_c),
                      weight = 1,
                      opacity = 1,
                      color = "white",
                      #dashArray = "3",
                      fillOpacity = 0.7) %>% 
          
          addLegend("bottomright", colors =c("#ffffd9",  "#c7e9b4", "#41b6c4","#225ea8","#081d58"), values =merged_df_2$SO_mean_c,
                    title = "CO",
                    opacity = 1,
                    labels= c("Low", "","","", "High"))
      })
      
      ##########################################
      mycolors <- c("#ffffd9",  "#c7e9b4", "#41b6c4","#225ea8","#081d58")
      world_map <- map_data("world")
      
      world_map$region <- ifelse(world_map$region == "USA", "United States of America", 
                                 ifelse(world_map$region == "UK", "United Kingdom", world_map$region))
      
      merged_df_2_DF$SO_coun = as.character(merged_df_2_DF$SO_coun)
      mapdata <- left_join(world_map, merged_df_2_DF, by = c('region' = 'SO_coun'), all.x=T)
      mapdata2 = mapdata[order(mapdata$order), ] 
      
      output$down_map <- downloadHandler(
        filename = "map.png",
        content = function(file) {
          ggsave(file, 
                 ggplot() + geom_map(data = mapdata2, map = mapdata2, aes(
                   map_id = region,  x=long,y=lat, fill = SO_mean ))  + coord_quickmap() + 
                   theme_void()+ scale_fill_gradientn(name="CO", colours = mycolors, na.value="lightgrey" , breaks = c(0.1, .9), labels = c("Low", "High")),
                                                      width = 5, height = 2.6, units = "in", dpi = 400 
          )
        },
        contentType = 'application/png'
      )


      #################################
      
      # Scaling it 
      df_sample$SO_sum_sc <- scale(df_sample$SO_sum, scale = F, center = T)
      df_sample$LOTR_sum_sc <- scale(df_sample$LOTR_sum, scale = F, center = T)
      
      ################################# 
      ####### Country rename ##########
      #################################
      
      # Recode gender
      df_sample$gender <- ifelse(df_sample$gender %in% c("Female", "Femme", "Femenino", "Feminino", "Mwanamke"), "Female",
                                 ifelse(df_sample$gender %in% c("Male", "Homme", "Masculino", "Masculino", "Mwanamume"), "Male", 
                                        ifelse(df_sample$gender %in% c("Prefer not to say", "Je préfère ne pas le dire", "Prefiero no decirlo", "Prefiro não dizer", "Sipendelei kusema"), "Prefer not to say",
                                               ifelse(df_sample$gender %in% c("Other", "Autre", "Otro", "Outro", "Nyingine"), "Other", "ERROR") ))) 
      
      # Rename column name - gender
      colnames(df_sample)[colnames(df_sample)=="gender"] <- "Gender"
      
      # Rename column name - age
      colnames(df_sample)[colnames(df_sample)=="Age"] <- "Age"
      
      # Recode position
      df_sample$position <- ifelse(df_sample$position %in% c("Ranger", "Garde forestier", "Guardaparques", "Guarda(a)", "Askari wa wanyamapori"), "Ranger",
                                   ifelse(df_sample$position %in% c("Fieldworker", "Intervenant sur le terrain", "Trabajador/a de campo", "Practicante de campo" , "Trabalhador(a) de campo", "Mfanyakazi wa vitendo"), "Fieldworker", 
                                          ifelse(df_sample$position %in% c("Manager", "Responsable", "Gerente o director/a", "Gestor(a)", "Meneja"), "Manager",
                                                 ifelse(df_sample$position %in% c("Administration", "Administration", "Administrador/a", "Administração", "Utawala"), "Administration", 
                                                        ifelse(df_sample$position %in% c("Graduate student", "Étudiant(e) diplômé(e)", "Estudiante de postgrado", "Estudante de Mestrado ou Doutoramento", "Mwanafunzi wa shahada ya uzamili au uzamivu"), "Graduate student", 
                                                               ifelse(df_sample$position %in% c("Bachelors student", "Étudiant(e) en licence", "Estudiante de licenciatura", "Estudante de licenciatura", "Mwanafunzi wa Shahada"), "Bachelors student", 
                                                                      ifelse(df_sample$position %in% c("Researcher", "Chercheur", "Investigador/a", "Investigador(a)", "Mtafiti"), "Researcher", 
                                                                             ifelse(df_sample$position %in% c("Consultant", "Consultant/self-employed", "Consultant(e)/à mon compte", "Consultor/a o trabajador/a independiente", "Consultor(a)/profissional liberal", "Mshauri/kujiajiri"), "Consultant/self-employed", 
                                                                                    ifelse(df_sample$position %in% c("Policymaker", "Décideur politique", "Encargado/a de formular políticas", "Legislador(a)", "Mtunga sera"), "Policymaker", 
                                                                                           ifelse(df_sample$position %in% c("Intern", "Stagiaire", "Becario/a", "Estagiário(a)", "Anayefanya kazi ya muda kupata uzoefu"), "Intern", 
                                                                                                  ifelse(df_sample$position %in% c("Other", "Autre", "Otra", "Outro", "Nyingine"), "Other", "ERROR"
                                                                                                  )))))))))))
      
      # Rename column name - gender
      colnames(df_sample)[colnames(df_sample)=="position"] <- "Position"
      
      
      ################################# 
      ############## End ##############
      #################################
      
      # Create the scatterplot object the plotOutput function is expecting
      output$scatterplot <- renderPlot({
        ggplot(df_sample, aes_string(x = "SO_sum_sc", y = "LOTR_sum_sc",  color = input$x))+ geom_point(size = 2) + 
          scale_x_continuous(breaks = c(min(df_sample$SO_sum_sc), max(df_sample$SO_sum_sc)),
                             labels = c("Low", "High"))+
          scale_y_continuous(breaks = c(min(df_sample$LOTR_sum_sc), max(df_sample$LOTR_sum_sc, .9)),
                             labels = c("Low", "High"))+ theme_classic() +
          theme(axis.line =  element_blank())+  xlab("Conservation optimism") + ylab("General optimism") + 
          geom_hline(yintercept=mean(df_sample$LOTR_sum_sc), color="lightblue") +
          geom_vline(xintercept=mean(df_sample$SO_sum_sc),color="lightblue") +
          
          theme(axis.text=element_text(size=16),
                axis.title=element_text(size=20,face="bold"))+ 
          geom_point(aes(x=tail(df_sample$SO_sum_sc,1), y=tail(df_sample$LOTR_sum_sc,1)),colour="black", size = 3)  + 
          theme(legend.position = "top" , legend.text=element_text(size=12), legend.title =element_blank() ,
                legend.background = element_rect(fill=NA))+
          annotate("text", label = "All-round\nconfident", x = max(df_sample$SO_sum_sc), y = max(df_sample$LOTR_sum_sc), size = 5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2)+
          annotate("text", label = "All-round\ncautious", x = min(df_sample$SO_sum_sc), y = min(df_sample$LOTR_sum_sc), size = 5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2) +
          annotate("text", label = "Confident\nconservationist", x = max(df_sample$SO_sum_sc), y = min(df_sample$LOTR_sum_sc), size = 5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2) +
          annotate("text", label = "Cautious\nconservationist", x = min(df_sample$SO_sum_sc), y = max(df_sample$LOTR_sum_sc), size = 5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2)
      })
      
      output$down <- downloadHandler(
        filename = "optimism.png",
        content = function(file) {
          ggsave(file,   ggplot(df_sample, aes_string(x = "SO_sum_sc", y = "LOTR_sum_sc",  color = input$x))+ geom_point(size = 1) + 
                   scale_x_continuous(breaks = c(min(df_sample$SO_sum_sc), max(df_sample$SO_sum_sc)),
                                      labels = c("Low", "High"))+
                   scale_y_continuous(breaks = c(min(df_sample$LOTR_sum_sc), max(df_sample$LOTR_sum_sc, .9)),
                                      labels = c("Low", "High"))+ theme_classic() +
                   theme(axis.line =  element_blank())+  xlab("Conservation optimism") + ylab("General optimism") + 
                   geom_hline(yintercept=mean(df_sample$LOTR_sum_sc), color="lightblue") +
                   geom_vline(xintercept=mean(df_sample$SO_sum_sc),color="lightblue") +
                   
                   theme(axis.text=element_text(size=12),
                         axis.title=element_text(size=14,face="bold"))+ 
                   geom_point(aes(x=tail(df_sample$SO_sum_sc,1), y=tail(df_sample$LOTR_sum_sc,1)),colour="black", size = 2)  + 
                   theme(legend.position = "top" , legend.text=element_text(size=6), legend.title =element_blank() ,
                         legend.background = element_rect(fill=NA))+
                   annotate("text", label = "All-round\nconfident", x = max(df_sample$SO_sum_sc), y = max(df_sample$LOTR_sum_sc), size = 3.5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2)+
                   annotate("text", label = "All-round\ncautious", x = min(df_sample$SO_sum_sc), y = min(df_sample$LOTR_sum_sc), size = 3.5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2) +
                   annotate("text", label = "Confident\nconservationist", x = max(df_sample$SO_sum_sc), y = min(df_sample$LOTR_sum_sc), size = 3.5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2) +
                   annotate("text", label = "Cautious\nconservationist", x = min(df_sample$SO_sum_sc), y = max(df_sample$LOTR_sum_sc), size = 3.5, colour = "#282e73", vjust = "inward", hjust = "inward", alpha = 0.80, fontface =2),
                
                 width = 5, height = 5, units = "in", dpi = 400
        )},
        contentType = 'application/png'
      )
      
      # Another figure 
      # ERI 
      backeri <- function(variable){
        # As factor 
        variable_out <- ifelse(variable %in% c("Strongly disagree", "Absolument pas d'accord", "Totalmente en desacuerdo", "Discordo inteiramente", "Sikubaliani kabisa"), 1, ifelse(
          variable %in% c("Disagree", "Pas d’accord", "En desacuerdo", "Discordo", "Sikubaliani"), 2, ifelse(
            variable %in% c("Agree", "D'accord", "De acuerdo", "Concordo", "Nakubaliana"), 3, ifelse(
              variable %in% c("Strongly agree", "Tout à fait d'accord", "Totalmente de acuerdo", "Concordo inteiramente", "Nakubaliana kabisa"), 4 , ifelse(
                variable %in% c("Not applicable", "Ne s'applique pas", "No aplica", "Não se aplica", "Hinihusu"), NA, NA
              )))))
        return(variable_out)
      }
      
      backeri_neg <- function(variable){
        # As factor 
        variable_out <- ifelse(variable %in% c("Strongly agree", "Tout à fait d'accord", "Totalmente de acuerdo", "Concordo inteiramente", "Nakubaliana kabisa"), 1, ifelse(
          variable %in% c("Agree", "D'accord", "De acuerdo", "Concordo", "Nakubaliana"), 2, ifelse(
            variable %in% c("Disagree", "Pas d’accord", "En desacuerdo", "Discordo", "Sikubaliani"),3 , ifelse(
              variable %in% c("Strongly disagree", "Absolument pas d'accord", "Totalmente en desacuerdo", "Discordo inteiramente", "Sikubaliani kabisa"), 4, ifelse(
                variable %in% c("Not applicable", "Ne s'applique pas", "No aplica", "Não se aplica", "Hinihusu"), NA, NA
              )))))
        return(variable_out)
      }
      
      # Back coding 
      df_sample[, c("ERI_1", "ERI_2", "ERI_3", "ERI_4", "ERI_8", "ERI_9", "ERI_10", "ADD1", "ADD2", "ADD3", "ADD4", "ADD5")]<- lapply(df_sample[, c("ERI_1", "ERI_2", "ERI_3", "ERI_4", "ERI_8", "ERI_9", "ERI_10", "ADD1", "ADD2", "ADD3", "ADD4", "ADD5")], function(x){ backeri(x) })
      df_sample[, c("ERI_5","ERI_6","ERI_7")]<- lapply(df_sample[, c("ERI_5","ERI_6","ERI_7")], function(x){ backeri_neg(x) })
      
      # ERI 
      df_sample$ERI_effort <- rowSums(df_sample[, c("ERI_1", "ERI_2", "ERI_3", "ADD1", "ADD2", "ADD3")], na.rm = T)
      df_sample$ERI_reward <- rowSums(df_sample[, c("ERI_4", "ERI_5", "ERI_6", "ERI_7", "ERI_8", "ERI_9", "ERI_10", "ADD4", "ADD5")], na.rm = T)
      
      # correction
      corection1 <- apply(df_sample[, c("ERI_1", "ERI_2", "ERI_3", "ADD1", "ADD2", "ADD3")], MARGIN = 1, FUN = function(x) length(x[!is.na(x)]) )
      corection2 <- apply(df_sample[, c("ERI_4", "ERI_5", "ERI_6", "ERI_7", "ERI_8", "ERI_9", "ERI_10", "ADD4", "ADD5")], MARGIN = 1, FUN = function(x) length(x[!is.na(x)]) )
      
      # correction factor 
      corr <- corection1/corection2
      
      # Apply the correction 
      df_sample$ERI_reward <- df_sample$ERI_reward*corr
      
      # Correction 
      df_sample$ERI_effort <- jitter(df_sample$ERI_effort,6)
      df_sample$ERI_reward <- jitter(df_sample$ERI_reward,6)
      
      # Effort reward imbalance 
      data_p <- data.frame(a = c(5,5,25), b = c(5,25,25), Gender = NA, Position = NA, Age = NA )
      
      # Plot
      output$scatterplot_ERI <- renderPlot({
        ggplot(df_sample, aes_string(x = "ERI_reward", y = "ERI_effort",  color = input$x2))+ geom_point(size = 2) + 
          scale_x_continuous(limits = c(5,25), breaks = c(8, 22),
                             labels = c("Low", "High"))+
          scale_y_continuous(limits = c(5, 25), breaks = c(8, 22),labels = c("Low", "High"))+
          theme_classic() +
          theme(axis.line =  element_blank())+  xlab("Reward") + ylab("Challenge/effort") + 
          #geom_abline(intercept = 0 , slope = 1, color="lightblue") +
          theme(axis.text=element_text(size=16),
                axis.title=element_text(size=20,face="bold"))+
          geom_point(aes(x=tail(df_sample$ERI_reward,1), y=tail(df_sample$ERI_effort,1)),colour="black", size = 3)  + 
          theme(legend.position = "top" , legend.text=element_text(size=12), legend.title =element_blank() ,
                legend.background = element_rect(fill=NA)) +
          geom_polygon(data=data_p, aes_string(x="a", y="b"), fill = "red", colour= NA, alpha = 0.05)
      })
      
      output$down_eri <- downloadHandler(
        filename = "ERI.png",
        content = function(file) {
          ggsave(file, ggplot(df_sample, aes_string(x = "ERI_reward", y = "ERI_effort",  color = input$x2))+ geom_point(size = 1) + 
                   scale_x_continuous(limits = c(5,25), breaks = c(8, 22),
                                      labels = c("Low", "High"))+
                   scale_y_continuous(limits = c(5, 25), breaks = c(8, 22),labels = c("Low", "High"))+
                   theme_classic() +
                   theme(axis.line =  element_blank())+  xlab("Reward") + ylab("Challenge/effort") + 
                   #geom_abline(intercept = 0 , slope = 1, color="lightblue") +
                   theme(axis.text=element_text(size=12),
                         axis.title=element_text(size=14,face="bold"))+
                   geom_point(aes(x=tail(df_sample$ERI_reward,1), y=tail(df_sample$ERI_effort,1)),colour="black", size = 2)  + 
                   theme(legend.position = "top" , legend.text=element_text(size=6), legend.title =element_blank() ,
                         legend.background = element_rect(fill=NA)) +
                   geom_polygon(data=data_p, aes_string(x="a", y="b"), fill = "red", colour= NA, alpha = 0.05), 
          width = 5, height = 5, units = "in", dpi = 400
          )},
        contentType = 'application/png'
      )
      
      myvars <- c("continent", "count")
      summary_table <- merged_df_3_DF[myvars]
      
      names(summary_table)[names(summary_table) == "continent"] <- "Continent"
      names(summary_table)[names(summary_table) == "count"] <- "Number of responses"
      output$response_table <- renderTable(summary_table)
      

      ##########################################################################################################

      shinyjs::reset("form") # Then reset the form
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg") # Show the thank you message
    },
    
    # If savng doesn't work, show the following error
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      shinyjs::show("submit")
      shinyjs::enable("submit")
      
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # Submitting anoymous comment 
  observeEvent(input$submit_anonymous, {
    shinyjs::show("submit_msg_anonymous") # The submit_msg is shown

    # Add a try catch 
    tryCatch({
      df_sample <- anonymous_save_Data(formData3()) #### create anonymous_save_Data
      })
  })
  
  
  ################################################################################################
  ### Thank you for submittng another ###
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  

  # Image frog
  output$frog <- renderUI({
    tags$img(src = "frog_c.jpg")
  })
  
  # Image fish
  output$fish <- renderUI({
    tags$img(src = "fish_c.jpeg")
  })
  
  # Image monkey
  output$monkey <- renderUI({
    tags$img(src = "monkey_o.jpg")
  })
  
  # Image divers
  output$dive <- renderUI({
    tags$img(src = "dive_c.jpg")
  })
  
  # Image lion
  output$lion <- renderUI({
    tags$img(src = "DSC_0553.JPG")
  })
  # Image turt
  output$turt <- renderUI({
    tags$img(src = "G1560461.jpg")
  })
  
}

### Run it ###
shinyApp(server = server, ui = ui)

