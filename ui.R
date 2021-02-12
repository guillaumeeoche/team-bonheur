source(file = "dependencies.R")

ui <- dashboardPage(
  skin="yellow",
  
  ## HEADER -----------------------------------------------------------
  dashboardHeader(title = span(img(src="img/smile.png", width = 60)) ),
  
  ## SIDEBAR ----------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Nettoyage du jeu de données", tabName = "clean", icon = icon("hand-sparkles")),
      menuItem("Filtres", tabName = "filtre", icon = icon("filter")),
      menuItem("Analyses statistiques", tabName = "stat", icon = icon("chart-pie")),
      menuItem("Analyses textuelles", tabName = "text", icon = icon("spell-check")),
      menuItem("Rapport", tabName = "report", icon = icon("file-word"))
    )
  ),
  
  
  
  ## BODY --------------------------------------------------------------
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    ")),
    
    
    ### MAIN - BODY ----------------------------------------------------
    
    tabItems(
      tabItem(tabName = "clean",
              
              img(id = "logo", src="img/happiness.png", width = 140),
              
              br(),
              
              h2(strong("Etude sur le bonheur")),
              p("Nous disposons des résultats d'une enquête socio-économique sur le bonheur. 435 personnes ont bien voulu participer à ce sondage et ont répondu à 23 questions.
                Nous avons récupéré les résultats de l'enquête au format xlsx et avons nettoyé le jeu de données. Celui-ci dispose de 17 question fermées et 6 questions ouvertes."),
              br(),
              
              h4(strong("Choisissez la partie du jeu de données nettoyé que vous souhaitez afficher : ")),
              
              br(),
              
              fluidRow(
                column(
                  width = 12,
                  bsButton("closed_questions_button", 
                           label = "QUESTIONS FERMÉES", 
                           icon = icon("keyboard"), 
                           style = "success"),
                  bsButton("opens_questions_button", 
                           label = "QUESTIONS OUVERTES", 
                           icon = icon("question-circle", class = "spinner-box"), 
                           style = "success")
                )
              ),
              
              br(),
              
              dataTableOutput("clean_data"),
              
              tags$u(h5("Les questions fermées seront utilisées pour l'analyse descriptive et les questions ouvertes pour l'analyse textuelle.")),
              
              br(),
              
              downloadButton("dl", "Télécharger le jeu de données nettoyé en entier")
      ),
      
      
      tabItem(tabName = "filtre",
              h2(strong("Filtrez vos analyses")),
              br(),
              p("Vous allez pouvoir filtrer les résultats d'analyse. Vous avez ci-dessous différents filtres que vous pouvez appliquer (vous pouvez en sélectionner plusieurs)."),
              p("Si vous n'en sélectionnez pas, le jeu de données sera pris en compte dans sa totalité."),
              h4(strong("Appliquez vos filtres")),
              tags$br(),
              fluidRow(
                column(3, 
                       awesomeRadio("sexe_input", strong("Choix du sexe"),
                                    choices = list("Les deux" = 2, 
                                                   "Homme" = 1, 
                                                   "Femme" = 0),
                                    selected = 2)),
                
                column(3, 
                       awesomeCheckboxGroup("age_input", strong("Choix des tranches d'âges"),
                                            choices = sort(unique(happiness_survey_out_oq$tranche_age)),
                       )),
                
                column(2, 
                       awesomeCheckboxGroup("region_input", strong("Choix des régions d'étude"),
                                            choices = sort(unique(happiness_survey_out_oq$region[happiness_survey$region != "NA"]))
                       )),
                
                column(3,
                       
                       strong("Souhaitez-vous que l'on vous aide à trouver des problématiques auxquelles vous pouvez répondre ?"),
                       br(),
                       br(),
                       prettyToggle(
                         inputId = "oui_ou_non_questions",
                         label_on = "Oui !", 
                         icon_on = icon("check"),
                         status_on = "info",
                         status_off = "warning", 
                         label_off = "Non, merci.",
                         icon_off = icon("remove")
                       ),
                       
                       uiOutput("select_question")
                )
              ),
              
              br(), 
              
              h4(strong("Vos individus ont le profil suivant :")),
              textOutput("sentence_recap"),
              textOutput("questions_selected"),
              
              br(), 
              br(),
              
              dataTableOutput("filter_data"),
              
      ),
      
      
      tabItem(tabName = "stat",
              h2(strong("Statistiques descriptives")),
              
              mainPanel(width = 20,
                        tabsetPanel(
                          tabPanel("Vos analyses",
                                   fluidRow(
                                     column(width =5 ,
                                            plotOutput("pie")
                                     ),
                                     
                                     column(width =5 ,
                                            plotOutput("hist")
                                     )
                                   ), 
                                   
                                   plotOutput("map"),
                                   
                                   h2("Vous pouvez revenir sur l'onglet Filtres à tout moment."),
                                   
                                   actionButton('switchtab2', 'Revenir au menu Filtres', icon("undo-alt"), 
                                                style="color: #fff; background-color: peru; border-color: burlyWood")
                          ), 
                          
                          tabPanel("Les problématiques proposées",
                                   uiOutput("title_if_not_questions_selected"),
                                   uiOutput("if_not_questions_selected")
                          )
                        ))
      ),
      
      
      tabItem(tabName = "text",
              h2(strong("Analyse des questions ouvertes")),
              tags$br(),
              p("Vous pouvez voir par question, les modalités qui ont été les plus citées par les répondants. Sur les nuages de mots, la taille est proportionnelle au nombre de fois où chaque mot a été cité."),
              br(),
              fluidPage(navlistPanel(
                tabPanel("- Quel était votre rêve d'enfant ?", wordcloud2Output("word_cloud_2"), 
                         textOutput("text_enfant")),
                tabPanel("- Si vous fermez les yeux et imaginez un endroit où vous vous sentez bien, comment le décririez-vous ?", plotOutput("bar_2")),
                tabPanel("- Que faites vous qui vous rende heureux pendant votre temps personnel ?",plotlyOutput("bar_3")),
                tabPanel("- Qu'est ce qui vous rend heureux au quotidien ?",wordcloud2Output("word_cloud")),
                tabPanel("- Quel est votre but dans la vie ?", plotlyOutput("bar")),
                tabPanel("- Qu'est-ce qu'être heureux selon vous ?", plotOutput("tree"), 
                         h5("Pour la majorité des gens, etre heureux consite a etre en vie"))
                
              ))),
      
      
      tabItem(tabName = "report",
              h2(strong("Télécharger les analyses")),
              tags$br(),
              p("Vous pouvez ici décider d'exporter les analyses sous la forme d'un rapport pdf. Vous avez la possibilité d'exporter l'intégralité des résultats ou de sélectionner les questions qui vous intéressent."),
              radioButtons("export", label="Choix d'export", choices=c("Exporter toutes les analyses","Sélectionner mes analyses"), selected = character(0), inline=T),
              br(),
              conditionalPanel(condition="input.export == 'Sélectionner mes analyses'",
                               checkboxGroupInput("check_analysis", "Cochez les questions à ajouter au rapport :",
                                                  choices=colnames(happiness_survey))),
              
              fluidPage(
                downloadButton("downloadData","Télécharger"))
      )
    )
  )
)