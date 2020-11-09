
list.of.packages <- c("shiny", "shinydashboard", "dplyr", "leaflet", "DT", "readxl", "writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only = TRUE)


###UI###

ui <- dashboardPage(
  skin="yellow",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Nettoyage du jeu de données", tabName = "clean", icon = icon("hand-sparkles"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    ")),
    
    tabItems(
      tabItem(tabName = "clean",
              h1(strong("Nettoyage du jeu de données")),
              img(id = "logo", src="img/logo.png", width = 140),
              h3("Importez votre dataset et laissez la magie opérer ! "),
              
              p("Vous pouvez importer votre jeu de données au format Excel ou CSV."),
              p("Vous pourrez ensuite le retélécharger au format que vous souhaitez."),
              tags$br(),
              fileInput("file", "Importer un fichier"),
              tableOutput("files"),
              dataTableOutput("clean_data"),
              downloadButton("dl", "Download")
              
      
              
      )
      
      
      
    )))



###SERVER###

server <- function(input, output) {
  
  output$files <- renderTable(input$file)
  
  dataset <- reactive({
    infile <- input$file
    if(is.null(infile))
      return(NULL)
    if(!is.null(infile)){
    data = read_excel(infile$datapath)}
    age = grep("âge|age", colnames(data), ignore.case = T)
    colnames(data)[which(names(data) == colnames(data[,grep("âge|age", colnames(data), ignore.case = T)]))] <- "Age"
    for (i in 1:length(data$Age)) {
      #replace par NULL les age <2 et >115

      if (data$Age[i] %in% c(2:115)==FALSE){
        data$Age[i]<- NA}
    }

    # Verification variable department ----------------------------


    i = grep("département|departement", colnames(data), ignore.case = T)

    colnames(data)[which(names(data) == colnames(data[,i]))] <- "Departement"


    data$Departement <- gsub("\\..*","",data$Departement)
    data$Departement = as.integer(data$Departement)


    for (i in 1:length(data$Departement)) {

      #transform code postal en department

      if (nchar(data$Departement[i])==5){
        data$Departement[i] = substr(data$Departement[i],0,2)}

      #replace par NULL les departments non valides

      if (data$Departement[i] %in% c(1:95,971:976)==FALSE){
        data$Departement[i]<- NA}
      
      # if (nchar(data$Departement[i])==1){
      #   data$Departement[i] = paste(0,data$Departement[i])
      # }
      }

    # Recodage variable à choix multiple -------------------------
    ## 1 colonne pour chaque proposition de réponse, codage booléen

    n=grep("Cochez", colnames(data))

    ## Votre famille
    for (i in 1: nrow(data)){
      if (grepl("Votre famille", data[i,n])){
        data$Famille[i] <- 1
      }else{
        data$Famille[i] <- 0
      }}

    ## Vos amis
    for (i in 1: nrow(data)){
      if (grepl("Vos amis", data[i,n])){
        data$Amis[i] <- 1
      }else{
        data$Amis[i] <- 0
      }}

    ##  Vos relations amoureuses
    for (i in 1: nrow(data)){
      if (grepl("Vos relations amoureuses", data[i,n])){
        data$Relations_amoureuses[i] <- 1
      }else{
        data$Relations_amoureuses[i] <- 0
      }}

    ## Vos relations sociales de manière générale
    for (i in 1: nrow(data)){
      if (grepl("Vos relations sociales de manière générale", data[i,n])){
        data$Relations_sociales[i] <- 1
      }else{
        data$Relations_sociales[i] <- 0
      }}


    # Détecter les questions ouvertes -------------------------------------

    sexe = grep("sexe", colnames(data), ignore.case=T )
    colnames(data)[which(names(data) == colnames(data[,sexe]))] <- "Sexe"

    ouverte = grep("qu|comment", colnames(data), ignore.case = T)
    ouverte = subset(ouverte, !ouverte==sexe)

    # Supprimer les réponses de moins de 3 caractères + réponses inutiles

    for (i in ouverte[1]:ouverte[length(ouverte)]){
      for (j in 1:nrow(data)){
        if(nchar(data[j,i]) <= 3){
          data[j,i] <- NA
        }
        if(grepl("aucun|no se|un j|burger king|ne sais pas", data[j,i], ignore.case = T)){
          data[j,i] <- NA
        }
      }}

    
    data
    })

  
  
output$clean_data <- renderDataTable({ dataset() })
  

 output$dl <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(dataset(), con)
     }

 )
}


###APP###

shinyApp(ui, server)

