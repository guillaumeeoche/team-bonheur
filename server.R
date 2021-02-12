source(file = "dependencies.R")


server <- function(input, output, session) {
  
  ## REACTIVE DATASETS --------------------------------
  
  closed_questions <- reactive({
    
    for (i in 1:length(happiness_survey_out_oq$age)) {
      
      if(happiness_survey_out_oq$age[i] %in% c(2:115)==FALSE){
        happiness_survey_out_oq$age[i]<- NA}
    }
    
    # Verification variable department ----------------------------
    
    happiness_survey_out_oq$departement <- as.integer(gsub("\\..*","", happiness_survey_out_oq$departement))
    
    for (i in 1:length(happiness_survey_out_oq$departement)) {
      
      #transform code postal en department
      
      if (nchar(happiness_survey_out_oq $departement[i])==5){
        happiness_survey_out_oq$departement[i] = substr(happiness_survey_out_oq $departement[i],0,2)}
      
      #replace par NULL les departments non valides
      
      if (happiness_survey_out_oq $departement[i] %in% c(1:95,971:976)==FALSE){
        happiness_survey_out_oq$departement[i]<- NA}
      
    }
    
    # Recodage variable à choix multiple -------------------------------
    ## 1 colonne pour chaque proposition de réponse, codage booléen
    
    ## Votre famille
    for (i in 1:nrow(happiness_survey_out_oq)){
      if(grepl("Votre famille", happiness_survey_out_oq[i, 5])){
        happiness_survey_out_oq$familles[i] <- 1
      }else{
        happiness_survey_out_oq$familles[i] <- 0
      }
    }
    
    # Vos amis
    for (i in 1: nrow(happiness_survey_out_oq)){
      if (grepl("Vos amis", happiness_survey_out_oq[i,5])){
        happiness_survey_out_oq$amis[i] <- 1
      }else{
        happiness_survey_out_oq$amis[i] <- 0
      }
    }
    
    ##  Vos relations amoureuses
    for (i in 1: nrow(happiness_survey_out_oq)){
      if (grepl("Vos relations amoureuses", happiness_survey_out_oq[i,5])){
        happiness_survey_out_oq$relations_amoureuses[i] <- 1
      }else{
        happiness_survey_out_oq$relations_amoureuses[i] <- 0
      }
    }
    
    ## Vos relations sociales de manière générale
    for (i in 1: nrow(happiness_survey_out_oq)){
      if (grepl("Vos relations sociales de manière générale", happiness_survey_out_oq[i,5])){
        happiness_survey_out_oq$relations_sociales[i] <- 1
      }else{
        happiness_survey_out_oq$relations_sociales[i] <- 0
      }
    }
    
    happiness_survey_out_oq <- happiness_survey_out_oq[, -5]
    
    ## Codage sexe binaire
    
    happiness_survey_out_oq$sexe <- ifelse(happiness_survey_out_oq$sexe == "Homme", 1, 0)
    
    happiness_survey_out_oq 
  })
  
  opens_questions <- reactive({
    #Supprimer les réponses de moins de 3 caractères 
    
    for(j in 1:ncol(happiness_survey_oq)){
      for(i in 1:nrow(happiness_survey_oq)){
        if(nchar(happiness_survey_oq[i, j]) <= 3){
          happiness_survey_oq[i, j] <- NA
        }
        
        if(grepl("aucun|no se|un j|burger king|ne sais pas", happiness_survey_oq[i, j], ignore.case = T)){
          happiness_survey_oq[i, j] <- NA
        }
      }
    }
    
    happiness_survey_oq
  })
  
  ###TAB ITEM - NETTOYAGE DE DONNÉES  ------------------------------------------------------------
  
  observeEvent(input$closed_questions_button, {
    output$clean_data <- renderDataTable(closed_questions(), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$opens_questions_button, {
    output$clean_data <- renderDataTable(opens_questions(), options = list(scrollX = TRUE))
  })
  
  opens_q <- reactive({
    cbind(closed_questions(),opens_questions())
  })
  
  ##dowload button
  
  output$dl <- downloadHandler(
    filename = function() {
      paste('happiness_survey-', Sys.Date(), '.csv', sep='')
    },
    
    content = function(con) {
      write.csv(cbind(closed_questions(), opens_questions()), con)
    })
  
  ###TAB ITEM - FILTRES ------------------------------------------------------------
  
  filter_all <- reactive({
    if((is.null(input$age_input) == TRUE) & (is.null(input$region_input) == TRUE)){
      if(input$sexe_input == 2){
        print("2, TRUE, TRUE")
        if (input$closed_questions_button==TRUE){
          closed_questions()
        }else if (input$opens_questions_button==TRUE){
          opens_q()
        }
      }else{
        print("others, TRUE, TRUE")
        if (input$closed_questions_button==TRUE){
          closed_questions()%>% filter(sexe == input$sexe_input)
        }else if (input$opens_questions_button==TRUE){
          opens_q()%>% filter(sexe == input$sexe_input)
        }
      }
    }else{
      if((is.null(input$age_input) == FALSE) & (is.null(input$region_input) == TRUE)){
        if(input$sexe_input == 2){
          print("2, FALSE, TRUE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(tranche_age %in% input$age_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(tranche_age %in% input$age_input)
          }
        }else{
          print("others, FALSE, TRUE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(sexe == input$sexe_input, tranche_age %in% input$age_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(sexe == input$sexe_input, tranche_age %in% input$age_input)
          }
        }
      }else if((is.null(input$age_input) == TRUE) & (is.null(input$region_input) == FALSE)){
        if(input$sexe_input == 2){
          print("2, TRUE, FALSE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(region %in% input$region_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(region %in% input$region_input)
          }
        }else{
          print("others, TRUE, FALSE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(sexe == input$sexe_input, region %in% input$region_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(sexe == input$sexe_input, region %in% input$region_input)
          }
        }
      }else{
        if(input$sexe_input == 2){
          print("2, FALSE, FALSE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(tranche_age %in% input$age_input, region %in% input$region_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(tranche_age %in% input$age_input, region %in% input$region_input)
          }
        }else{
          print("others, FALSE, FALSE")
          if (input$closed_questions_button==TRUE){
            closed_questions()%>% filter(sexe == input$sexe_input, tranche_age %in% input$age_input, region %in% input$region_input)
          }else if (input$opens_questions_button==TRUE){
            opens_q()%>% filter(sexe == input$sexe_input, tranche_age %in% input$age_input, region %in% input$region_input)
          }
        }
      }
    }
  })
  
  
  output$filter_data <- renderDataTable(filter_all(), options = list(scrollX = TRUE))
  
  output$select_question <- renderUI(
    if(input$oui_ou_non_questions == "TRUE")
      pickerInput(
        inputId = "question_input",
        label = "Sélectionnez la ou les questions auxquelles vous souhaitez répondre.", 
        choices = questions,
        multiple = TRUE, 
        selected = 1, 
        options = pickerOptions(noneSelectedText = "Aucune question sélectionnée.")
      )
  )
  
  output$sentence_recap <- renderText(
    paste("Des", ifelse(input$sexe_input == 2, "Hommes et des Femmes", ifelse(input$sexe_input == 1, "Hommes", "Femmes")), 
          " âgé.e.s de ", ifelse(is.null(input$age_input) == TRUE, "Toutes les tranches d'âges", paste(c(input$age_input), collapse = " , ")), 
          " qui habitent dans la/les régions suivantes : ", ifelse(is.null(input$region_input) == TRUE, "Toutes les régions", paste(c(input$region_input), collapse = " , ")))
  )
  
  output$questions_selected <- renderText(
    paste(ifelse(input$oui_ou_non_questions == "FALSE", "Vous ne souhaitez pas voir les problématiques.", paste("Vous avez sélectionné les problématique suivantes :", paste(c(input$question_input), collapse = " - "))))
    
  )
  
  
  ###TAB ITEM - ANALYSES STATS ------------------------------------------------------------
  
  output$title_if_not_questions_selected <- renderUI(
    if(input$oui_ou_non_questions == "FALSE"){
      h2("Vous n'avez pas sélectionné de problématiques, vous pouvez revenir dans l'onglet filtres si vous le souhaitez.")
    }
  )
  
  output$if_not_questions_selected <- renderUI(
    if(input$oui_ou_non_questions == "FALSE"){
      actionButton('switchtab1', 'Revenir au menu Filtres', icon("undo-alt"), 
                   style="color: #fff; background-color: peru; border-color: burlyWood")
    }
  )
  
  observeEvent(input$switchtab1, {
    print(input$tabs)
    newtab <- switch(input$tabs,
                     "stat" = "filtre"
    )
    print(newtab)
    updateTabItems(session = session, "tabs", newtab)
  })
  
  
  observeEvent(input$switchtab2, {
    print(input$tabs)
    newtab <- switch(input$tabs,
                     "stat" = "filtre"
    )
    print(newtab)
    updateTabItems(session = session, "tabs", newtab)
  })
  
  
  
  
  #### camembert
  
  
  output$pie <- renderPlot({
    ggplot(as.data.frame(t(t(table(filter_all()$sexe)))), aes(x="", y=Freq, fill=Var1)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + 
      labs(fill = "sexe",y="",x="") +
      theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank(),plot.title = element_text( face="bold", size=16,hjust=0.5)) +
      geom_text(aes(y = Freq/3 + c(0, cumsum(Freq)[-length(Freq)]),label = paste(round(Freq/length(filter_all()$sexe)*100,2)," %")),size=7) +
      ggtitle("Proportion de répondant selon leur sexes")
    
  })
  
  #### histogramme
  
  output$hist<-renderPlot({
    ggplot(filter_all(), aes(x = `age`)) +
      geom_histogram(aes(y=..density..),fill="green") +
      labs(y="frequence",x="age",title="Histogramme des ages")+
      theme(plot.title = element_text( face="bold", size=16,hjust=0.5)) 
    
  })
  
  #### Map
  
  output$map <- renderPlot({ 
    france=read.csv("data/departements-france.csv",encoding="UTF-8")
    data_maps<-merge(france,filter_all(),by.x="code_departement",by.y="departement")[,-2:-4]
    colnames(data_maps)[1]<-"departement"
    code_departement=as.integer(unique(data_maps$departement))
    
    
    somme_departement=0
    for (j in 1:length(code_departement)){
      for (i in 1:nrow(data_maps)){
        somme_departement[j]=sum(data_maps$departement == code_departement[j])
      }
    }
    
    happiness_surveydpt=data.frame(code_departement,somme_departement)
    happiness_surveydpt=merge(happiness_surveydpt,france,by="code_departement")
    
    France <- gadm_sf_loadCountries("FRA", level=2 )
    
    choropleth(France, 
               data = happiness_surveydpt,
               value = "somme_departement", 
               adm.join = "nom_departement",
               palette = "Set3",
               legend="Nombre de participations au questionaire",
               title="Participation au questionnaire")
    
  })
  
  
  # TAB ITEM - ANALYSE TEXTUELLE ------------------------------------------------------------
  
  # Question 1 : Quel était votre rêve d'enfant ?
  
  output$word_cloud_2 = renderWordcloud2({
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=reve_enfant,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    docs_non=data.frame(docs_non)
    
    suppr <- data.frame(lemme=c(NA,"enfant"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    wordcloud2(docs[c(1:60),], color="random-light")
  })
  

  # Question 2 : Si vous fermez les yeux et imaginez un endroit où vous vous sentez bien, comment le décririez-vous ?
  
  output$bar_2 <- renderPlot({ 
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=endroit_reve,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    docs_non=data.frame(docs_non)
    
    suppr <- data.frame(lemme=c("calmer","endroit","bruire"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    #Create dataset
    d_1 <- data.frame(
      id=seq(1,30),
      individual=docs[1:30,]$lemme,
      value=as.integer(docs[1:30,]$n)
    )
    
    # ----- This section prepare a dataframe for labels ---- #
    # Get the name and the y position of each label
    label_data <- d_1
    
    # calculate the ANGLE of the labels
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    
    # flip angle BY to make them readable
    label_data$angle<-ifelse(angle < -90, angle+180, angle)
    
    # ----- ------------------------------------------- ---- #
    # Start the plot
    p <- ggplot(label_data, aes(x=id, y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      # This add the bars with a blue color
      geom_bar(stat="identity", fill=alpha("peru", 0.7)) +
      
      #   # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
      ylim(-100,120) +
      #   
      #   # Custom the theme: no axis title and no cartesian grid
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
      ) +
      #   
      #   # This makes the coordinate polar instead of cartesian.
      coord_polar(start=0) +
      #   
      #   # Add the labels, using the label_data dataframe that we have created before
      geom_text(data=label_data, aes(x=id, y=value+5, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.8, size=3.5, angle=label_data$angle,inherit.aes = FALSE ) 
    p 
    
  })
  
  
  # Question 3 : Que faites vous qui vous rende heureux pendant votre temps personnel ?
  output$bar_3 <- renderPlotly({ 
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=heureux_temps_perso,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    docs_non=data.frame(docs_non)
    
    suppr <- data.frame(lemme=c("temps","musiquer"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    p3 <- plot_ly() %>%
      add_trace(x=docs[1:15,]$n,
                y=docs[1:15,]$lemme,
                type='bar',
                marker=list(color="peru",
                            line=list(color="burlyWood", width=2))) %>%
      layout(title = "Fréquence des 15 mots les plus cités",
             xaxis=list(title="Fréquence",
                        zeroline=F),
             yaxis=list(title="",
                        zeroline=F))
    p3
    
  })
  
  #Question 4 - Qu'est ce qui vous rend heureux au quotidien ? 
  
  output$word_cloud = renderWordcloud2({
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=heureux_quotidien,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    docs_non=data.frame(docs_non)
    
    suppr <- data.frame(lemme=c(NA,"choses"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    wordcloud2(docs[1:60,], color="random-light")
  })
  
  # Question 5 : Quel est votre but dans la vie ? 
  output$bar <- renderPlotly({ 
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=but_vie,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    docs_non=data.frame(docs_non)
    
    suppr <- data.frame(lemme=c("vie",NA,"vivre"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    p2 <- plot_ly() %>%
      add_trace(x=docs[1:10,]$n,
                y=docs[1:10,]$lemme,
                type='bar',
                marker=list(color="peru",
                            line=list(color="burlyWood", width=2))) %>%
      layout(title = "Fréquence des 10 mots les plus cités",
             xaxis=list(title="Fréquence",
                        zeroline=F),
             yaxis=list(title="",
                        zeroline=F))
    p2
    
    
  })
  
  # Question 5 : Qu'est ce qu'etre heureux selon vous ? 
  output$tree <- renderPlot({ 
    
    docs_1 <- filter_all()
    docs=docs_1 %>% 
      tidytext::unnest_tokens(output="bigramme",
                              input=etre_heureux_selon_vous,
                              token="ngrams",
                              n=1)
    
    docs_non <- docs %>%
      anti_join(proustr::proust_stopwords(),by=c("bigramme"="word"))
    
    source('https://raw.githubusercontent.com/chrplr/openlexicon/master/datasets-info/fetch_datasets.R')
    lexique382 <- get_lexique383_rds()
    
    docs_non <- dplyr::left_join(docs_non,
                                 lexique382,
                                 by=c("bigramme"="ortho"))
    docs_non=docs_non %>% 
      dplyr::select(bigramme,lemme)
    
    docs_non=docs_non  %>%
      dplyr::group_by(lemme) %>% 
      dplyr::summarise(n=n()) %>% 
      dplyr::arrange(desc(n))
    
    # suppression mot
    suppr <- data.frame(lemme=c("heureux"))
    suppr$temp <-1
    docs <- merge(docs_non, suppr, by=c("lemme"), all.x=TRUE)
    docs <- docs[is.na(docs$temp), ]
    docs <- docs[order(docs$n, decreasing = TRUE),]
    
    # Create data
    group <- docs[1:20,]$lemme
    value <- docs[1:20,]$n
    data <- data.frame(group,value)
    
    # treemap
    treemap(data,
            index="group",
            vSize="value",
            type="index",
            palette="OrRd"
    )
  })
  
  # TAB ITEM - DOWNLOAD ------------------------------------------------------------
  
  
  
}



