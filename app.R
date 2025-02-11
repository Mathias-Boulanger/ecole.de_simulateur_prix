library(shiny)

ecolage <- list(
  "Élementaire - CP à CM" = 301,
  "Maternelle - de 3 à 6 ans" = 257,
  "Toute Petite Section - dès 2 ans" = 455
)

garderie <- list(
  'Élementaire - CP à CM' = data.frame(
    norm = c(0, 61, 158, 252, 340), vac = c(0, 115, 226, 333, 432),
    reduc = c(0, 61, 158, 230, 296), reduc_vac = c(0, 115, 226, 303, 376),
    row.names = c("13h10", "14h10 - mini", "15h45 - midi", "17h00 - midi17", "18h00 - maxi")),
  'Maternelle - de 3 à 6 ans' = data.frame(
    norm = c(0, 50, 129, 194, 249), vac = c(0, 89, 178, 252, 314),
    reduc = c(0, 50, 129, 194, 249), reduc_vac = c(0, 115, 226, 303, 376),
    row.names = c("13h10", "14h10 - mini", "15h45 - midi", "17h00 - midi17", "18h00 - maxi")),
  'Toute Petite Section - dès 2 ans' = data.frame(
    norm = c(0, 88, 153, 208), vac = c(66, 167, 240, 302),
    reduc = c(0, 88, 153, 208), reduc_vac = c(66, 167, 240, 302),
    row.names = c("14h10 - mini", "15h45 - midi", "17h00 - midi17", "18h00 - maxi"))
)

reduction.l <- c(no=1, `1`=0.85, `2`=0.75)
limitFamilial=round(((ecolage[["Élementaire - CP à CM"]]/5)*100)*12, 0)

ui <- fluidPage(
  titlePanel("Calcul des frais scolaires par famille 2024-2025"),
  
  # Résumé de l'application
  fluidRow(
    column(12, p(paste0("Ce simulateur permet de calculer les frais scolaires par enfant en fonction de sa classe, 
    des horaires choisis et des options de vacances. Les prix calculés prennent également en compte les 
    réductions appliquées pour les fratries ainsi que celles pour les revenus familiaux inférieurs à ", limitFamilial,"€."))),
    column(12, p("Si plusieurs enfants, merci de les entrer par ordre de classe, le 1er enfant étant dans la classe la plus avancée."))
  ),
  
  # Section pour les entrées des enfants
  fluidRow(
    column(12, h4("Informations de la famille"), uiOutput("child_inputs"))
  ),
  
  fluidRow(
    column(12, actionButton("add_child", "Ajouter un enfant"))
  ),
  
  # Case à cocher pour le critère de revenu familial
  fluidRow(
    column(4, checkboxInput("low_income", paste0("Revenu familial < ", limitFamilial,"€**"), FALSE)),
    column(4, uiOutput("income_input"))
  ),
  
  # Tableau récapitulatif
  fluidRow(
    column(12, h4("Récapitulatif des frais"), tableOutput("summary_table")),
    column(12, p("* Total à titre indicatif. Validation lors de l'inscription.", style = "font-size: small; color: gray;")),
    column(12, p("** Réduction basées sur le revenu des familles sur tarifs élémentaire uniquement. Sur demande et sur justificatifs.", style = "font-size: small; color: gray;"))
  ),
  
  fluidRow(
    column(12, actionButton("reset", "Réinitialiser"))
  )
  
)

server <- function(input, output, session) {
  # Stocker les données pour chaque enfant
  child_data <- reactiveVal(data.frame(
    Nom = character(), Classe = character(), Horaire = character(),
    Vacances = character(), Écolage = numeric(), Garderie = numeric(),
    Total = numeric(), stringsAsFactors = FALSE
  ))
  
  # Compteur d'enfants
  child_count <- reactiveVal(1)
  
  # Générer les champs dynamiquement
  output$child_inputs <- renderUI({
    children_ui <- lapply(1:child_count(), function(i) {
      # Determine if the current child has the issue
      classe <- input[[paste0("child_class_", i)]]
      horaire <- input[[paste0("child_time_", i)]]
      name_labels <- if (!is.null(classe) && classe == "Toute Petite Section - dès 2 ans" &&
                        (horaire == "17h00 - midi17" || horaire == "18h00 - maxi")) {
        c("Nom de l'enfant ***", "Classe", "Horaire***", "Vacances")
      } else {
        c("Nom de l'enfant", "Classe", "Horaire", "Vacances")
      }
      
      fluidRow(
        column(2, textInput(
          paste0("child_name_", i), name_labels[1],
          value = ifelse(i <= nrow(child_data()), child_data()$Nom[i], "")
        )),
        column(3, selectInput(
          paste0("child_class_", i), name_labels[2],
          choices = c("", names(garderie)),
          selected = ifelse(i <= nrow(child_data()), child_data()$Classe[i], "")
        )),
        column(2, selectInput(
          paste0("child_time_", i), name_labels[3],
          choices = c("13h10", "14h10 - mini", "15h45 - midi", "17h00 - midi17", "18h00 - maxi"),
          selected = ifelse(i <= nrow(child_data()), child_data()$Horaire[i], "13h10")
        )),
        column(2, selectInput(
          paste0("child_vacation_", i), name_labels[4],
          choices = c("Oui", "Non"),
          selected = ifelse(i <= nrow(child_data()), child_data()$Vacances[i], "Non")
        ))
      )
    })
    
    # Add the error message below child inputs
    children_ui <- append(children_ui, list(
      fluidRow(
        column(12, textOutput("tps_warning"), style = "color: red; font-size: small; margin-top: 5px;")
      )
    ))
    do.call(tagList, children_ui)
  })
  
  # Warning for TPS
  output$tps_warning <- renderText({
    for (i in 1:child_count()) {
      classe <- input[[paste0("child_class_", i)]]
      horaire <- input[[paste0("child_time_", i)]]
      
      if (!is.null(classe) && classe == "Toute Petite Section - dès 2 ans" &&
          (horaire == "17h00 - midi17" || horaire == "18h00 - maxi")) {
        return("***La TPS après 15h45 est disponible uniquement pour les enfants de plus de 3 ans.")
      }
    }
    return("")
  })
  
  # Render the income input box dynamically
  output$income_input <- renderUI({
    if (input$low_income) {
      fluidRow(
        column(8, numericInput("family_income", "Revenu brut du foyer", value = 0, min = 0, step = 1, width = "100%")),
        column(4, p("€", style = "margin-top: 25px;")),
        column(12, textOutput("income_warning"), style = "color: red; font-size: small; margin-top: 5px;")
      )
    }
  })
  
  output$income_warning <- renderText({
    if (input$low_income) {
      if (is.null(input$family_income) || input$family_income == 0) {
        return("Le revenu annuel doit être supérieur à 0€.")
      } else if (input$family_income < 0) {
        return("Le revenu annuel doit être un nombre positif.")
      }
    }
    return("")
  })
  
  # Conserver les données existantes et calculer les totaux
  observe({
    new_data <- lapply(1:child_count(), function(i) {
      classe <- input[[paste0("child_class_", i)]]
      horaire <- input[[paste0("child_time_", i)]]
      vacances <- input[[paste0("child_vacation_", i)]] == "Oui"
      
      if (!is.null(classe) && classe != "" && !is.null(horaire)) {
        # Prix de la classe (écolage)
        prix_classe <- ecolage[[classe]]
        
        # Prix de l'horaire (garderie)
        tarif <- garderie[[classe]]
        col_name <- ifelse(vacances, 
                           ifelse(input$low_income, "reduc_vac", "vac"),
                           ifelse(input$low_income, "reduc", "norm"))
        prix_horaire <- tarif[horaire, col_name]
        
        # Réduction en fonction du rang de l'enfant
        reduction <- reduction.l['no']
        if (i == 2){reduction <- reduction.l['1']}
        if (i >= 3){reduction <- reduction.l['2']}
        
        # Appliquer la réduction sur 
        # les deux (écolage et garderie) pour Élementaire et 
        # seulement sur l'écolage pour Maternelle/TPS
        if (classe == "Élementaire - CP à CM") {
          prix_classe <- prix_classe * reduction
          prix_horaire <- prix_horaire * reduction
        } else {
          prix_classe <- prix_classe * reduction
        }
        
        # Réduction pour les familles sous 72k
        if (input$low_income) {
          income_limit <- (input$family_income %||% 0) / 12 * 0.05
          if (classe == "Élementaire - CP à CM" && prix_classe > income_limit) {
            prix_classe <- income_limit
          }
        }
        
        # Calcul total
        total <- prix_classe + prix_horaire
        return(data.frame(
          Nom = input[[paste0("child_name_", i)]],
          Classe = classe,
          Horaire = horaire,
          Vacances = ifelse(vacances, "Oui", "Non"),
          Écolage = round(prix_classe, 0),
          Garderie = round(prix_horaire, 0),
          Total = round(total, 0)
        ))
      } else {
        return(data.frame(
          Nom = "", Classe = "", Horaire = "13h10", Vacances = "Non",
          Écolage = 0, Garderie = 0, Total = 0
        ))
      }
    })
    
    child_data(do.call(rbind, new_data))
  })
  
  # Ajouter un nouvel enfant sans réinitialiser
  observeEvent(input$add_child, {
    if (child_count() < 5) {
      # Augmenter le compteur
      child_count(child_count() + 1)
    }
  })
  
  # Réinitialiser les données
  observeEvent(input$reset, {
    child_count(1)
    child_data(data.frame(
      Nom = character(), Classe = character(), Horaire = character(),
      Vacances = character(), Écolage = numeric(), Garderie = numeric(),
      Total = numeric(), stringsAsFactors = FALSE
    ))
  })
  
  # Afficher le tableau récapitulatif
  output$summary_table <- renderTable({
    summary <- child_data()
    if (nrow(summary) > 0) {
      total_row <- data.frame(
        Nom = "Total", Classe = "", Horaire = "", Vacances = "",
        Écolage = paste0(round(sum(summary$Écolage, na.rm = TRUE), 0), " €*"),
        Garderie = paste0(round(sum(summary$Garderie, na.rm = TRUE), 0), " €*"),
        Total = paste0(round(sum(summary$Total, na.rm = TRUE), 0), " €*")
      )
      summary <- rbind(summary, total_row)
    }
    summary
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)