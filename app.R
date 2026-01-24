library(shiny)

# Année scolaire (à modifier chaque année)
school_year <- 2026

ecolage <- list(
  "Élementaire - CP à CM" = 340,
  "Maternelle - de 3 à 6 ans" = 291
)

garderie <- list(
  'Élementaire - CP à CM' = data.frame(
    norm = c(0, 127, 196, 281, 380), vac = c(0, 196, 275, 372, 484),
    reduc = c(0, 127, 196, 256, 330), reduc_vac = c(0, 196, 275, 338, 421),
    row.names = c("13h10", "15h00 - mini", "16h00 - midi", "17h00 - midi17", "18h00 - maxi")),
  'Maternelle - de 3 à 6 ans' = data.frame(
    norm = c(0, 103, 159, 219, 280), vac = c(0, 154, 217, 284, 353),
    reduc = c(0, 103, 159, 219, 280), reduc_vac = c(0, 154, 217, 284, 353),
    row.names = c("13h10", "15h00 - mini", "16h00 - midi", "17h00 - midi17", "18h00 - maxi"))
)

association <- 500
reinscritpion <- list(premiere = 500, normale = 50, majoree = 100)
extra_fee_young_maternelle <- 75  # Extra fee per month for kids born after 31/12/(school_year-3) in maternelle

# Flag to show/hide inscription costs in the main app
# Set to TRUE for internal school use to include inscription/reinscription fees
SHOW_INSCRIPTION_COSTS <- FALSE

reduction.l <- c(no=1, `1`=0.85, `2`=0.75)
limitFamilial=round(((ecolage[["Élementaire - CP à CM"]]/5)*100)*12, 0)

ui <- fluidPage(
  titlePanel(paste0("Calcul des frais scolaires par famille ", school_year, "-", school_year + 1)),
  
  # Résumé de l'application
  fluidRow(
    column(12, p(paste0("Ce simulateur permet de calculer les frais scolaires par enfant en fonction de sa classe, 
    des horaires choisis et des options de vacances. Les prix calculés prennent également en compte les 
    réductions appliquées pour les fratries ainsi que celles pour les revenus familiaux inférieurs à ", limitFamilial,"€. le tarif final ne comprends pas la cantine pour les enfants inscrit en garderie."))),
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
    column(4, uiOutput("income_input")),
    column(12, uiOutput("income_footnote"))
  ),
  
  # Tableaux récapitulatifs
  fluidRow(
    column(12, h4("Récapitulatif mensuel"), tableOutput("monthly_table")),
    column(12, uiOutput("yearly_table_header")),
    column(12, p("* Total à titre indicatif n'incluant pas le tarif de la cantine. Validation lors de l'inscription incluant les frais annuels annexes (frais d'inscription ou re-inscription).", style = "font-size: small; color: gray;")),
    column(12, uiOutput("extra_fee_footnote"))
  ),
  
  fluidRow(
    column(12, actionButton("reset", "Réinitialiser"))
  )
  
)

server <- function(input, output, session) {
  # Stocker les données pour chaque enfant
  child_data <- reactiveVal(data.frame(
    Nom = character(), Classe = character(), Horaire = character(),
    Vacances = character(), Inscription = character(), Écolage = numeric(), 
    Garderie = numeric(), InscriptionCost = numeric(),
    HasExtraFee = logical(), Total = numeric(), stringsAsFactors = FALSE
  ))
  
  # Compteur d'enfants
  child_count <- reactiveVal(1)
  
  # Générer les champs dynamiquement
  output$child_inputs <- renderUI({
    children_ui <- lapply(1:child_count(), function(i) {
      # Get current input values first to preserve them when UI re-renders
      current_name <- input[[paste0("child_name_", i)]]
      current_birthdate <- input[[paste0("child_birthdate_", i)]]
      current_class <- input[[paste0("child_class_", i)]]
      current_time <- input[[paste0("child_time_", i)]]
      current_vacation <- input[[paste0("child_vacation_", i)]]
      current_inscription <- input[[paste0("child_inscription_", i)]]
      
      classe <- current_class
      horaire <- current_time
      birth_date <- current_birthdate
      
      # Age validation and class filtering
      birth_date_obj <- NULL
      if (!is.null(birth_date) && !is.na(birth_date)) {
        birth_date_obj <- if (inherits(birth_date, "Date")) {
          birth_date
        } else {
          tryCatch(as.Date(birth_date), error = function(e) NA)
        }
      }
      
      # Calculate age eligibility
      eligible_elementary <- FALSE
      eligible_maternelle <- FALSE
      is_older_than_3 <- FALSE
      
      if (!is.null(birth_date_obj) && !is.na(birth_date_obj)) {
        # For Élémentaire: must be 6 years old before end of school_year
        # Born before (school_year - 6) - 12-31
        elementary_cutoff <- as.Date(paste0(school_year - 6, "-12-31"))
        comparison <- birth_date_obj <= elementary_cutoff
        eligible_elementary <- ifelse(is.na(comparison), FALSE, comparison)
        
        # For Maternelle: must be 2 years old before July of (school_year + 1)
        # Born before (school_year + 1) - 07-01, minus 2 years = (school_year - 1) - 07-01
        maternelle_min_cutoff <- as.Date(paste0(school_year - 1, "-07-01"))
        # Cannot be older than 8: born after (school_year - 8 - 1) - 12-31
        maternelle_max_cutoff <- as.Date(paste0(school_year - 8 - 1, "-12-31"))
        comp1 <- birth_date_obj >= maternelle_max_cutoff
        comp2 <- birth_date_obj < maternelle_min_cutoff
        eligible_maternelle <- ifelse(is.na(comp1) || is.na(comp2), FALSE, comp1 && comp2)
        
        # For horaire restrictions: must be 3+ at start of school year
        # Born before (school_year - 3) - 09-01
        age_3_cutoff <- as.Date(paste0(school_year - 3, "-09-01"))
        comparison <- birth_date_obj < age_3_cutoff
        is_older_than_3 <- ifelse(is.na(comparison), FALSE, comparison)
      }
      
      # Filter class choices based on age eligibility
      available_classes <- c("")
      if (eligible_elementary) {
        available_classes <- c(available_classes, "Élementaire - CP à CM")
      }
      if (eligible_maternelle) {
        available_classes <- c(available_classes, "Maternelle - de 3 à 6 ans")
      }
      # If no date entered or not eligible, show all classes
      if (is.null(birth_date_obj) || is.na(birth_date_obj)) {
        available_classes <- c("", names(garderie))
      }
      
      # Filter horaire choices based on age
      horaire_choices <- c("13h10", "15h00 - mini", "16h00 - midi")
      if (is_older_than_3) {
        horaire_choices <- c(horaire_choices, "17h00 - midi17", "18h00 - maxi")
      }
      
      # Warning label for horaire if needed
      horaire_label <- "Horaire"
      if (!is.null(horaire) && (horaire == "17h00 - midi17" || horaire == "18h00 - maxi") && !is_older_than_3) {
        horaire_label <- "Horaire ***"
      }
      
      # Warning label for class if needed
      class_warning <- ""
      if (!is.null(birth_date_obj) && !is.na(birth_date_obj)) {
        if (!is.null(current_class) && current_class != "") {
          if (current_class == "Élementaire - CP à CM" && !eligible_elementary) {
            class_warning <- "***"
          } else if (current_class == "Maternelle - de 3 à 6 ans" && !eligible_maternelle) {
            class_warning <- "***"
          }
        }
      }
      
      # Use current input values if they exist, otherwise use child_data or defaults
      name_value <- if (!is.null(current_name) && current_name != "") current_name else 
                    if (i <= nrow(child_data()) && child_data()$Nom[i] != "") child_data()$Nom[i] else ""
      
      birthdate_value <- if (!is.null(current_birthdate) && !is.na(current_birthdate)) current_birthdate else
                         as.Date(paste0(school_year - 3, "-01-01"))
      
      class_value <- if (!is.null(current_class) && current_class != "") current_class else
                     if (i <= nrow(child_data()) && child_data()$Classe[i] != "") child_data()$Classe[i] else ""
      
      # If selected class is not in available classes, reset to empty
      if (class_value != "" && !class_value %in% available_classes) {
        class_value <- ""
      }
      
      time_value <- {
        if (!is.null(current_time) && current_time %in% horaire_choices) {
          current_time
        } else if (!is.null(current_time) && current_time != "") {
          # If current time is not in choices (e.g., midi17 for young child), use default
          if ("13h10" %in% horaire_choices) "13h10" else horaire_choices[1]
        } else if (i <= nrow(child_data()) && child_data()$Horaire[i] %in% horaire_choices) {
          child_data()$Horaire[i]
        } else {
          if ("13h10" %in% horaire_choices) "13h10" else horaire_choices[1]
        }
      }
      
      vacation_value <- if (!is.null(current_vacation) && current_vacation != "") current_vacation else
                        if (i <= nrow(child_data()) && child_data()$Vacances[i] != "") child_data()$Vacances[i] else "Non"
      
      # Inscription value - only used if SHOW_INSCRIPTION_COSTS is TRUE
      inscription_value <- if (SHOW_INSCRIPTION_COSTS) {
        if (!is.null(current_inscription) && current_inscription != "") current_inscription else
          if (i <= nrow(child_data()) && !is.null(child_data()$Inscription[i]) && child_data()$Inscription[i] != "") 
            child_data()$Inscription[i] else "premiere"
      } else {
        "premiere"  # Default value when disabled
      }
      
      fluidRow(
        column(2, textInput(
          paste0("child_name_", i), "Nom de l'enfant",
          value = name_value
        )),
        column(2, dateInput(
          paste0("child_birthdate_", i), "Date de naissance",
          value = birthdate_value,
          format = "dd/mm/yyyy",
          language = "fr",
          startview = "year"
        )),
        column(2, selectInput(
          paste0("child_class_", i), paste0("Classe", class_warning),
          choices = available_classes,
          selected = class_value
        )),
        column(2, selectInput(
          paste0("child_time_", i), horaire_label,
          choices = horaire_choices,
          selected = time_value
        )),
        column(2, selectInput(
          paste0("child_vacation_", i), "Vacances",
          choices = c("Oui", "Non"),
          selected = vacation_value
        ))
        # Inscription dropdown - hidden when SHOW_INSCRIPTION_COSTS is FALSE
        # To enable: set SHOW_INSCRIPTION_COSTS <- TRUE at the top of the file
        # Original code (commented for easy reactivation):
        # ,column(2, selectInput(
        #   paste0("child_inscription_", i), "Inscription",
        #   choices = list(
        #     "1ère inscription" = "premiere",
        #     "Re-inscription" = "normale",
        #     "Reinscription majorée" = "majoree"
        #   ),
        #   selected = inscription_value
        # ))
      )
    })
    
    # Add the error message below child inputs
    children_ui <- append(children_ui, list(
      fluidRow(
        column(12, textOutput("age_warning"), style = "color: red; font-size: small; margin-top: 5px;")
      )
    ))
    do.call(tagList, children_ui)
  })
  
  # Render yearly table header conditionally
  output$yearly_table_header <- renderUI({
    if (SHOW_INSCRIPTION_COSTS) {
      h4("Frais annuels supplémentaires")
    }
  })
  
  # Warning for age restrictions
  output$age_warning <- renderText({
    warnings <- character()
    for (i in 1:child_count()) {
      classe <- input[[paste0("child_class_", i)]]
      horaire <- input[[paste0("child_time_", i)]]
      birth_date <- input[[paste0("child_birthdate_", i)]]
      
      if (!is.null(birth_date) && !is.na(birth_date)) {
        birth_date_obj <- if (inherits(birth_date, "Date")) {
          birth_date
        } else {
          tryCatch(as.Date(birth_date), error = function(e) NA)
        }
        
        if (!is.na(birth_date_obj)) {
          # Check class eligibility
          if (!is.null(classe) && classe != "") {
            # For Élémentaire: must be 6 years old before end of school_year
            elementary_cutoff <- as.Date(paste0(school_year - 6, "-12-31"))
            eligible_elementary <- birth_date_obj <= elementary_cutoff
            
            # For Maternelle: must be 2 years old before July of (school_year + 1)
            maternelle_min_cutoff <- as.Date(paste0(school_year - 1, "-07-01"))
            maternelle_max_cutoff <- as.Date(paste0(school_year - 8 - 1, "-12-31"))
            eligible_maternelle <- birth_date_obj >= maternelle_max_cutoff && birth_date_obj < maternelle_min_cutoff
            
            if (classe == "Élementaire - CP à CM" && !eligible_elementary) {
              warnings <- c(warnings, paste0("***L'enfant ", i, " doit avoir 6 ans avant le 31/12/", school_year, " pour être en Élémentaire."))
            } else if (classe == "Maternelle - de 3 à 6 ans" && !eligible_maternelle) {
              if (birth_date_obj < maternelle_max_cutoff) {
                warnings <- c(warnings, paste0("***L'enfant ", i, " a plus de 8 ans et ne peut pas être en Maternelle."))
              } else {
                warnings <- c(warnings, paste0("***L'enfant ", i, " doit avoir 2 ans avant le 01/07/", school_year + 1, " pour être en Maternelle."))
              }
            }
          }
          
          # Check horaire restrictions
          if (!is.null(horaire) && (horaire == "17h00 - midi17" || horaire == "18h00 - maxi")) {
            age_3_cutoff <- as.Date(paste0(school_year - 3, "-09-01"))
            if (birth_date_obj >= age_3_cutoff) {
              warnings <- c(warnings, paste0("***Les horaires midi17 et maxi tarif sont disponibles uniquement pour les enfants de plus de 3 ans."))
            }
          }
        }
      } else if (!is.null(classe) && classe != "" && !is.null(horaire) && 
                 (horaire == "17h00 - midi17" || horaire == "18h00 - maxi")) {
        warnings <- c(warnings, "***Veuillez entrer la date de naissance pour vérifier l'âge requis.")
      }
    }
    if (length(warnings) > 0) {
      return(paste(warnings, collapse = " "))
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
  
  # Display income reduction footnote under the checkbox
  output$income_footnote <- renderUI({
    if (input$low_income) {
      p("** Réduction basées sur le revenu des familles sur tarifs élémentaire uniquement. Sur demande et sur justificatifs.", 
        style = "font-size: small; color: gray; margin-top: 5px;")
    }
  })
  
  # Display extra fee footnote only if there's a child with the extra fee
  output$extra_fee_footnote <- renderUI({
    summary <- child_data()
    if (nrow(summary) > 0) {
      # Check if any child has the extra fee (born after cutoff in maternelle)
      has_extra_fee <- FALSE
      for (i in 1:child_count()) {
        classe <- input[[paste0("child_class_", i)]]
        birth_date <- input[[paste0("child_birthdate_", i)]]
        if (!is.null(classe) && classe == "Maternelle - de 3 à 6 ans" && !is.null(birth_date) && !is.na(birth_date)) {
          birth_date_obj <- if (inherits(birth_date, "Date")) {
            birth_date
          } else {
            tryCatch(as.Date(birth_date), error = function(e) NA)
          }
          if (!is.na(birth_date_obj)) {
            cutoff_date <- as.Date(paste0(school_year - 3, "-12-31"))
            comparison <- birth_date_obj > cutoff_date
            if (!is.na(comparison) && comparison) {
              has_extra_fee <- TRUE
              break
            }
          }
        }
      }
      if (has_extra_fee) {
        p(paste0("*** Le supplément de 75€/mois pour les enfants nés après le 31/12/", school_year - 3, " en maternelle est inclus dans l'Écolage."), 
          style = "font-size: small; color: gray;")
      }
    }
  })
  
  # Conserver les données existantes et calculer les totaux
  observe({
      # First, collect all children data with birth dates
      children_list <- lapply(1:child_count(), function(i) {
        classe <- input[[paste0("child_class_", i)]]
        horaire <- input[[paste0("child_time_", i)]]
        vacances <- input[[paste0("child_vacation_", i)]] == "Oui"
        birth_date <- input[[paste0("child_birthdate_", i)]]
        # Get inscription type only if SHOW_INSCRIPTION_COSTS is enabled
        inscription_type <- if (SHOW_INSCRIPTION_COSTS) {
          input[[paste0("child_inscription_", i)]]
        } else {
          NULL
        }
        name <- input[[paste0("child_name_", i)]]
      
      if (!is.null(classe) && classe != "" && !is.null(horaire)) {
        # Parse birth date
        birth_date_obj <- NULL
        if (!is.null(birth_date) && !is.na(birth_date)) {
          birth_date_obj <- if (inherits(birth_date, "Date")) {
            birth_date
          } else {
            tryCatch(as.Date(birth_date), error = function(e) NA)
          }
        }
        
        return(list(
          index = i,
          name = name,
          classe = classe,
          horaire = horaire,
          vacances = vacances,
          birth_date = birth_date,
          birth_date_obj = birth_date_obj,
          inscription_type = inscription_type
        ))
      } else {
        return(NULL)
      }
    })
    
    # Filter out NULL entries
    valid_children <- children_list[!sapply(children_list, is.null)]
    
    if (length(valid_children) > 0) {
      # Sort children by birth date (oldest first)
      birth_dates <- sapply(valid_children, function(x) {
        if (!is.null(x$birth_date_obj) && !is.na(x$birth_date_obj)) {
          as.numeric(x$birth_date_obj)
        } else {
          Inf  # Put children without birth dates at the end
        }
      })
      sorted_indices <- order(birth_dates)
      sorted_children <- valid_children[sorted_indices]
      
      # Now calculate prices with reductions based on age order
      new_data <- lapply(1:length(sorted_children), function(age_rank) {
        child <- sorted_children[[age_rank]]
        i <- child$index
        
        classe <- child$classe
        horaire <- child$horaire
        vacances <- child$vacances
        birth_date <- child$birth_date
        birth_date_obj <- child$birth_date_obj
        inscription_type <- child$inscription_type
        
        # Prix de la classe (écolage)
        prix_classe <- ecolage[[classe]]
        
        # Extra fee for kids born after 31/12/2023 in maternelle (75€ per month) - add to ecolage
        has_extra_fee <- FALSE
        if (classe == "Maternelle - de 3 à 6 ans" && !is.null(birth_date_obj) && !is.na(birth_date_obj)) {
          cutoff_date <- as.Date(paste0(school_year - 3, "-12-31"))
          comparison <- birth_date_obj > cutoff_date
          if (!is.na(comparison) && comparison) {
            # 75€ per month - add to ecolage
            prix_classe <- prix_classe + extra_fee_young_maternelle
            has_extra_fee <- TRUE
          }
        }
        
        # Prix de l'horaire (garderie)
        tarif <- garderie[[classe]]
        col_name <- ifelse(vacances, 
                           ifelse(input$low_income, "reduc_vac", "vac"),
                           ifelse(input$low_income, "reduc", "norm"))
        prix_horaire <- tarif[horaire, col_name]
        
        # Réduction en fonction du rang d'âge (oldest = no reduction, second = 0.85, third+ = 0.75)
        reduction <- reduction.l['no']  # Oldest (age_rank = 1)
        if (age_rank == 2) {
          reduction <- reduction.l['1']  # Second oldest
        } else if (age_rank >= 3) {
          reduction <- reduction.l['2']  # Third and beyond
        }
        
        # Appliquer la réduction sur 
        # les deux (écolage et garderie) pour Élementaire et 
        # seulement sur l'écolage pour Maternelle
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
        
        # Calculate inscription cost
        # Set to 0 when SHOW_INSCRIPTION_COSTS is FALSE (for public app)
        # To enable: set SHOW_INSCRIPTION_COSTS <- TRUE at the top of the file
        inscription_cost <- 0
        if (SHOW_INSCRIPTION_COSTS) {
          if (!is.null(inscription_type) && inscription_type != "") {
            inscription_cost <- reinscritpion[[inscription_type]] %||% reinscritpion$premiere
          }
        }
        # Original logic (commented for easy reactivation):
        # inscription_cost <- 0
        # if (!is.null(inscription_type) && inscription_type != "") {
        #   inscription_cost <- reinscritpion[[inscription_type]] %||% reinscritpion$premiere
        # }
        
        # Calcul total
        total <- prix_classe + prix_horaire + inscription_cost
        return(data.frame(
          Nom = child$name,
          Classe = classe,
          Horaire = horaire,
          Vacances = ifelse(vacances, "Oui", "Non"),
          Inscription = ifelse(!is.null(inscription_type), inscription_type, "premiere"),
          Écolage = round(prix_classe, 0),
          Garderie = round(prix_horaire, 0),
          InscriptionCost = round(inscription_cost, 0),
          HasExtraFee = has_extra_fee,
          Total = round(total, 0)
        ))
      })
      
      child_data(do.call(rbind, new_data))
    } else {
      child_data(data.frame(
        Nom = character(), Classe = character(), Horaire = character(),
        Vacances = character(), Inscription = character(), Écolage = numeric(), 
        Garderie = numeric(), InscriptionCost = numeric(),
        HasExtraFee = logical(), Total = numeric(), stringsAsFactors = FALSE
      ))
    }
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
    # Reset child count
    child_count(1)
    
    # Clear all child input fields
    for (i in 1:5) {  # Reset up to 5 children (max allowed)
      updateTextInput(session, paste0("child_name_", i), value = "")
      updateDateInput(session, paste0("child_birthdate_", i), 
                     value = as.Date(paste0(school_year - 3, "-01-01")))
      updateSelectInput(session, paste0("child_class_", i), selected = "")
      updateSelectInput(session, paste0("child_time_", i), selected = "13h10")
      updateSelectInput(session, paste0("child_vacation_", i), selected = "Non")
      if (SHOW_INSCRIPTION_COSTS) {
        updateSelectInput(session, paste0("child_inscription_", i), selected = "premiere")
      }
    }
    
    # Reset income checkbox and input
    updateCheckboxInput(session, "low_income", value = FALSE)
    updateNumericInput(session, "family_income", value = 0)
    
    # Clear child data
    child_data(data.frame(
      Nom = character(), Classe = character(), Horaire = character(),
      Vacances = character(), Inscription = character(), Écolage = numeric(), 
      Garderie = numeric(), InscriptionCost = numeric(),
      HasExtraFee = logical(), Total = numeric(), stringsAsFactors = FALSE
    ))
  })
  
  # Afficher le tableau récapitulatif mensuel (Écolage et Garderie)
  output$monthly_table <- renderTable({
    summary <- child_data()
    if (nrow(summary) > 0) {
      # Monthly table: Écolage (includes extra fee for young maternelle) and Garderie
      # Add *** to ecolage for kids with extra fee
      ecolage_display <- ifelse(
        summary$HasExtraFee,
        paste0(round(summary$Écolage, 0), " €***"),
        paste0(round(summary$Écolage, 0), " €")
      )
      monthly_summary <- data.frame(
        Nom = summary$Nom,
        Classe = summary$Classe,
        Horaire = summary$Horaire,
        Vacances = summary$Vacances,
        Écolage = ecolage_display,
        Garderie = paste0(round(summary$Garderie, 0), " €"),
        `Total mensuel` = paste0(round(summary$Écolage + summary$Garderie, 0), " €"),
        stringsAsFactors = FALSE
      )
      
      # Calculate monthly totals
      total_ecolage <- round(sum(summary$Écolage, na.rm = TRUE), 0)
      total_garderie <- round(sum(summary$Garderie, na.rm = TRUE), 0)
      total_monthly <- total_ecolage + total_garderie
      
      # Add total row
      total_row <- data.frame(
        Nom = "Total mensuel", Classe = "", Horaire = "", Vacances = "",
        Écolage = paste0(total_ecolage, " €*"),
        Garderie = paste0(total_garderie, " €*"),
        `Total mensuel` = paste0(total_monthly, " €*"),
        stringsAsFactors = FALSE
      )
      monthly_summary <- rbind(monthly_summary, total_row)
      monthly_summary
    } else {
      NULL
    }
  })
  
  # Afficher le tableau récapitulatif annuel (Écolage × 12, Garderie × 12 [+ Inscription])
  # Only rendered when SHOW_INSCRIPTION_COSTS is TRUE
  output$yearly_table <- renderTable({
    # Return NULL if inscription costs are disabled
    if (!SHOW_INSCRIPTION_COSTS) {
      return(NULL)
    }
    
    summary <- child_data()
    if (nrow(summary) > 0) {
      # Yearly table: Écolage × 12, Garderie × 12, [plus inscription fees if enabled]
      # Add *** to ecolage for kids with extra fee
      ecolage_yearly_display <- ifelse(
        summary$HasExtraFee,
        paste0(round(summary$Écolage * 12, 0), " €***"),
        paste0(round(summary$Écolage * 12, 0), " €")
      )
      
      # Build yearly summary - conditionally include inscription column
      if (SHOW_INSCRIPTION_COSTS) {
        yearly_summary <- data.frame(
          Nom = summary$Nom,
          Classe = summary$Classe,
          Horaire = summary$Horaire,
          Vacances = summary$Vacances,
          Écolage = ecolage_yearly_display,
          Garderie = paste0(round(summary$Garderie * 12, 0), " €"),
          Inscription = paste0(round(summary$InscriptionCost, 0), " €"),
          `Total annuel` = paste0(round(summary$Écolage * 12 + summary$Garderie * 12 + summary$InscriptionCost, 0), " €"),
          stringsAsFactors = FALSE
        )
        
        # Calculate yearly totals
        total_ecolage_yearly <- round(sum(summary$Écolage, na.rm = TRUE) * 12, 0)
        total_garderie_yearly <- round(sum(summary$Garderie, na.rm = TRUE) * 12, 0)
        total_inscription <- round(sum(summary$InscriptionCost, na.rm = TRUE), 0)
        total_yearly <- total_ecolage_yearly + total_garderie_yearly + total_inscription
        
        # Add total row
        total_row <- data.frame(
          Nom = "Total annuel", Classe = "", Horaire = "", Vacances = "",
          Écolage = paste0(total_ecolage_yearly, " €"),
          Garderie = paste0(total_garderie_yearly, " €"),
          Inscription = paste0(total_inscription, " €"),
          `Total annuel` = paste0(total_yearly, " €*"),
          stringsAsFactors = FALSE
        )
      } else {
        # Without inscription costs
        yearly_summary <- data.frame(
          Nom = summary$Nom,
          Classe = summary$Classe,
          Horaire = summary$Horaire,
          Vacances = summary$Vacances,
          Écolage = ecolage_yearly_display,
          Garderie = paste0(round(summary$Garderie * 12, 0), " €"),
          `Total annuel` = paste0(round(summary$Écolage * 12 + summary$Garderie * 12, 0), " €"),
          stringsAsFactors = FALSE
        )
        
        # Calculate yearly totals (without inscription)
        total_ecolage_yearly <- round(sum(summary$Écolage, na.rm = TRUE) * 12, 0)
        total_garderie_yearly <- round(sum(summary$Garderie, na.rm = TRUE) * 12, 0)
        total_yearly <- total_ecolage_yearly + total_garderie_yearly
        
        # Add total row
        total_row <- data.frame(
          Nom = "Total annuel", Classe = "", Horaire = "", Vacances = "",
          Écolage = paste0(total_ecolage_yearly, " €"),
          Garderie = paste0(total_garderie_yearly, " €"),
          `Total annuel` = paste0(total_yearly, " €*"),
          stringsAsFactors = FALSE
        )
      }
      
      yearly_summary <- rbind(yearly_summary, total_row)
      yearly_summary
    } else {
      NULL
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
