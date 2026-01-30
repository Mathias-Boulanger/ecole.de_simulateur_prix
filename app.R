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
    row.names = c("13h10", "15h00", "16h00", "17h00", "18h00")),
  'Maternelle - de 3 à 6 ans' = data.frame(
    norm = c(0, 103, 159, 219, 280), vac = c(0, 154, 217, 284, 353),
    reduc = c(0, 103, 159, 219, 280), reduc_vac = c(0, 154, 217, 284, 353),
    row.names = c("13h10", "15h00", "16h00", "17h00", "18h00"))
)

association <- 500
reinscritpion <- list(premiere = 500, normale = 50, majoree = 100)
extra_fee_young_maternelle <- 75  # Extra fee per month for kids born after 31/12/(school_year-3) in maternelle

# Flag to show/hide inscription costs in the main app
# Set to TRUE for internal school use to include inscription/reinscription fees
SHOW_INSCRIPTION_COSTS <- FALSE

reduction.l <- c(no=1, `1`=0.85, `2`=0.75)
limitFamilial=round(((ecolage[["Élementaire - CP à CM"]]/5)*100)*12, 0)

# Internal keys for class (used in data); display names come from i18n
CLASS_ELEM <- "Élementaire - CP à CM"
CLASS_MAT <- "Maternelle - de 3 à 6 ans"

# Translations: French (fr) and German (de)
i18n <- list(
  fr = list(
    title = paste0("Calcul des frais scolaires par famille ", school_year, "-", school_year + 1),
    intro = paste0("Ce simulateur permet de calculer les frais scolaires par enfant en fonction de sa classe, des horaires choisis et des options de vacances. Les prix calculés prennent également en compte les réductions appliquées pour les fratries ainsi que celles pour les revenus familiaux inférieurs à ", limitFamilial, "€. Le tarif final ne comprend pas la cantine pour les enfants inscrits en garderie."),
    family_info = "Informations de la famille",
    add_child = "Ajouter un enfant",
    low_income_label = paste0("Revenu familial < ", limitFamilial, "€**"),
    income_label = "Revenu du foyer (selon le barème de la ville)",
    income_warning_zero = "Le revenu annuel doit être supérieur à 0€.",
    income_warning_negative = "Le revenu annuel doit être un nombre positif.",
    income_footnote = "** Réduction basée sur le revenu des familles sur tarifs élémentaire uniquement. Sur demande et sur justificatifs.",
    income_barème_link = "Barème / document de calcul du revenu (ville)",
    recap_monthly = "Récapitulatif mensuel",
    reset = "Réinitialiser",
    child_name = "Nom de l'enfant",
    birthdate = "Date de naissance",
    class = "Classe",
    time = "Horaire",
    vacation = "Vacances",
    vacation_yes = "Oui",
    vacation_no = "Non",
    total_monthly = "Total mensuel",
    total_annual = "Total annuel",
    ecolage = "Écolage",
    garderie = "Garderie",
    inscription = "Inscription",
    col_nom = "Nom",
    col_classe = "Classe",
    col_horaire = "Horaire",
    col_vacances = "Vacances",
    class_elementary = CLASS_ELEM,
    class_maternelle = CLASS_MAT,
    indicatif_footnote = "* Total à titre indicatif, sous réserve de validation du secrétariat. Le tarif final n'inclut pas :",
    indicatif_cantine = "Le tarif de la cantine ;",
    indicatif_inscription = "les frais d'inscription ou de re-inscription ;",
    indicatif_cotisation = "les frais de cotisation à l'association de parents gestionnaires.",
    extra_fee_footnote = paste0("*** Le supplément de 75€/mois pour les enfants nés après le 31/12/", school_year - 3, " en maternelle est inclus dans l'Écolage."),
    yearly_header = "Frais annuels supplémentaires",
    age_warn_elementary = paste0("***L'enfant ", "%s", " doit avoir 6 ans avant le 31/12/", school_year, " pour être en Élémentaire."),
    age_warn_maternelle_old = paste0("***L'enfant ", "%s", " a plus de 8 ans et ne peut pas être en Maternelle."),
    age_warn_maternelle_young = paste0("***L'enfant ", "%s", " doit avoir 2 ans avant le 01/07/", school_year + 1, " pour être en Maternelle."),
    age_warn_horaire = "***Les horaires 17h et 18h tarif sont disponibles uniquement pour les enfants de plus de 3 ans.",
    age_warn_birthdate = "***Veuillez entrer la date de naissance pour vérifier l'âge requis."
  ),
  de = list(
    title = paste0("Berechnung der Schulkosten pro Familie ", school_year, "-", school_year + 1),
    intro = paste0("Dieser Simulator berechnet die Schulkosten pro Kind je nach Klasse, gewählten Zeiten und Ferienoptionen. Die berechneten Preise berücksichtigen auch Ermäßigungen für Geschwister sowie für Haushalte mit Einkommen unter ", limitFamilial, "€. Der Endpreis enthält nicht die Kantine für Kinder in der Nachmittagsbetreuung."),
    family_info = "Angaben zur Familie",
    add_child = "Kind hinzufügen",
    low_income_label = paste0("Haushaltseinkommen < ", limitFamilial, "€**"),
    income_label = "Haushaltseinkommen (laut städtischem Berechnungsbogen)",
    income_warning_zero = "Das Jahreseinkommen muss über 0€ liegen.",
    income_warning_negative = "Das Jahreseinkommen muss eine positive Zahl sein.",
    income_footnote = "** Ermäßigung basierend auf dem Einkommen der Familien, nur für Elementarstufe. Auf Antrag und mit Nachweisen.",
    income_barème_link = "Berechnungsbogen / Einkommensnachweis (Stadt)",
    recap_monthly = "Monatliche Übersicht",
    reset = "Zurücksetzen",
    child_name = "Name des Kindes",
    birthdate = "Geburtsdatum",
    class = "Klasse",
    time = "Betreuungszeit",
    vacation = "Ferien",
    vacation_yes = "Ja",
    vacation_no = "Nein",
    total_monthly = "Monatssumme",
    total_annual = "Jahressumme",
    ecolage = "Schulgeld",
    garderie = "Nachmittag",
    inscription = "Einschreibung",
    col_nom = "Name",
    col_classe = "Klasse",
    col_horaire = "Zeit",
    col_vacances = "Ferien",
    class_elementary = "Grundschule – 1. bis 4. Klasse",
    class_maternelle = "Kindergarten – 3 bis 6 Jahre",
    indicatif_footnote = "* Gesamtbetrag nur zur Orientierung, vorbehaltlich der Bestätigung durch die Verwaltung. Der Endpreis enthält nicht:",
    indicatif_cantine = "den Kantinenpreis;",
    indicatif_inscription = "Einschreibungs- oder Rückmeldegebühren;",
    indicatif_cotisation = "den Beitrag zum Elternverein.",
    extra_fee_footnote = paste0("*** Der Zuschlag von 75€/Monat für nach dem 31.12.", school_year - 3, " geborene Kinder im Kindergarten ist im Schulgeld enthalten."),
    yearly_header = "Jährliche Zusatzkosten",
    age_warn_elementary = paste0("***Kind ", "%s", " muss vor dem 31.12.", school_year, " 6 Jahre alt sein für die Elementarstufe."),
    age_warn_maternelle_old = paste0("***Kind ", "%s", " ist älter als 8 Jahre und kann nicht den Kindergarten besuchen."),
    age_warn_maternelle_young = paste0("***Kind ", "%s", " muss vor dem 01.07.", school_year + 1, " 2 Jahre alt sein für den Kindergarten."),
    age_warn_horaire = "***Die Zeiten 17 Uhr und 18 Uhr sind nur für Kinder über 3 Jahre verfügbar.",
    age_warn_birthdate = "***Bitte Geburtsdatum angeben, um das erforderliche Alter zu prüfen."
  )
)

ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".lang-switcher { position: fixed; top: 8px; right: 12px; z-index: 1000; }
     .lang-switcher a { margin-left: 6px; font-size: 1.2em; text-decoration: none; }"
  ))),
  div(class = "lang-switcher", uiOutput("lang_switcher")),
  uiOutput("main_content")
)

server <- function(input, output, session) {
  # Language: French by default, toggle with flag
  lang <- reactiveVal("fr")
  tr <- reactive({ i18n[[lang()]] })

  # Language switcher (show other language flag in top right)
  output$lang_switcher <- renderUI({
    if (lang() == "fr") {
      actionLink("set_lang_de", title = "Deutsch", label = "🇩🇪", style = "font-size: 1.5em;")
    } else {
      actionLink("set_lang_fr", title = "Français", label = "🇫🇷", style = "font-size: 1.5em;")
    }
  })
  observeEvent(input$set_lang_de, { lang("de") })
  observeEvent(input$set_lang_fr, { lang("fr") })

  # Main content (re-renders when language changes)
  output$main_content <- renderUI({
    tagList(
      titlePanel(tr()$title),
      fluidRow(column(12, p(tr()$intro))),
      fluidRow(column(12, h4(tr()$family_info), uiOutput("child_inputs"))),
      fluidRow(column(12, actionButton("add_child", tr()$add_child))),
      fluidRow(
        column(4, checkboxInput("low_income", tr()$low_income_label, FALSE)),
        column(4, uiOutput("income_input")),
        column(12, uiOutput("income_footnote"))
      ),
      fluidRow(
        column(12, h4(tr()$recap_monthly), tableOutput("monthly_table")),
        column(12, uiOutput("yearly_table_header")),
        column(12, uiOutput("indicatif_footnote")),
        column(12, uiOutput("extra_fee_footnote"))
      ),
      fluidRow(column(12, actionButton("reset", tr()$reset)))
    )
  })

  # Stocker les données pour chaque enfant
  child_data <- reactiveVal(data.frame(
    Nom = character(), Classe = character(), Horaire = character(),
    Vacances = character(), Inscription = character(), Écolage = numeric(), 
    Garderie = numeric(), InscriptionCost = numeric(),
    HasExtraFee = logical(), Total = numeric(), stringsAsFactors = FALSE
  ))
  
  # Compteur d'enfants
  child_count <- reactiveVal(1)
  
  # Reactive that depends on all writable/calculation inputs (triggers recalculation)
  calc_trigger <- reactive({
    fam_income <- if (isTRUE(input$low_income) && !is.null(input$family_income)) {
      input$family_income
    } else {
      0
    }
    out <- list(
      n = child_count(),
      low_income = isTRUE(input$low_income),
      family_income = fam_income
    )
    for (i in seq_len(child_count())) {
      out[[paste0("child_", i)]] <- list(
        name = input[[paste0("child_name_", i)]],
        class = input[[paste0("child_class_", i)]],
        time = input[[paste0("child_time_", i)]],
        vacation = input[[paste0("child_vacation_", i)]],
        birthdate = input[[paste0("child_birthdate_", i)]]
      )
      if (SHOW_INSCRIPTION_COSTS) {
        out[[paste0("child_", i)]][["inscription"]] <- input[[paste0("child_inscription_", i)]]
      }
    }
    out
  })
  
  # Debounce all writable fields: update app 1500ms after user stops typing
  calc_trigger_debounced <- debounce(calc_trigger, millis = 1500)
  
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
          tryCatch(as.Date(birth_date), error = function(e) { NA })
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
      
      # Filter class choices based on age eligibility (internal keys)
      available_classes <- c("")
      if (eligible_elementary) {
        available_classes <- c(available_classes, CLASS_ELEM)
      }
      if (eligible_maternelle) {
        available_classes <- c(available_classes, CLASS_MAT)
      }
      if (is.null(birth_date_obj) || is.na(birth_date_obj)) {
        available_classes <- c("", CLASS_ELEM, CLASS_MAT)
      }
      class_labels <- c(CLASS_ELEM = tr()$class_elementary, CLASS_MAT = tr()$class_maternelle)
      available_class_choices <- setNames(
        available_classes, 
        sapply(available_classes, function(x) {
          if (x == "") return("")
          if (x %in% names(class_labels)) return(class_labels[x])
          return(x)
        })
      )

      # Filter horaire choices based on age
      horaire_choices <- c("13h10", "15h00", "16h00")
      if (is_older_than_3) {
        horaire_choices <- c(horaire_choices, "17h00", "18h00")
      }

      horaire_label <- tr()$time
      if (!is.null(horaire) && (horaire == "17h00" || horaire == "18h00") && !is_older_than_3) {
        horaire_label <- paste0(tr()$time, " ***")
      }

      class_warning <- ""
      if (!is.null(birth_date_obj) && !is.na(birth_date_obj)) {
        if (!is.null(current_class) && current_class != "") {
          if (current_class == CLASS_ELEM && !eligible_elementary) {
            class_warning <- "***"
          } else if (current_class == CLASS_MAT && !eligible_maternelle) {
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
          if ("13h10" %in% horaire_choices) "13h10" else horaire_choices[1]
        } else if (i <= nrow(child_data()) && child_data()$Horaire[i] %in% horaire_choices) {
          child_data()$Horaire[i]
        } else {
          if ("13h10" %in% horaire_choices) "13h10" else horaire_choices[1]
        }
      }
      
      vacation_value <- if (!is.null(current_vacation) && current_vacation != "") current_vacation else
                        if (i <= nrow(child_data()) && child_data()$Vacances[i] != "") child_data()$Vacances[i] else "Non"  # value stays "Oui"/"Non"

      vacation_choices <- setNames(c("Oui", "Non"), c(tr()$vacation_yes, tr()$vacation_no))
      if (!is.null(time_value) && time_value == "13h10") {
        vacation_value <- "Non"
        vacation_choices <- setNames("Non", tr()$vacation_no)
      }
      
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
          paste0("child_name_", i), tr()$child_name,
          value = name_value
        )),
        column(2, dateInput(
          paste0("child_birthdate_", i), tr()$birthdate,
          value = birthdate_value,
          format = "dd/mm/yyyy",
          language = lang(),
          startview = "year"
        )),
        column(2, selectInput(
          paste0("child_class_", i), paste0(tr()$class, class_warning),
          choices = available_class_choices,
          selected = class_value
        )),
        column(2, selectInput(
          paste0("child_time_", i), horaire_label,
          choices = horaire_choices,
          selected = time_value
        )),
        column(2, selectInput(
          paste0("child_vacation_", i), tr()$vacation,
          choices = vacation_choices,
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
  
  output$yearly_table_header <- renderUI({
    if (SHOW_INSCRIPTION_COSTS) {
      h4(tr()$yearly_header)
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
          tryCatch(as.Date(birth_date), error = function(e) { NA })
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
            
            if (classe == CLASS_ELEM && !eligible_elementary) {
              warnings <- c(warnings, sprintf(tr()$age_warn_elementary, i))
            } else if (classe == CLASS_MAT && !eligible_maternelle) {
              if (birth_date_obj < maternelle_max_cutoff) {
                warnings <- c(warnings, sprintf(tr()$age_warn_maternelle_old, i))
              } else {
                warnings <- c(warnings, sprintf(tr()$age_warn_maternelle_young, i))
              }
            }
          }
          if (!is.null(horaire) && (horaire == "17h00" || horaire == "18h00")) {
            age_3_cutoff <- as.Date(paste0(school_year - 3, "-09-01"))
            if (birth_date_obj >= age_3_cutoff) {
              warnings <- c(warnings, tr()$age_warn_horaire)
            }
          }
        }
      } else if (!is.null(classe) && classe != "" && !is.null(horaire) &&
                 (horaire == "17h00" || horaire == "18h00")) {
        warnings <- c(warnings, tr()$age_warn_birthdate)
      }
    }
    if (length(warnings) > 0) {
      return(paste(warnings, collapse = " "))
    }
    return("")
  })
  
  output$income_input <- renderUI({
    if (isTRUE(input$low_income)) {
      fluidRow(
        column(8, numericInput("family_income", tr()$income_label, value = 0, min = 0, step = 1, width = "100%")),
        column(4, p("€", style = "margin-top: 25px;")),
        column(12, textOutput("income_warning"), style = "color: red; font-size: small; margin-top: 5px;")
      )
    }
  })

  output$income_warning <- renderText({
    if (isTRUE(input$low_income)) {
      if (is.null(input$family_income) || input$family_income == 0) {
        return(tr()$income_warning_zero)
      } else if (input$family_income < 0) {
        return(tr()$income_warning_negative)
      }
    }
    return("")
  })

  output$income_footnote <- renderUI({
    if (isTRUE(input$low_income)) {
      tagList(
        p(tr()$income_footnote, style = "font-size: small; color: gray; margin-top: 5px;"),
        p(tags$a(href = "https://ecole.de/wp-content/uploads/2026/01/Berechnungsbogen-zur-Selbsteinschatzung.pdf",
                target = "_blank", tr()$income_barème_link),
          style = "font-size: small; color: gray; margin-top: 2px;")
      )
    }
  })

  output$indicatif_footnote <- renderUI({
    summary <- child_data()
    if (nrow(summary) > 0) {
      p(HTML(paste0(tr()$indicatif_footnote, "<br/><span style=\"margin-left: 1.5em; display: block;\">• ", tr()$indicatif_cantine, "</span><span style=\"margin-left: 1.5em; display: block;\">• ", tr()$indicatif_inscription, "</span><span style=\"margin-left: 1.5em; display: block;\">• ", tr()$indicatif_cotisation, "</span>")), style = "font-size: small; color: gray;")
    }
  })

  output$extra_fee_footnote <- renderUI({
    summary <- child_data()
    if (nrow(summary) > 0) {
      has_extra_fee <- FALSE
      for (i in 1:child_count()) {
        classe <- input[[paste0("child_class_", i)]]
        birth_date <- input[[paste0("child_birthdate_", i)]]
        if (!is.null(classe) && classe == CLASS_MAT && !is.null(birth_date) && !is.na(birth_date)) {
          birth_date_obj <- if (inherits(birth_date, "Date")) {
            birth_date
          } else {
            tryCatch(as.Date(birth_date), error = function(e) { NA })
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
        p(tr()$extra_fee_footnote, style = "font-size: small; color: gray;")
      }
    }
  })
  
  # Conserver les données existantes et calculer les totaux
  # Updates 1500ms after user stops typing in any writable field (name, income, etc.)
  observe({
      calc_trigger_debounced()  # depend on debounced trigger
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
        # Name is in refresh scope again (no isolate)
        name <- input[[paste0("child_name_", i)]]
      
      if (!is.null(classe) && classe != "" && !is.null(horaire)) {
        # Parse birth date
        birth_date_obj <- NULL
        if (!is.null(birth_date) && !is.na(birth_date)) {
          birth_date_obj <- if (inherits(birth_date, "Date")) {
            birth_date
          } else {
            tryCatch(as.Date(birth_date), error = function(e) { NA })
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
        
        # Prix de l'horaire (garderie)
        tarif <- garderie[[classe]]
        col_name <- ifelse(vacances, 
                           ifelse(isTRUE(input$low_income), "reduc_vac", "vac"),
                           ifelse(isTRUE(input$low_income), "reduc", "norm"))
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
        if (classe == CLASS_ELEM) {
          prix_classe <- prix_classe * reduction
          prix_horaire <- prix_horaire * reduction
        } else {
          prix_classe <- prix_classe * reduction
        }
        if (isTRUE(input$low_income)) {
          family_income_value <- input$family_income %||% 0
          income_limit <- family_income_value / 12 * 0.05
          if (classe == CLASS_ELEM && prix_classe > income_limit) {
            prix_classe <- income_limit
          }
        }
        
        # Extra fee for kids born after 31/12/(school_year-3) in maternelle (75€ per month)
        # This is added AFTER the reduction is applied
        has_extra_fee <- FALSE
        if (classe == CLASS_MAT && !is.null(birth_date_obj) && !is.na(birth_date_obj)) {
          cutoff_date <- as.Date(paste0(school_year - 3, "-12-31"))
          comparison <- birth_date_obj > cutoff_date
          if (!is.na(comparison) && comparison) {
            # 75€ per month - add to ecolage AFTER reduction
            prix_classe <- prix_classe + extra_fee_young_maternelle
            has_extra_fee <- TRUE
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
  
  output$monthly_table <- renderTable({
    summary <- child_data()
    t <- tr()
    if (nrow(summary) > 0) {
      class_display <- ifelse(summary$Classe == CLASS_ELEM, t$class_elementary, ifelse(summary$Classe == CLASS_MAT, t$class_maternelle, summary$Classe))
      vac_display <- ifelse(summary$Vacances == "Oui", t$vacation_yes, t$vacation_no)
      ecolage_display <- ifelse(
        summary$HasExtraFee,
        paste0(round(summary$Écolage, 0), " €***"),
        paste0(round(summary$Écolage, 0), " €")
      )
      monthly_summary <- data.frame(
        summary$Nom,
        class_display,
        summary$Horaire,
        vac_display,
        ecolage_display,
        paste0(round(summary$Garderie, 0), " €"),
        paste0(round(summary$Écolage + summary$Garderie, 0), " €"),
        stringsAsFactors = FALSE
      )
      names(monthly_summary) <- c(t$col_nom, t$col_classe, t$col_horaire, t$col_vacances, t$ecolage, t$garderie, t$total_monthly)
      total_ecolage <- round(sum(summary$Écolage, na.rm = TRUE), 0)
      total_garderie <- round(sum(summary$Garderie, na.rm = TRUE), 0)
      total_monthly <- total_ecolage + total_garderie
      total_row <- data.frame(
        t$total_monthly, "", "", "",
        paste0(total_ecolage, " €*"),
        paste0(total_garderie, " €*"),
        paste0(total_monthly, " €*"),
        stringsAsFactors = FALSE
      )
      names(total_row) <- names(monthly_summary)
      monthly_summary <- rbind(monthly_summary, total_row)
      monthly_summary
    } else {
      NULL
    }
  })
  
  output$yearly_table <- renderTable({
    if (!SHOW_INSCRIPTION_COSTS) return(NULL)
    summary <- child_data()
    t <- tr()
    if (nrow(summary) > 0) {
      class_display <- ifelse(summary$Classe == CLASS_ELEM, t$class_elementary, ifelse(summary$Classe == CLASS_MAT, t$class_maternelle, summary$Classe))
      vac_display <- ifelse(summary$Vacances == "Oui", t$vacation_yes, t$vacation_no)
      ecolage_yearly_display <- ifelse(
        summary$HasExtraFee,
        paste0(round(summary$Écolage * 12, 0), " €***"),
        paste0(round(summary$Écolage * 12, 0), " €")
      )
      if (SHOW_INSCRIPTION_COSTS) {
        yearly_summary <- data.frame(
          summary$Nom, class_display, summary$Horaire, vac_display,
          ecolage_yearly_display,
          paste0(round(summary$Garderie * 12, 0), " €"),
          paste0(round(summary$InscriptionCost, 0), " €"),
          paste0(round(summary$Écolage * 12 + summary$Garderie * 12 + summary$InscriptionCost, 0), " €"),
          stringsAsFactors = FALSE
        )
        names(yearly_summary) <- c(t$col_nom, t$col_classe, t$col_horaire, t$col_vacances, t$ecolage, t$garderie, t$inscription, t$total_annual)
        total_ecolage_yearly <- round(sum(summary$Écolage, na.rm = TRUE) * 12, 0)
        total_garderie_yearly <- round(sum(summary$Garderie, na.rm = TRUE) * 12, 0)
        total_inscription <- round(sum(summary$InscriptionCost, na.rm = TRUE), 0)
        total_yearly <- total_ecolage_yearly + total_garderie_yearly + total_inscription
        total_row <- setNames(data.frame(t$total_annual, "", "", "", paste0(total_ecolage_yearly, " €"), paste0(total_garderie_yearly, " €"), paste0(total_inscription, " €"), paste0(total_yearly, " €*"), stringsAsFactors = FALSE), names(yearly_summary))
      } else {
        yearly_summary <- data.frame(
          summary$Nom, class_display, summary$Horaire, vac_display,
          ecolage_yearly_display,
          paste0(round(summary$Garderie * 12, 0), " €"),
          paste0(round(summary$Écolage * 12 + summary$Garderie * 12, 0), " €"),
          stringsAsFactors = FALSE
        )
        names(yearly_summary) <- c(t$col_nom, t$col_classe, t$col_horaire, t$col_vacances, t$ecolage, t$garderie, t$total_annual)
        total_ecolage_yearly <- round(sum(summary$Écolage, na.rm = TRUE) * 12, 0)
        total_garderie_yearly <- round(sum(summary$Garderie, na.rm = TRUE) * 12, 0)
        total_yearly <- total_ecolage_yearly + total_garderie_yearly
        total_row <- setNames(data.frame(t$total_annual, "", "", "", paste0(total_ecolage_yearly, " €"), paste0(total_garderie_yearly, " €"), paste0(total_yearly, " €*"), stringsAsFactors = FALSE), names(yearly_summary))
      }
      rbind(yearly_summary, total_row)
    } else {
      NULL
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
