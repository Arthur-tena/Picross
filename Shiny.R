library(shiny)
library(shinythemes)
library(emojifont)
library(ggplot2)
library(timeR)

ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Picross"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Jeu", value = "jeu"),
        tabPanel("Règles", value = "regles") 
      ),
      conditionalPanel(
        condition = 'input.dataset === "jeu"',
        fluidRow(
          selectInput(inputId = "diff1",
                      label = "Difficulté:",
                      choices = c("Facile", "Normal", "Difficile", "Expert")),
          br(),
          sliderInput("taille", "Taille:",
                      min = 5, max = 20,
                      value = 5, step = 1),
          br(),
          actionButton('replay', "Rejouer", icon("refresh")),
          hr(),
          radioButtons('hyp', " Passer en mode hypothèse :", c("Oui", "Non"), selected="Non")
        )
      ),
      conditionalPanel(
        condition = 'input.dataset === "regles"',
        h2("Règles du jeu:"),
        hr(),
        h5("Afin de résoudre des grilles de picross, il vous faut tout d'abord connaitre les règles du jeu. Une fois ces règles assimilées, des heures de jeu et de réflexion vous attendent !"),
        h3("But du jeu:"),
        h5("Le but d'un ", strong("Picross")," est de noircir les cases de la grille afin de faire apparaître une image, un dessin. Les nombres à gauche et au-dessus de la grille sont là pour vous aider à déduire les cases à noircir."),
        hr(),
        h5("La séquence 3 2 signifie qu'il y a au moins une case vide entre une séquence de trois cases à noircir et une autre séquence de deux cases à noircir."),
        img("../Images/rules_02.jpg"),
        br(),
        h3('Passer en mode "hypothèse"'),
        h5("Il se peut qu'à un moment donné vous soyez bloqué(e), vous ne savez plus quelles cases noircir. Vous pouvez alors passer en mode hypothèse. Ce mode mets des petits chameaux sur les cases que vous allez noircir et éliminer afin de facilement les repérer si vous vous trompez par la suite.
Ce mode vous permet de partir d'une hypothèse afin de progresser dans la résolution du", strong("Picross"), "et de pouvoir revenir en arrière.")
      )
    ),
    mainPanel(tabsetPanel(tabPanel("Jeu",
                                   fluidRow(
                                     column(12,
                                            uiOutput("grid"),  # Utilisation de la fonction uiOutput pour afficher la grille
                                            verbatimTextOutput("cliquees_list")
                                     ))),
                          tabPanel("Statistiques", "Il y aura les stats ici")
    )
    )
  ))

server <- function(input, output) {
  
  cases_cliquees <- reactiveVal(integer(0))
  indices_cliques <- reactiveVal(list())
  mode_hypothese <- reactiveVal(FALSE)
  indices_hyp <- reactiveVal(list())
  
  observe({
    if (!is.null(input$taille)) {
      observe({
        lapply(1:(input$taille^2), function(i) {
          observeEvent(input[[paste0("button_", i)]], {
            if (input[[paste0("button_", i)]] > 0) {
              if (mode_hypothese()) {
                indices_hyp(c(indices_hyp(), i))
              } else {
                indices_cliques(c(indices_cliques(), i))
              }
            }
          })
        })
      })
      
      observeEvent(input$hyp, {
        mode_hypothese(input$hyp == "Oui")
      })
      if (input$taille >= 10) {
        output$grid <- renderUI({
          grid <- matrix(
            # Créer chaque case cliquable
            lapply(1:(input$taille^2), function(i) {
              if (i %in% indices_hyp()) {
                actionButton(inputId = paste0("button_", i), label = emoji('camel'), class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              } else if (i %in% indices_cliques()) {
                actionButton(inputId = paste0("button_", i), label = "", style = "background-color: black;", class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              } else {
                actionButton(inputId = paste0("button_", i), label = "", style = "", class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              }
            }),
            nrow = input$taille, ncol = input$taille, byrow = TRUE
          )
          # Mettre en forme la matrice en liste
          grid_list <- lapply(1:input$taille, function(i) {
            fluidRow(do.call(tagList, grid[i, ]))
          })
          do.call(tagList, grid_list)
        })
      } else {
        output$grid <- renderUI({
          grid <- matrix(
            # Créer chaque case cliquable
            lapply(1:(input$taille^2), function(i) {
              if (i %in% indices_hyp()) {
                actionButton(inputId = paste0("button_", i), label = emoji('camel'), class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              } else if (i %in% indices_cliques()) {
                actionButton(inputId = paste0("button_", i), label = "", style = "background-color: black;", class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              } else {
                actionButton(inputId = paste0("button_", i), label = "", style = "", class = c("btn-sn", if(i %in% cases_cliquees()) "case-cliquee" else ""))
              }
            }),
            nrow = input$taille, ncol = input$taille, byrow = TRUE
          )
          # Mettre en forme la matrice en liste
          grid_list <- lapply(1:input$taille, function(i) {
            fluidRow(do.call(tagList, grid[i, ]))
          })
          do.call(tagList, grid_list)
        })
      }
    }
  })
  
  # Observer pour réagir aux clics sur les boutons
  observeEvent(input$grid, {
    clicked_button <- as.numeric(substr(input$grid, 8, nchar(input$grid)))
    
    # Afficher un message pour vérifier que l'événement est détecté
    print(paste("Clic sur le bouton", clicked_button))
    
    # Faire quelque chose avec le bouton cliqué
    print(paste("Bouton", clicked_button, "cliqué !"))
    
    # Afficher la classe actuelle du bouton
    current_class <- input[[paste0("button_", clicked_button)]]
    print(paste("Classe actuelle:", current_class))
    
    # Mettre à jour la liste des cases cliquées
    toggle_class <- function(class_list, class_name) {
      if (class_name %in% class_list) {
        class_list <- setdiff(class_list, class_name)
      } else {
        class_list <- c(class_list, class_name)
      }
      return(class_list)
    }
    
    current_classes <- toggle_class(class_list = current_class, class_name = "case-cliquee")
    
    # Afficher les classes mises à jour
    print(paste("Nouvelles classes:", current_classes))
    
    # Mettre à jour la classe du bouton
    updateActionButton(
      session = getDefaultReactiveDomain(),
      inputId = paste0("button_", clicked_button),
      class = current_classes
    )
  })
  # Observer pour afficher la liste des cases cliquées
  output$cliquees_list <- renderPrint({
    isolate(cases_cliquees())
  })
}

shinyApp(ui, server)
