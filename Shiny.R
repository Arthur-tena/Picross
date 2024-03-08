library(shiny)
library(shinythemes)

ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Grille Cliquable"),
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
          actionButton('replay', "Rejouer")
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
        img(src="./Images/rules_02.jpg",width=25,height=25),
        br(),
        h3('Passer en mode "hypothèse"'),
        h5("Il se peut qu'à un moment donné vous soyez bloqué(e), vous ne savez plus quelles cases noircir. Vous pouvez alors passer en mode hypothèse. Ce mode modifie la couleur des cases que vous allez noircir et éliminer afin de facilement les repérer si vous vous trompez par la suite.
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
  )
)


server <- function(input, output) {
  cases_cliquees <- reactiveVal(integer(0))
  indices_cliques <- reactiveVal(list())
  
  observe({
    if (!is.null(input$taille)) {
      output$grid <- renderUI({
        grid <- matrix(
          # Créer chaque case cliquable
          lapply(1:(input$taille^2), function(i) {
            id <- paste0("button_", floor((i - 1) / input$taille) + 1, "_", (i - 1) %% input$taille + 1)
            actionButton(inputId = id, label = "", 
                         style = if (id %in% indices_cliques()) "width: 25px; height: 25px; margin: 1px; background-color: black;" else "width: 25px; height: 25px; margin: 1px;")
          }),
          nrow = input$taille, ncol = input$taille, byrow = TRUE
        )
        # Convertir la matrice en liste pour l'affichage
        grid_list <- lapply(1:input$taille, function(i) {
          fluidRow(do.call(tagList, grid[i, ]))
        })
        do.call(tagList, grid_list)
      })
    }
  })
  
  observe(true_matrice<-picross_grid(input$taille,0.5,0.5))
  #reactive(
  #  your_matrice<-matrix(0,nrow = input$taille,ncol = input$taille)
  #  )
  
  observeEvent((input$button_1_1),{
    print(("Message"))
    #print(paste(input$grid))
  })
  ## idée: créer une liste réactive, stocker les indices cliqués dedans, pour chaque indice cliqué dans la
  ## liste on affecte la valeur 1 à your_matrice et le background-color black au boutton
  #your_matrice<-reactiveVal(matrix(0,nrow = input$taille,ncol = input$taille))
  
  
  
  observe({
    true_matrice<-picross_grid(input$taille,0.5,0.5)  
    modif_matrice <- function(i,j,val){
      if(!is.null(your_matrice)){
        mat<-your_matrice()
        mat[i,j]<-val
        your_matrice(mat)
      }
    }
    your_matrice<-reactiveVal(matrix(0,nrow = input$taille,ncol = input$taille))
    lapply(1:(input$taille), function(i){
      lapply(1:(input$taille),function(j){
        observeEvent(input[[paste0("button_",i,"_",j)]],{
          print(paste0(i,j))
          #case<-
          indices_cliques(c(indices_cliques(),paste0("button_",i,"_",j)))
          modif_matrice(i,j,1)
          print(typeof(your_matrice))
          mat<-your_matrice()
          print(mat)
          print(true_matrice)
          
        })
      })
    }) 
  })
  
  
  
}

shinyApp(ui, server)
