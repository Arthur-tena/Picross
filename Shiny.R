library(shiny)
library(shinythemes)
library(emojifont)

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
          actionButton('replay', "Rejouer", icon("refresh"))
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
    mainPanel(tabsetPanel(
      tabPanel("Jeu", h2("Jeu du PICROSS")),
      tabPanel("Statistiques", checkboxGroupInput("show_vars", "Stats à regarder:"),"Il y aura les stats ici")
    ),
    plotOutput("grille", width = "600px" , height="400px")
  )
))

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
