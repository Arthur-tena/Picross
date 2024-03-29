library(shiny)
library(shinythemes)

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
          selectInput(
            inputId = "diff1",
            label = "Difficulté:",
            choices = c("Facile", "Normal", "Difficile", "Expert")
          ),
          br(),
          sliderInput(
            "taille",
            "Taille:",
            min = 5,
            max = 20,
            value = 5,
            step = 1
          ),
          br(),
          actionButton('replay1', "Rejouer avec la même grille"),
          actionButton('replay2', "Rejouer avec une autre grille"),
          br(),
          hr(),
          actionButton('verification','Verification'),

          hr(),
          radioButtons('hyp', " Passer en mode hypothèse :", c("Oui", "Non"), selected="Non")
        )
      ),
      conditionalPanel(
        condition = 'input.dataset === "regles"',
        h2("Règles du jeu:"),
        hr(),
        h5(
          "Afin de résoudre des grilles de picross, il vous faut tout d'abord connaitre les règles du jeu. Une fois ces règles assimilées, des heures de jeu et de réflexion vous attendent !"
        ),
        h3("But du jeu:"),
        h5(
          "Le but d'un ",
          strong("Picross"),
          " est de noircir les cases de la grille afin de faire apparaître une image, un dessin. Les nombres à gauche et au-dessus de la grille sont là pour vous aider à déduire les cases à noircir."
        ),
        hr(),
        h5(
          "La séquence 3 2 signifie qu'il y a au moins une case vide entre une séquence de trois cases à noircir et une autre séquence de deux cases à noircir."
        ),
        img(
          src = "./www/rules_02.jpg",
          width = 25,
          height = 25
        ),
        br(),
        h3('Passer en mode "hypothèse"'),
        h5(
          "Il se peut qu'à un moment donné vous soyez bloqué(e), vous ne savez plus quelles cases noircir. Vous pouvez alors passer en mode hypothèse. Ce mode modifie la couleur des cases que vous allez noircir et éliminer afin de facilement les repérer si vous vous trompez par la suite.
Ce mode vous permet de partir d'une hypothèse afin de progresser dans la résolution du",
          strong("Picross"),
          "et de pouvoir revenir en arrière."
        )
      )
    ),
    mainPanel(tabsetPanel(
      tabPanel("Jeu",
               fluidRow(
                 column(12,
                        uiOutput("grid"),  # Utilisation de la fonction uiOutput pour afficher la grille
                        verbatimTextOutput("cliquees_list"),
                        verbatimTextOutput('verif',placeholder=TRUE))
               )),
      tabPanel("Statistiques", "Il y aura les stats ici")
    ))
  )
)


server <- function(input, output) {
  source("fonction.R")
  indices_cliques <- reactiveVal(list())
  indices_hyp <- reactiveVal(list())
  mode_hypothese <- reactiveVal(FALSE) 
  
  observe({
    if(input$diff1=="Facile"){true_matrice<-picross_grid(input$taille,0.25,0.75)}
    else {if(input$diff1=="Normal"){true_matrice<-picross_grid(input$taille,0.5,0.5)}
      else { if(input$diff1=="Difficile"){true_matrice <- picross_grid(input$taille,0.6,0.4)}
        else {true_matrice<-picross_grid(input$taille,0.75,0.25)}}}
    
    # your_matrice <- reactiveVal(matrix(0, nrow = input$taille, ncol = input$taille))
    # mat<-your_matrice()
    
    decallage<-floor(input$taille/2)+1
    taille<-input$taille+decallage
    if (!is.null(input$taille)) {
      output$grid <- renderUI({
        
        grid <- matrix(
          # Créer chaque case cliquable
          lapply(1:(taille ^ 2), function(i) {
            ligne <- floor((i - 1) / taille) + 1
            colonne <- (i - 1) %% taille + 1
            zone_morte <- ((ligne %in% 1:decallage && colonne %in% 1:decallage))
            zone_ligne <- (ligne %in% (decallage+1):taille && colonne %in% 1:decallage)
            id <- paste0("button_", ligne, "_", colonne)
            zone_colonne <- (colonne %in% (decallage+1):taille && ligne %in% 1:decallage)
            
            actionButton(
              inputId = id,
              label = if(zone_ligne){count1row(ligne-decallage,true_matrice)[colonne]}
              else {if(zone_colonne){count1col(colonne-decallage,true_matrice)[ligne]} else ""},
              
              # label = if(zone_ligne){valeurs[colonne]}
              # else {if(zone_colonne){valeurs[ligne]} else ""},
              style = if (id %in% indices_cliques()){
                #style = if (mat[ligne-decallage,colonne-decallage]==1) {
                "width: 25px; height: 25px; margin: 0px; padding:0px; background-color: black;"
              } 
              else if (id %in% indices_hyp()){
                "width: 25px; height: 25px; margin: 0px; padding:0px; background-color: blue;"
              }
              else {
                if (((ligne %in% (decallage + 1):taille && colonne %in% 1:decallage) ||
                     (ligne %in% 1:decallage && colonne %in% (decallage + 1):taille))) {
                  "width: 25px; height: 25px; margin: 0px; padding: 0px; text-align: center; border: none;"
                } else {
                  if (zone_morte) {
                    "width: 25px; height: 25px; margin: 0px; padding:0px; border: none;"
                  } else "width: 25px; height: 25px; margin: 0px; padding:0px;"
                }
              }
              #tags$style(HTML(paste0("#",id," {border-radius: 0;}")))
            )
          }),
          nrow = taille,
          ncol = taille,
          byrow = TRUE
        )
        
        # Convertir la matrice en liste pour l'affichage
        grid_list <- lapply(1:taille, function(i) {
          fluidRow(do.call(tagList, grid[i, ]))
        })
        do.call(tagList, grid_list)
      })
      
    }
    modif_matrice <- function(i, j, val) {
      if (!is.null(your_matrice)) {
        mat <- your_matrice()
        mat[i, j] <- val
        your_matrice(mat)
      }
    }
    
    removeElement <- function(elementToRemove) {
      currentList <- indices_cliques()
      updatedList <- currentList[currentList != elementToRemove]
      indices_cliques(updatedList)
    }
    removeHyp <- function(elementToRemove) {
      currentList <- indices_hyp()
      updatedList <- currentList[currentList != elementToRemove]
      indices_hyp(updatedList)
    }
    
    observeEvent(input$hyp, {
      mode_hypothese(input$hyp == "Oui")
    })
    
    your_matrice <-
      reactiveVal(matrix(0, nrow = input$taille, ncol = input$taille))
    lapply(3:(taille), function(i) {
      lapply(1:(taille), function(j) {
        observeEvent(input[[paste0("button_", i, "_", j)]], {
          id<-paste0("button_", i, "_", j)
          print(paste0(i-decallage, j-decallage))
          if(mode_hypothese()==FALSE){
          if(id %in% indices_cliques()){
            removeElement(id)
            modif_matrice(i-decallage, j-decallage, 0)
            mat <- your_matrice()
            print(mat)
          }
          else{
            indices_cliques(c(indices_cliques(), paste0("button_", i, "_", j)))
            modif_matrice(i-decallage, j-decallage, 1)
            #print(typeof(your_matrice))
            mat <- your_matrice()
            print(mat)
            print(true_matrice)
            print(mat[i-decallage,j-decallage])
          }}
          else if(mode_hypothese()){
            if(id %in% indices_hyp()){
              removeHyp(id)
              }
            else {
              indices_hyp(c(indices_hyp(), paste0("button_", i, "_", j)))
              }
          }
          }
        )
      })
    })
    observeEvent(input$verification,{
      if(comparaison(true_matrice,your_matrice())){
        output$verif<-modalDialog(title="C'est gagné !", size="m", easyClose=T)
      }
      else output$verif<-modalDialog(title="C'est Perdu, big L !", size="m", easyClose=T)
    })
  
    observeEvent(input$replay1,{
      your_matrice(matrix(0, nrow = input$taille, ncol = input$taille))
      indices_cliques(list())
      indices_hyp(list())
    })
    
    observeEvent(input$replay2,{
      if(input$diff1=="Facile"){true_matrice<-picross_grid(input$taille,0.25,0.75)}
        else {if(input$diff1=="Normal"){true_matrice<-picross_grid(input$taille,0.5,0.5)}
          else { if(input$diff1=="Difficile"){true_matrice <- picross_grid(input$taille,0.6,0.4)}
            else {true_matrice<-picross_grid(input$taille,0.75,0.25)}}}
        your_matrice(matrix(0, nrow = input$taille, ncol = input$taille))
        indices_cliques(list())
        indices_hyp(list())
        print(true_matrice)
        
        output$grid <- renderUI({
          grid <- matrix(
            lapply(1:(taille ^ 2), function(i) {
              ligne <- floor((i - 1) / taille) + 1
              colonne <- (i - 1) %% taille + 1
              zone_morte <- ((ligne %in% 1:decallage && colonne %in% 1:decallage))
              zone_ligne <- (ligne %in% (decallage+1):taille && colonne %in% 1:decallage)
              id <- paste0("button_", ligne, "_", colonne)
              zone_colonne <- (colonne %in% (decallage+1):taille && ligne %in% 1:decallage)
              
              actionButton(
                inputId = id,
                label = if(zone_ligne) { count1row(ligne - decallage, true_matrice)[colonne] }
                else if(zone_colonne) { count1col(colonne - decallage, true_matrice)[ligne] }
                else { "" },
                style = if (id %in% indices_cliques()) {
                  "width: 25px; height: 25px; margin: 0px; padding:0px; background-color: black;"
                } else if (id %in% indices_hyp()) {
                  "width: 25px; height: 25px; margin: 0px; padding:0px; background-color: blue;"
                } else {
                  if (((ligne %in% (decallage + 1):taille && colonne %in% 1:decallage) ||
                       (ligne %in% 1:decallage && colonne %in% (decallage + 1):taille))) {
                    "width: 25px; height: 25px; margin: 0px; padding: 0px; text-align: center; border: none;"
                  } else {
                    if (zone_morte) {
                      "width: 25px; height: 25px; margin: 0px; padding:0px; border: none;"
                    } else "width: 25px; height: 25px; margin: 0px; padding:0px;"
                  }
                }
              )
            }),
            nrow = taille,
            ncol = taille,
            byrow = TRUE
          )
          
          grid_list <- lapply(1:taille, function(i) {
            fluidRow(do.call(tagList, grid[i, ]))
          })
          do.call(tagList, grid_list)
        })
      })
      
    
  })

  
  ## idée: créer une liste réactive, stocker les indices cliqués dedans, pour chaque indice cliqué dans la
  ## liste on affecte la valeur 1 à your_matrice et le background-color black au boutton
  #your_matrice<-reactiveVal(matrix(0,nrow = input$taille,ncol = input$taille))
  
  
  ## à la ligne decallage dans la colonne 1 on stocke la première valeur du vecteur count1row
  ##                                      2 on stocke la deuxième valeur...
  ## ainsi de suite jusqu'à la colonne decallage
  ## --> traiter séparemment les lignes et les colonnes, par ex pour les lignes on peut accéder à la ligne
  ## et attribuer la valeur numéro 'colonne' de count1row. Faire une "zone ligne" et une "zone colonne"
  ## donc modifier count1row et count1col pour remplir le vecteur de "" de sorte à atteindre la taille decallage
  ## zone_ligne <- (ligne %in% (decallage+1):taille && colonne %in% 1:decallage) 
  ## zone_colonne <- (colonne %in% (decallage+1):taille && ligne %in% 1:decallage)
  
  
}

picross=function(){
  shinyApp(ui, server)
}

picross()
