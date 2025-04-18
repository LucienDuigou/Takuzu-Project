
#' Nombre aleatoire entre 0 et 1
#' 
#' Cette fonction renvoie un entier aleatoire entre 0 et 1
#' @return un entier entre 0 et 1
#' @examples
#' rand()
#' @export
#' 
rand = function(){
  x=runif(1,0,1)
  if (x<0.5){
    x=0
  }
  else {
    x=1
  }
  return(x)
}

#' Verification de ligne 
#' 
#' Cette fonction verifie si une ligne ou colonne respecte les regles : pas plus de 3 zeros ou 3 uns d'affiles et exactement 4 zeros et 4 uns
#' @param l un vecteur de taille 8
#' @return un booleen
#' @examples
#' verif_line(c(1,1,0,0,1,1,0,0))
#' @export
verif_line = function(l){
  if (l[1] != 'NaN'){
    if (l[1]==0){
      n0=1
      n1=0
    }
    else {
      n0=0
      n1=1
    }
  }
  else {
    n0=0
    n1=0
  }
  for (i in 2:8){
    if (l[i] != 'NaN'){
      if (l[i]==0){
        if (l[i-1]==1){
          n1=0
        }
        n0=n0+1
        if (n0==3){
          return(FALSE)
        }
      }
      else {
        if(l[i-1]==0){
          n0=0
        }
        n1=n1+1
        if (n1==3){
          return(FALSE)
        }
      }
    }
  }
  n0=0
  n1=0
  for (i in 1:8){
    if (l[i] !='NaN'){
      if (l[i]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
    }
  }
  if (n0>4){
    return(FALSE)
  }
  if (n1>4){
    return(FALSE)
  }
  if(n0==4){
    if(n1==4){
      return(TRUE)
    }
  }
  return(TRUE)
}


#' Verification de ligne 
#' 
#' Cette fonction verifie si une ligne ou colonne respecte les regles pas plus de 3 zeros ou 3 uns d'affiles et exactement 4 zeros et 4 uns
#' @param l un vecteur de taille 8
#' @return un booleen
#' @examples
#' verif_line(c(1,1,0,0,1,1,0,0))
#' @export
verif_line2 = function(l){
  if (l[1] != 'NaN'){
    if (l[1]==0){
      n0=1
      n1=0
    }
    else {
      n0=0
      n1=1
    }
  }
  else {
    n0=0
    n1=0
  }
  for (i in 2:8){
    if (l[i] != 'NaN'){
      if (l[i]==0){
        if (l[i-1]==1){
          n1=0
        }
        n0=n0+1
        if (n0==3){
          return(FALSE)
          break
        }
      }
      else {
        if(l[i-1]==0){
          n0=0
        }
        n1=n1+1
        if (n1==3){
          return(FALSE)
          break
        }
      }
    }
  }
  n0=0
  n1=0
  for (i in 1:8){
    if (l[i] !='NaN'){
      if (l[i]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
    }
  }
  if (n0>4){
    return(FALSE)
  }
  if (n1>4){
    return(FALSE)
  }
  if(n0==4){
    if(n1==4){
      return(TRUE)
    }
  }
  return(FALSE)
}


#' creer une ligne
#' 
#' Cette fonction remplie un vecteur de taille 8 contenant des 0,des 1 et des NaN. Elle cherche a remplacer les NaN par des 0 ou 1 qui respectent les regles. Si la configuration de line ne permet pas le respect de celles-ci, elle renvoie FALSE. Sinon, elle renvoie le vecteur remplie avec un TRUE
#' @param line un vecteur de taille 8 contenant des nombres 0 et 1 et des NaN
#' @return un vecteur de taille 8 et un booleen
#' @examples
#' create_line2(c(1,1,0,0,NaN,NaN,NaN,NaN))
#' @export
create_line2=function(line){
  c=0
  n0=0
  n1=0
  for (i in 1:8){
    if (line[i]!='NaN') {
      c=c+1
      if (line[i]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
    }
  }
  time_limit=0.05
  start_time=Sys.time()
  test=FALSE
  while(test==FALSE){
    if (c==0){
      line[1]=rand()
      if (line[1]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
      line[2]=rand()
      if (line[2]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
    }
    if (c==1){
      line[2]=rand()
      if (line[2]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
    }
    for (i in (max(3,c+1):8)){
      t=0
      line[i]=rand()
      if (line[i]==0){
        if (line[i-1]==0){
          if (line[i-2]==0){
            t=1
          }
        }
      }
      if (line[i]==1){
        if (line[i-1]==1){
          if (line[i-2]==1){
            line[i]=0
          }
        }
      }
      line[i]=line[i]+t
      if (line[i]==0){
        n0=n0+1
      }
      else {
        n1=n1+1
      }
      if (n0>4){
        line[i]=1
      }
      if (n1>4){
        line[i]=0
      }
    }
    test=verif_line(line)
    elapsed_time=as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed_time > time_limit){
      return(list(FALSE,FALSE))
      break
    }
  }
  return(list(line,TRUE))
}

#' Creer une ligne
#' 
#' Cette fonction remplie un vecteur de taille 8 contenant des 0, des 1 et des NaN. Tant que la fonction create_line2 renvoie FALSE, on la refait jusqu'a avoir une ligne correcte
#' @param line un vecteur de taille 8 contenant des 0, des 1 et des NaN
#' @examples
#' create_line3(c(1,1,0,0,NaN,NaN,NaN,NaN))
#' @export
create_line3=function(line){
  test=FALSE
  while (test==FALSE){
    t=create_line2(line)
    newline=t[[1]]
    if (t[[2]]==TRUE){
      break
    }
  }
  return(newline)
}


#' Creer une grille
#' 
#' Cette fonction cree une grille de taille 8x8 respectant les regles : pas plus de 3 zeros ou 3 uns d'affiles et exactement 4 zeros et 4 uns pour chaque ligne et chaque colonne. Si a un moment donne la configuration de la grille ne permet pas de respecter ces regles, le code s'arrete avec un FALSE. Sinon, elle renvoie la grille complete
#' @return une grille de taille 8x8 ou un bouleen FALSE
#' @examples
#' create_game1()
#' @export
create_game1=function(){
  M=rep(NaN,64)
  M=matrix(M,ncol=8,byrow=TRUE)
  M[1,]=create_line3(M[1,])
  M[,1]=create_line3(M[,1])
  M[2,]=create_line3(M[2,])
  M[,2]=create_line3(M[,2])
  for (i in 3:8){
    for(j in 3:8){
      n0=0
      n1=0
      m0=0
      m1=0
      for (t in 1:(i-1)){
        if (M[t,j]==0){
          n0=n0+1
        }
        else {
          n1=n1+1
        }
      }
      for (t in 1:(j-1)){
        if (M[i,t]==0){
          m0=m0+1
        }
        else {
          m1=m1+1
        }
      }
      test1=FALSE
      test2=FALSE
      start_time = Sys.time()
      time_limit=0.1
      elapsed_time=0
      while (test1==FALSE | test2==FALSE){
          t=0
          M[i,j]=rand()
          if (M[i,j]==0){
            if (M[i,j-1]==0){
              if (M[i,j-2]==0){
                t=1
              }
            }
            if (M[i-1,j]==0){
              if (M[i-2,j]==0){
                t=1
              }
            }
          }
          if (M[i,j]==1){
            if (M[i,j-1]==1){
              if(M[i,j-2]==1){
                M[i,j]=0
              }
            }
            if (M[i-1,j]==1){
              if (M[i-2,j]==1){
                M[i,j]=0
              }
            }
          }
          M[i,j]=M[i,j]+t 
          if (M[i,j]==0){
            n0=n0+1
            m0=m0+1
          }
          else {
            n1=n1+1
            m1=m1+1
          }
          if (n0>4 | m0>4){
            M[i,j]=1
          }
          if (n1>4 | m1>4){
            M[i,j]=0
          }
          test1=verif_line(M[i,])
          test2=verif_line(M[,j])
          elapsed_time=as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          if (elapsed_time>time_limit){
            return(M)
            break
          }
      }
    }
  }
  return(M)
}

#' Creer une grille
#' 
#' Cette fonction renvoie une grille de taille 8x8 respectant les 2 premieres regles. Tant que la fonction create_game1 renvoie False, on continue jusqu'a obtenir une grille correcte
#' @return une grille de taille 8x8
#' @examples
#' create_game3()
#' @export
create_game3=function(){
  G=create_game1()
  c=0
  for (i in 1:8){
    for (j in 1:8){
      if (G[i,j]== 'NaN'){
        c=c+1
      }
    }
  }
  test1=verif_line(G[8,])
  test2=verif_line(G[,8])
  while (c>0 | (test1==FALSE | test2==FALSE)){
    c=0
    G=create_game1()
    for (i in 1:8){
      for (j in 1:8){
        if (G[i,j]== 'NaN'){
          c=c+1
        }
      }
    }
    test1=verif_line(G[8,])
    test2=verif_line(G[,8])
  }
  return(G)
}

#' Verifier une grille
#' 
#' Cette fonction verifie si une grille de taille 8x8 repectent la troisieme regle : pas de lignes identiques et pas de colonnes identiques
#' @param M une grille de taille 8x8 contenant des 0 et des 1
#' @return un Booleen
#' @export
verif_grille = function(M){
  l1=list(M[1,])
  l2=list(M[,1])
  for (i in 2:8){
    if (any(sapply(l1, function(x) identical(x, M[i,])))){
      test=FALSE
    }
    else {
      l1=append(l1,list(M[i,]))
    }
    if (any(sapply(l2, function(x) identical(x, M[,i])))){
      test=FALSE
    }
    else{
      l2=append(l2,list(M[,i]))
    }
  }
  if (length(l1)==8){
    if (length(l2)==8){
      test=TRUE
    }
  }
  return(test)
}


#' Creer une grille 
#' 
#' Cette fonction cree une grille de taille 8x8 qui respecte les 3 regles du jeu
#' @return une grille de taille 8x8
#' @examples
#' create_game()
#' @export
create_game=function(){
  test=FALSE
  while (test==FALSE){
    G=create_game3()
    test=verif_grille(G)
  }
  return(G)
}

#' Verifier une grille
#' 
#' Cette fonction verifie si une grille de taille 8x8 repectent la troisieme regle : pas de lignes identiques et pas de colonnes identiques
#' @param M une grille de taille 8x8 contenant des 0 et 1
#' @return un Booleen
#' @export
verif_game = function(M){
  for (i in 1:8){
    for (j in 1:8){
      test1 = verif_line2(M[i,])
      test2 = verif_line2(M[,j])
      if (test1 == FALSE | test2==FALSE){
        return(FALSE)
        break
      }
    }
  }
  test = verif_grille(M)
  return(test)
}

#' Enlever les chaines de caracteres
#' 
#' Cette fonction prend en argument un vecteur contenant des chaines de caracteres et remplace chaque chaine contenant un entier en cette entier sans la chaine
#' @param V vecteur de chaines de caracteres
#' @examples
#' unchained(c("1","1","0","0","NaN","NaN","NaN","NaN"))
#' @export
unchained = function(V){
  for(i in 1:length(V)){
    if (V[i]!='NaN'){
      V[i]=as.integer(V[i])
    }
  }
  return(V)
}


#' jouer au jeu
#' 
#' Cette fonction cree l'application shiny qui permet de jouer au jeu du takuzu de taille 8x8
#' @importFrom shiny fluidPage shinyApp reactiveVal observeEvent renderUI
#' @importFrom shinyWidgets show_alert
#' @importFrom shiny isolate updateActionButton modalDialog column uiOutput
#' @importFrom stats runif
#' @export
Takuzu = function(){
  data(DB_facile, package = "Takuzu")
  data(DB_moyen, package = "Takuzu")
  data(DB_difficile, package = "Takuzu")
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Le package 'shiny' est requis.")
  }
  if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
    stop("Le package 'shinyWidgets' est requis.")
  }
  
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("body {
    font-size: 20px;
  }
  .btn {
    font-size: 20px !important;
    padding: 15px 25px;
  }
  .shiny-input-container {
    font-size: 20px;
  }
  "))),
    shiny::div(style = "text-align:center;",
               shiny::titlePanel("Jeu du binary")),
    shiny::div(style = "text-align:center;",
               shiny::uiOutput("page_content"))
  )

  server <- function(input, output, session) {
    current_page = shiny::reactiveVal("Accueil")
    J = shiny::reactiveValues(vect = c())
    J_ = shiny::reactiveValues(vect = c())
    B = shiny::reactiveValues(vect = c())
    start_time = shiny::reactiveValues(vect = c())

    observeEvent(input$bouton_jeu, {
      if (current_page() == 'Accueil') {
        current_page('choix_difficulte')
        output$page_content = shiny::renderUI({
          shiny::div(style = "text-align:center;",
                     shiny::actionButton("retour_accueil", "Retour à la page d'accueil", style = "color: black; background-color: #FFC0CB; border-color: #9C767C"),
                     shiny::br(), shiny::br(),
                     shiny::actionButton("bouton_difficulte1", "Facile", style = "color: black; background-color: #2EA045; border-color: #15481F"),
                     shiny::actionButton("bouton_difficulte2", "Moyen", style = "color: black; background-color: #FFA500; border-color: #B57500"),
                     shiny::actionButton("bouton_difficulte3", "Difficile", style = "color: black; background-color: #F34A4A; border-color: #AC3434")
          )
        })
      }
    })
  
    observeEvent(input$retour_accueil, {
      current_page('Accueil')
      output$page_content = shiny::renderUI({
        shiny::div(style = "text-align:center;",
                   shiny::p('Accueil'),
                   shiny::actionButton("bouton_jeu", "Jouer une partie", style = "color: black; background-color: #86D3F7; border-color: #497285")
        )
      })
    })
  
  observeEvent(input$bouton_difficulte2, {
    B$vect=c()
    current_page('jeu_moyen')
    J$vect= DB_moyen[sample(1:20, 1)][[1]]
    J_$vect=J$vect
    G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
    output$dynamicPanel <- shiny::renderUI({
      if (verif_game(G_)) {
        shinyWidgets::show_alert(
          title = "Victoire !",
          text = "Félicitations ! Vous avez complété la grille avec succès",
          type = "success"
        )
      }
    })
    for (id in btn_ids) {
      values[[id]] <- 'rien'
      if (J$vect[as.integer(substr(id, 5, nchar(id)))] == 'NaN') {
        new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
        shiny::updateActionButton(session, id, label = new_label)
      }
    }
    
    output$page_content = shiny::renderUI({
      shiny::fluidPage(
        shiny::actionButton("retour_accueil", "retour à la page d'accueil", style = "color: black; background-color: #FFC0CB; border-color: #9C767C"),
        shiny::fluidRow(
          shiny::column(12,
                        shiny::div(
                          style = "display: grid; grid-template-columns: repeat(8, 50px); grid-gap: 5px; justify-content: center; align-items: center;",
                          lapply(1:64, function(i) {
                            if (J$vect[i] == 'NaN') {
                              shiny::actionButton(paste0("btn_", i), label = ' ', style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;")
                            } else {
                              shiny::actionButton(paste0("btn_", i), label = as.character(J$vect[i]), style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;", disabled = TRUE)
                            }
                          })
                        )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput("dynamicPanel"))
        ),
        shiny::actionButton('Back', 'Retour en arrière', style = "color: black; background-color: #F8BBD0; border-color: #AC8290"),
        shiny::actionButton('Clear', 'Recommencer', style = "color: black; background-color: #A8E6CF; border-color: #749F8F")
      )
    })
    start_time$vect = Sys.time()
  })
  
  observeEvent(input$bouton_difficulte1, {
    B$vect=c()
    current_page('jeu_facile')
    J$vect = DB_facile[sample(1:20, 1)][[1]]
    J_$vect=J$vect
    G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
    output$dynamicPanel <- shiny::renderUI({
      if (verif_game(G_)) {
        shinyWidgets::show_alert(
          title = "Victoire !",
          text = "Félicitations ! Vous avez complété la grille avec succès",
          type = "success"
        )
      }
    })
    for (id in btn_ids) {
      values[[id]] <- 'rien'
      if (J$vect[as.integer(substr(id, 5, nchar(id)))] == 'NaN') {
        new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
        shiny::updateActionButton(session, id, label = new_label)
      }
    }
    
    output$page_content = shiny::renderUI({
      shiny::fluidPage(
        shiny::actionButton("retour_accueil", "retour à la page d'accueil", style = "color: black; background-color: #FFC0CB; border-color: #9C767C"),
        shiny::fluidRow(
          shiny::column(12,
                        shiny::div(
                          style = "display: grid; grid-template-columns: repeat(8, 50px); grid-gap: 5px; justify-content: center; align-items: center;",
                          lapply(1:64, function(i) {
                            if (J$vect[i] == 'NaN') {
                              shiny::actionButton(paste0("btn_", i), label = ' ', style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;")
                            } else {
                              shiny::actionButton(paste0("btn_", i), label = as.character(J$vect[i]), style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;", disabled = TRUE)
                            }
                          })
                        )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput("dynamicPanel"))
        ),
        shiny::actionButton('Back', 'Retour en arrière', style = "color: black; background-color: #F8BBD0; border-color: #AC8290"),
        shiny::actionButton('Clear', 'Recommencer', style = "color: black; background-color: #A8E6CF; border-color: #749F8F")
      )
    })
    start_time$vect = Sys.time()
  })
  
  observeEvent(input$bouton_difficulte3, {
    B$vect=c()
    current_page('jeu_difficile')
    J$vect = DB_difficile[sample(1:20, 1)][[1]]
    J_$vect=J$vect
    G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
    output$dynamicPanel <- shiny::renderUI({
      if (verif_game(G_)) {
        shinyWidgets::show_alert(
          title = "Victoire !",
          text = "Félicitations ! Vous avez complété la grille avec succès",
          type = "success"
        )
      }
    })
    for (id in btn_ids) {
      values[[id]] <- 'rien'
      if (J$vect[as.integer(substr(id, 5, nchar(id)))] == 'NaN') {
        new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
        shiny::updateActionButton(session, id, label = new_label)
      }
    }
    
    output$page_content = shiny::renderUI({
      shiny::fluidPage(
        shiny::actionButton("retour_accueil", "retour à la page d'accueil", style = "color: black; background-color: #FFC0CB; border-color: #9C767C"),
        shiny::fluidRow(
          shiny::column(12,
                        shiny::div(
                          style = "display: grid; grid-template-columns: repeat(8, 50px); grid-gap: 5px; justify-content: center; align-items: center;",
                          lapply(1:64, function(i) {
                            if (J$vect[i] == 'NaN') {
                              shiny::actionButton(paste0("btn_", i), label = ' ', style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;")
                            } else {
                              shiny::actionButton(paste0("btn_", i), label = as.character(J$vect[i]), style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;", disabled = TRUE)
                            }
                          })
                        )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput("dynamicPanel"))
        ),
        shiny::actionButton('Back', 'Retour en arrière', style = "color: black; background-color: #F8BBD0; border-color: #AC8290"),
        shiny::actionButton('Clear', 'Recommencer', style = "color: black; background-color: #A8E6CF; border-color: #749F8F")
      )
    })
    start_time$vect = Sys.time()
  })
  
  observeEvent(input$Clear, {
    B$vect=c()
    J_$vect=J$vect
    G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
    output$dynamicPanel <- shiny::renderUI({
      if (verif_game(G_)) {
        shinyWidgets::show_alert(
          title = "Victoire !",
          text = "Félicitations ! Vous avez complété la grille avec succès",
          type = "success"
        )
      }
    })
    for (id in btn_ids) {
      values[[id]] <- 'rien'
      if (J$vect[as.integer(substr(id, 5, nchar(id)))] == 'NaN') {
        new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
        shiny::updateActionButton(session, id, label = new_label)
      }
    }
    
    output$page_content = shiny::renderUI({
      shiny::fluidPage(
        shiny::actionButton("retour_accueil", "retour à la page d'accueil"),
        shiny::fluidRow(
          shiny::column(12,
                        shiny::div(
                          style = "display: grid; grid-template-columns: repeat(8, 50px); grid-gap: 5px; justify-content: center; align-items: center;",
                          lapply(1:64, function(i) {
                            if (J$vect[i] == 'NaN') {
                              shiny::actionButton(paste0("btn_", i), label = ' ', style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;")
                            } else {
                              shiny::actionButton(paste0("btn_", i), label = as.character(J$vect[i]), style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;", disabled = TRUE)
                            }
                          })
                        )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput("dynamicPanel"))
        ),
        shiny::actionButton('Back', 'Retour en arrière', style = "color: black; background-color: #F8BBD0; border-color: #AC8290"),
        shiny::actionButton('Clear', 'Recommencer', style = "color: black; background-color: #A8E6CF; border-color: #749F8F")
      )
    })
    start_time$vect = Sys.time()
  })
  
  observeEvent(input$Back, {
    isolate({
      B_ = B$vect
      if (length(B_)>0){
        b = B_[length(B_)]
        B$vect = B_[-length(B_)]
        i = as.integer(substr(b, 5, nchar(b)))
        values[[b]]='rien'
        J_$vect[i] = NaN
        shiny::updateActionButton(session, b, label = "&nbsp;")
      
        G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
      }
    })
  })
  
  output$page_content = shiny::renderUI({
    if (current_page() == 'Accueil') {
      shiny::div(style = "text-align:center;",
                 shiny::p('Accueil'),
                 shiny::actionButton("bouton_jeu", "Jouer une partie", style = "color: black; background-color: #86D3F7; border-color: #497285"))
    }
  })
  
  
  
  btn_ids <- paste0("btn_", 1:64)
  values <- shiny::reactiveValues()
  for (id in btn_ids) {
    values[[id]] <- 'rien'
  }
  

  lapply(btn_ids, function(id) {
    shiny::observeEvent(input[[id]], {
      current_val <- values[[id]] 
      if (current_val == "rien") {
        values[[id]] <- "0"
      } else if (current_val == "0") {
        values[[id]] <- "1"
      } else {
        values[[id]] <- "rien"
      }
      
      if (values[[id]] == "rien") {
        J_$vect[as.integer(substr(id, 5, nchar(id)))] <- NaN
      } else {
        J_$vect[as.integer(substr(id, 5, nchar(id)))] <- as.integer(values[[id]])
      }
      
      B$vect = c(B$vect, id)
      
      G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
      
      new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
      updateActionButton(session, id, label = new_label)

      output$dynamicPanel <- shiny::renderUI({
        if (verif_game(G_)) {
          elapsed_time=as.numeric(difftime(Sys.time(), start_time$vect[1], units = "secs"))
          s=floor(elapsed_time)
          m=s%/%60
          s=s-60*m
          h=m%/%60
          m=m-60*h
          shiny::showModal(modalDialog(
            title="Victoire !",
            "Félicitations ! Vous avez complété la grille avec succès",
            shiny::br(),
            paste("Votre temps:",h,'h',m,'min',s,'s'),
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::actionButton("replay","Rejouer", style="color: black; background-color: #7CFC00; border-color: #377100"),
              shiny::actionButton("Main","Accueil", style="color: black; background-color: #FFC0CB; border-color: #9C767C")
            )
          ))
        }
      })
    })
  })
      
      observeEvent(input$replay, {
        shiny::removeModal()
          if (current_page()=='jeu_facile'){
            J$vect= DB_facile[sample(1:20, 1)][[1]]
          }
            B$vect=c()
          if (current_page()=='jeu_moyen'){
            J$vect= DB_moyen[sample(1:20, 1)][[1]]
          }
          if (current_page()=='jeu_difficile'){
            J$vect= DB_difficile[sample(1:20, 1)][[1]]
          }
            B$vect=c()
            J_$vect=J$vect
            G_ <- t(matrix(J_$vect, ncol = 8, byrow = TRUE))
            output$dynamicPanel <- shiny::renderUI({
              if (verif_game(G_)) {
                shinyWidgets::show_alert(
                  title="Victoire !",
                  text= "Félicitation ! Vous avez complété la grille avec succès",
                  type= "success",
                )
              }
            })
            for (id in btn_ids) {
              values[[id]] <- 'rien'
              if (J$vect[as.integer(substr(id, 5, nchar(id)))]=='NaN'){
                new_label <- ifelse(values[[id]] == "rien", "&nbsp;", values[[id]])
                shiny::updateActionButton(session, id, label = new_label)
              }
            }
            output$page_content = shiny::renderUI({
              shiny::fluidPage(
                shiny::actionButton("retour_accueil","retour à la page d'accueil"),
                shiny::fluidRow(
                  shiny::column(12, 
                         shiny::div(
                           style = "display: grid; grid-template-columns: repeat(8, 50px); grid-gap: 5px; justify-content: center; align-items: center;",
                           lapply(1:64, function(i) {
                             if (J$vect[i] == 'NaN') {
                               shiny::actionButton(paste0("btn_", i), label = ' ', style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;")
                             } else {
                               shiny::actionButton(paste0("btn_", i), label = as.character(J$vect[i]), style = "width: 50px; height: 50px; font-size: 20px; padding: 0; margin: 0;", disabled = TRUE)
                             }
                           })
                         )
                  )
                ),
                shiny::fluidRow(
                  column(12, uiOutput("dynamicPanel"))
                ),
                shiny::actionButton('Back', 'Retour en arrière', style = "color: black; background-color: #F8BBD0; border-color: #AC8290"),
                shiny::actionButton('Clear', 'Recommencer', style = "color: black; background-color: #A8E6CF; border-color: #749F8F")
              )
            })
            start_time$vect = Sys.time()
      })
      observeEvent(input$Main,{
        shiny::removeModal()
        current_page('Accueil')
        output$page_content = shiny::renderUI({
          shiny::div(style = "text-align:center;",
              shiny::p('Accueil'),
              shiny::actionButton("bouton_jeu","Jouer une partie", style = "color: black; background-color: #86D3F7; border-color: #497285")
          )
        })
      })
}

# Lancer l'application
shiny::shinyApp(ui = ui, server = server)
}
