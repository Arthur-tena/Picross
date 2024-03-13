### Générer la grille de picross ###
picross_grid <- function(n,p,q){ 
  M<-matrix(nrow = n,ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      M[i,j]<-sample(c(0,1),size=1,prob = c(p,q))
    }
  }
  M
}
### Args:
## n: "int": taille de la matrice (carré)
## p: "float": proba associée au 0
## q: "float": proba associée au 1
print(picross_grid(5,0,1))


## compter les 1 consécutifs sur une ligne
count1row<-function(row,M){
  n <- dim(M)[1]
  m <- floor(n/2 +1)
  s=0
  rep<-c()
  for(j in 1:n){
    if(M[row,j]==1){if(j==n){s=s+1
    rep=c(rep,s)}
      else{s=s+1}
    }
    if(M[row,j]==0){
      if(s!=0){rep=c(rep,s)
      s=0}
    }
  }
  if(length(rep)==m){return(paste0(rep))}
  else {
    for(i in 1:(m-length(rep))){
    rep<-c("",rep)
    }
    return(paste0(rep))
    }
}

### ARGS: 
## row : "int": la ligne sur laquelle on compte
## M: "matrix": la matrice dans laquelle on compte



## compter les 1 consécutifs sur une colonne
count1col<-function(col,M){
  n<-dim(M)[1]
  m<-floor(n/2 +1)
  s=0
  rep=c()
  for(i in 1:n){
    if(M[i,col]==1){if(i==n){s=s+1
    rep=c(rep,s)}
      else{s=s+1}
    }
    if(M[i,col]==0){
      if(s!=0){rep=c(rep,s)
      s=0}
    }
  }
  if(length(rep)==m){return(paste0(rep))}
  else {
    for(i in 1:(m-length(rep))){
      rep<-c("",rep)
    }
    return(paste0(rep))
  }
}
### ARGS: 
## col : "int": la colonne sur laquelle on compte
## M : "matrix": la matrix dans laquelle on compte

M<-picross_grid(5,0.5,0.5)
Q <- M

dim(M)==dim(Q)

comparaison <- function(M,Q){
  if(dim(M)[1]!=dim(Q)[1] || dim(M)[2] != dim(Q)[2]){return("les matrices ne sont pas de même dimension")}
  else {
    result=TRUE
    for(i in 1: dim(M)[1]){
      for(j in 1:dim(M)[2]){
        if(M[i,j]!=Q[i,j]){result = FALSE}
      }
    }
  return(result)}
}
comparaison(M,Q)

observe({
matrice <-reactiveVal(matrix(0, nrow = 5, ncol = 5))
matrice2<-matrice()
print(matrice2)
})
