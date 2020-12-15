angle_2dim = function(l){                          
  l_ordonnée=l[2,]
  i=1
  j=c(1)
  min=l_ordonnée[1]
  while (i<length(l_ordonnée)+1){
    if (min>l_ordonnée[i]){
      min=l_ordonnée[i]
      j=c(i)
    }
    i=i+1
  }
  i=1
  mins=c()
  while (i<length(l_ordonnée)+1){
    if (min==l_ordonnée[i]){
      mins=c(mins,i)
    }
    i=i+1
  }
  i=1
  minabs=l[1,mins[1]]
  if (length(mins)>1){
      while (i<length(mins)+1){
        if (minabs>l[1,mins[i]]){
          minabs=l[1,mins[i]]
          j=mins[i]
        }
        i=i+1
    }
    }
  i=1
  p=l[,j]
  list_angle=matrix(nrow = 2,ncol = length(l[1,]))
  while (i<length(l[1,])+1){
    if (i == j){
      list_angle[,i]=c(NA,i)
      }
    else if (l[1,i]==p[1]){
      list_angle[,i]=c(Inf,i)
    }
    else{
      list_angle[,i]=c((l[2,i]-p[2])/(l[1,i]-p[1]),i)
    }
    i=i+1
  }
  i=1
  while (i<length(list_angle[1,])+1){
    if (is.na(list_angle[1,i])){
      list_angle=list_angle[,-i]
    }
    i=i+1
  }
  i=1
  if (length(list_angle[1,])==1){
    list_angle=t(list_angle)
  }
  return(list_angle)
}
# Ici l est une liste de vecteur dans R^2 représenté par une matrice de taille 2*(nombres de points).
# On commence par trouver le point p de plus petite ordonnée ( p pour pivot ).
# On calcule ensuite l'angle que fait chaque point avec le pivot et l'axe des absices.
# On retourne une matrice ou la première ligne correspond à l'angle et la deuxieme la position dans la liste la position dans la matrice de départ 
# NA sur la première ligne veux dire que le point considérer est le pivot et Inf veux dire que le point considérer est sur le même axe vertical que le pivot.
l=matrix(c(5,-8,5,-8,0,-10,5,2,1,-8),nrow = 2)
u=angle_2dim(l)
u

