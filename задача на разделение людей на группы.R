my_mat <- function(r,V){
  G <- matrix(data = 0,nrow=r,ncol=r)
  n <- 1
  for(i in 1:length(V)){
    for(j in 1:length(V[[i]])){
      if(n!=V[[i]][j]){
        G[n,V[[i]][j]] <- 1
        G[V[[i]][j],n] <- 1 
      }
    }
    n <- n+1
  }
  return(G)
}

list.vert <- function(v,G){
  if(v<=ncol(G)){
    A <- c()
    for(i in 1:nrow(G)){
      if(G[v,i]!=0){
        A <- append(A,i)
      }
    }
    return(A)
  } else{
    return('вершины нет в графе')
  }
}

G <- my_mat(10,list(c(2,5,10),c(3,5,6),c(6,7,9),c(7,8),c(6,9,10),c(9),c(8,9),c(9,10),c(10)))
n = ncol(G)
B <-  matrix(ncol = n, data = as.integer(!(G==1)))
for(i in 1:n){
  B[i,i] <- 0 
}

twogroups <- function(A, B){
  group1 <- list.vert(1, A)
  group2 <- append(list.vert(1, B), 1)
  print(group1)
  print(group2)
  if(length(group1) > 1){
    for(i in group1){
      if(sum(group1[group1!=i]%in%list.vert(i, B)) < length(group1[group1!=i])){
        return('NO')
      }
    }
  }
  for(i in group2[group2!=1]){
    if(sum(group2[group2!=i]%in%list.vert(i, B)) < length(group2[group2!=i])){
      if(length(group1) ==0 || sum(group1%in%list.vert(i, B))==length(group1)){
        group1 <- append(group1, i)
        group2 <- group2[group2 != i]
        print(group1)
        print(group2)
      }
    }
  }
  if(length(group1) + length(group2) == ncol(A)){
    return(list(group1, group2))
  }
}




A <- my_mat(6,list(c(2,4,3,6),c(1,4,3),c(5,6),c(3,1),c(6, 3)))
n = ncol(A)
B <-  matrix(ncol = n, data = as.integer(!(A==1)))
for(i in 1:n){
  B[i,i] <- 0 
}
twogroups(B, A)
