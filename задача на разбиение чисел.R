lich <- function(A){
  if(length(A) %% 2 == 1){
    return('Числа нельзя разбить по парам')
  } else{
    ind = 1
    for(i in A){
      ind = ind * i 
    }
      ind = round(ind^(1 / (length(A)/2)), 6)
      res = list()
    while(length(A) != 0){
      pair <-  c(A[1])
      A <-  A[-1]
      i = 1
      while(length(pair) != 2){
        if((A[i] * pair[1])==ind){
          pair <-  append(pair, A[i])
          res[[length(res) + 1]] <- pair
          A <-  A[-i]
        } else if(i != length(A)){
          i <-  i + 1
        } else{
          return('Числа нельзя разбить по парам')
        }
      }
    }
    return(res)
  }
}
  

merge_part <- function(A, B){
  res = c()
  while(length(A) != 0 & length(B) != 0){
    if(A[1] < B[1]){
      res <- append(res, A[1])
      A <- A[-1]
    } else{
      res <- append(res, B[1])
      B <- B[-1]
    }
  }
  res <- append(res,B)
  res <- append(res, A)
  return(res)
}

rec_part <- function(A){
  if(length(A) < 1){
    return(A)
  } else{
    a <- length(A) %/% 2
    return(merge_part(rec_part(A[1:a]), rec_part(A[(a+1):length(A)])))
  }
}

fast_sort <- function(A){
  if(length(A) < 2){
    return(A)
  } else{
    i = A[length(A)]
    rA <- fast_sort(A[A > i])
    lA <- fast_sort(A[A < i])
    return(c(lA, A[A == i], rA))
  }
}
  
  
  