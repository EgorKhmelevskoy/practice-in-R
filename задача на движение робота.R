#Функция, определяющая следующую точку (точки, если робот 'зажат' между двумя точками)
#движения робота при заданном направлении

next.point <- function(P,PS,direc){
  if(direc=='вертикаль'){
    high.p <- c()
    low.p <- c()
    for(i in 1:length(PS)){
      if(P[1]==PS[[i]][1]&&PS[[i]][2]>P[2]){
        high.p <- append(high.p,i)
      }
      if(P[1]==PS[[i]][1]&&PS[[i]][2]<P[2]){
        low.p <- append(low.p,i)
      }
    }
    if(length(high.p)==0&&length(low.p)==0){
      return(FALSE)
    } else if(length(high.p)>0&&length(low.p)==0){
      next.p <- high.p[1]
      for(i in high.p[-1]){
        if(PS[[i]][2]<PS[[next.p]][2]){
          next.p <- i
        }
      }
    } else if(length(low.p)>0&&length(high.p)==0){
      next.p <- low.p[1]
      for(i in low.p[-1]){
        if(PS[[i]][2]>PS[[next.p]][2]){
          next.p <- i
        }
      }
    } else{
      next.ph <- high.p[1]
      next.plow <- low.p[1]
      for(i in high.p[-1]){
        if(PS[[i]][2]<PS[[next.ph]][2]){
          next.ph <- i
        }
      }
      for(i in low.p[-1]){
        if(PS[[i]][2]>PS[[next.plow]][2]){
          next.plow <- i
        }
      }
      return(list(PS[[next.ph]],PS[-next.ph],PS[[next.plow]],PS[-next.plow]))
    }
  } else if(direc=="горизонталь"){
    left.p <- c()
    right.p <- c()
    for(i in 1:length(PS)){
      if(P[2]==PS[[i]][2]&&PS[[i]][1]<P[1]){
        left.p <- append(left.p,i)
      }
      if(P[2]==PS[[i]][2]&&PS[[i]][1]>P[1]){
        right.p <- append(right.p,i)
      }
    }
    if(length(left.p)==0&&length(right.p)==0){
      return(FALSE)
    } else if(length(left.p)>0&&length(right.p)==0){
      next.p <- left.p[1]
      for(i in left.p[-1]){
        if(PS[[i]][1]>PS[[next.p]][1]){
          next.p <- i
        }
      }
    } else if(length(right.p)>0&&length(left.p)==0){
      next.p <- right.p[1]
      for(i in right.p[-1]){
        if(PS[[i]][1]<PS[[next.p]][1]){
          next.p <- i
        }
      }
    } else{
      next.pl <- left.p[1]
      next.pr <- right.p[1]
      for(i in left.p[-1]){
        if(PS[[i]][1]>PS[[next.pl]][1]){
          next.pl <- i
        }
      }
      for(i in right.p[-1]){
        if(PS[[i]][1]<PS[[next.pr]][1]){
          next.pr <- i
        }
      }
      return(list(PS[[next.pl]],PS[-next.pl],PS[[next.pr]],PS[-next.pr]))
    }
  }
  return(list(PS[[next.p]],PS[-next.p]))
}

#Функция, определяющая, может ли робот посетить все точки, если задана точка старта
#и направление(горизонтальное или вертикальное) движения

Rec.search <- function(P,PS,direc){
  while(length(PS)>0){
    next.sit <- next.point(P,PS,direc) 
    if(length(next.sit)==1){
      return('нельзя обойти все точки')
    } else if(length(next.sit)==2){
      P <- next.sit[[1]]
      PS <- next.sit[[2]]
      if(direc=='горизонталь'){
        direc <- 'вертикаль'
      } else{
        direc <- 'горизонталь'
      }
    } else{
      if(direc=='горизонталь'){
        direc <- 'вертикаль'
      } else{
        direc <- 'горизонталь'
      }
      P1 <- next.sit[[1]]
      PS1 <- next.sit[[2]]
      P2 <- next.sit[[3]]
      PS2 <- next.sit[[4]]
      if(Rec.search(P1,PS1,direc)=='все точки можно обойти'||Rec.search(P2,PS2,direc)=='все точки можно обойти'){
        return('все точки можно обойти')
      } else{
        return('нельзя обойти все точки')
      }
    }
  }
  return('все точки можно обойти')
}


A <- list(c(1,1),c(1,2),c(2,1),c(2,2),c(3,1),c(3,2),c(4,1),c(4,2))
B <- list(c(3,0),c(3,-3),c(1,-3),c(3,-6),c(3,4),c(7,4),c(7,8),c(1,8))
D <- list(c(-3,3),c(-3,5),c(0,1),c(2,1),c(0,3))

#Функция, определяющая, может робот посетить все точки при точке старта (0,0) 
#и направлении (только вправо)

fix.dir <- function(PS){
  P <- c(0,0)
  next.ps <- c()
  for(i in 1:length(PS)){
    if(PS[[i]][2]==0&&PS[[i]][1]>0){
      next.ps <- append(next.ps,i)
    }
  }
  if(length(next.ps)==0){
    return('нельзя обойти все точки')
  } else{
    next.p <- next.ps[1]
    for(i in next.ps[-1]){
      if(PS[[i]][1]<PS[[next.p]][1]){
        next.p <- i
      }
    }
  }
  P <- PS[[next.p]]
  PS <- PS[-next.p]
  return(Rec.search(P,PS,'вертикаль'))
}

#Функция, присваивающая точкам новые значения в зависимости от угла поворота робота
#(поворот координатной оси)

new.coord <- function(f,P){
  X <- as.integer(P[1]*cos(f)+P[2]*sin(f))
  Y <- as.integer(-P[1]*sin(f)+P[2]*cos(f))
  return(c(X,Y))
}

#Функция, определяющая, может робот посетить все точки при заданной точке старта 
#и менящемся направлении

fix.point <- function(P,PS){
  Corners <- c(atan(1/4),atan(1/3),atan(1/2),atan(2/3),atan(1),atan(3/2),atan(2),atan(3),atan(4))
  PS.new <- PS
  P.new <- P
  for(f in Corners){
    P.new <- new.coord(f,P)
    for(i in 1:length(PS)){
      PS.new[[i]] <- new.coord(f,PS[[i]])
    }
    if(Rec.search(P.new,PS.new,'горизонталь')=='все точки можно обойти'||Rec.search(P.new,PS.new,'вертикаль')=='все точки можно обойти'){
      return('все точки можно обойти')
    }
  }
  return('нельзя обойти все точки')
}

#Функция, определяющая, может робот посетить все точки при меняющихся точке старта 
#и направлении

change.d.and.p <- function(PS){
  for(i in 1:length(PS)){
    if(fix.point(PS[[i]],PS[-i])=='все точки можно обойти'){
      return('все точки можно обойти')
    }
  }
  return('нельзя обойти все точки')
}

fix.point(c(0,0),list(c(-2,-1),c(-1,-3),c(-3,-4),c(-4,-2),c(-5,-5),c(-6,-3),c(-7,-1),c(-7,-6),c(-9,-2)))
change.d.and.p(list(c(-2,-1),c(-1,-3),c(-3,-4),c(-4,-2),c(-5,-5),c(-6,-3),c(-7,-1),c(-7,-6),c(-9,-2)))
fix.point(c(0,0),list(c(3,1),c(2,4),c(6,2),c(9,3),c(8,6)))
fix.point(c(0,0),list(c(-1,-3),c(-3,-4),c(-4,-2),c(-5,-5),c(-6,-3),c(-7,-1),c(-7,-6),c(-9,-2)))
change.d.and.p(list(c(-1,-3),c(-3,-4),c(-4,-2),c(-5,-5),c(-6,-3),c(-7,-1),c(-7,-6),c(-9,-2)))
fix.dir(list(c(3,0),c(3,3),c(1,3),c(1,5),c(-2,5),c(-2,8),c(3,8),c(3,5),c(5,5),c(5,-3),c(3,-3)))







