ratings<-matrix(c(  3,  NaN,    5,    1,    1,  NaN,    2,  NaN,  NaN,   NaN,
                    1,    2,  NaN,    3,  NaN,  NaN,  NaN,  NaN,  NaN,   NaN,
                  NaN,  NaN,    4,  NaN,  NaN,    1,  NaN,    3,    5,   NaN,
                  NaN,    2,  NaN,    2,  NaN,  NaN,    5,    3,  NaN,     4,
                    4,  NaN,    4,    2,    2,    3,  NaN,  NaN,    3,   NaN),nrow=5,ncol=10,byrow=TRUE)

ratings2<-ratings
m<-dim(ratings)[1]
n<-dim(ratings)[2]
MaxRatings<-5
MinRatings<-1
range<-MaxRatings-MinRatings+1
ratingClass<-vector(mode="list", length=m)
tt<-matrix(rep(NaN,m*range),nrow=m,ncol=range,byrow=TRUE)
for (i in 1:m){
  for(j in 1:range){
    t=0
    vasat<-vector(mode="list", length=1)
    for(k in 1:n){
      if(!is.na(ratings[i,k]) && ratings[i,k]==j){
        t<-t+1
        vasat[[1]][t]<-k }
    }
    tt[i,j]<-t; ratingClass[[i]][j]<-vasat }
}
Neigh   <-vector(mode="list", length=m)
Items   <-vector(mode="list", length=m)
Sim     <-vector(mode="list", length=m)
b2<-0
for(ac in 1:m){
  active<-ac
  bb<-0
  person=vector(mode="list", length=1)
  item  =vector(mode="list", length=1)
  rate  =vector(mode="list", length=1)
  for(i in 1:range){
    for(j in 1:tt[active,i]){
      for(k in 1:m){
        if(length(ratingClass[[k]][[i]])!=0 && length(ratingClass[[active]][[i]])!=0){
          for(e in 1:length(ratingClass[[k]][[i]])){
            if(k!=active && ratingClass[[active]][[i]][j]==ratingClass[[k]][[i]][e]){
              bb<-bb+1
              person[[1]][bb]<-k
              item[[1]][bb]<-ratingClass[[active]][[i]][j]
              rate[[1]][bb]<-i }
          }
        }
      }
    }
  }
  if(!is.na(bb)){b2<-b2+bb}
  Neigh[[ac]]<-person
  Items[[ac]]<-item
  Sim[[ac]]<-rate
}
UserSim <-matrix(rep(0,3*b2),nrow=b2,ncol=3,byrow=TRUE)
UserSim_order <-matrix(rep(0,3*b2),nrow=b2,ncol=3,byrow=TRUE)
b3<-0
for(i in 1:m){
  if(length(Neigh[[i]][[1]])!=0){
    for(j in 1:length(Neigh[[i]][[1]])){
      b3<-b3+1
      UserSim[b3,1]<-i
      UserSim[b3,2]<-Neigh[[i]][[1]][j]
      UserSim[b3,3]<-Sim[[i]][[1]][j]     }
  }
}
UserSim_order<-UserSim[order(UserSim[,3], decreasing = TRUE),]

for(i in 1:b2){
  for(j in 1:n){
    if(is.na(ratings2[UserSim_order[i,1],j])&&!is.na(ratings2[UserSim_order[i,2],j])){
      ratings2[UserSim_order[i,1],j]<-ratings2[UserSim_order[i,2],j]  }
  }
}
ratings3<-ratings2
for(i in 1:m){
  for(j in 1:n){
    if(!is.na(ratings[i,j])){
      ratings3[i,j]<-NaN   }
  }
}   
temp1<-c(rep(NaN,n))
temp2<-c(rep(NaN,n))
Item_order<-vector(mode="list", length=m)
for(i in 1:m){
  temp1<-sort(ratings3[i,],decreasing = TRUE)
  temp2<-order(ratings3[i,],decreasing = TRUE)
  if(length(temp1)!=0){
    temp3<-c(rep(NaN,length(temp1)))
    for(j in 1:length(temp1)){
      temp3[j]<-temp2[j]   }
    Item_order[[i]]<-temp3    
  }
}
cat("----------------ratings-----------------", "\n", "\n")
print(ratings)
cat("----------------ratings2----------------", "\n", "\n")
print(ratings2)
cat("---------------Item order---------------", "\n", "\n")
for(i in 1:m){
  if(length(Item_order[[i]])==0){Item_order[[i]][1]<-NA}
  print(Item_order[[i]],print.gap=1)
}
cat("----------------Neighbors---------------", "\n", "\n")
for(i in 1:m){
  if(length(Neigh[[i]][[1]])==0){Neigh[[i]][[1]][1]<-NA}
  print(Neigh[[i]][[1]])
}
cat("---------------Similarity--------------", "\n", "\n")
for(i in 1:m){
  if(length(Sim[[i]][[1]])==0){Sim[[i]][[1]][1]<-NA}
  print(Sim[[i]][[1]])
}
#=============================================================
