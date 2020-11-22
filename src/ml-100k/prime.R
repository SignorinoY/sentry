library(recommenderlab)
library(softImpute)
library(primePCA)
library(MASS)
library(ggplot2)
library(tidyr)

data(MovieLense)

ratings = MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
dim(ratings)

# Splitting data for test and train
percent_train = 0.8
#min(rowCounts(ratings.n))
items_to_keep = 15        # items to use for each user
rating_threshold = 3      # good rating implies >=3
n_eval = 1                # number of times to run eval

eval_sets = evaluationScheme(data = ratings, method = "split",
                             train = percent_train, given = items_to_keep,
                             goodRating = rating_threshold, k = n_eval)

train <- getData(eval_sets, "train")
test <- getData(eval_sets, "known")
eval <- getData(eval_sets, "unknown")

# User-User Collaborative Filtering

items_to_recommend <- 10000
recommender <- Recommender(data = train, method = "UBCF")
ratings.impute.ubcf <- predict(object = recommender, newdata = test, n = items_to_recommend, type = "ratings")
ratings.impute.ubcf <- ratings.impute.ubcf

# Item-Item Collaborative Filtering

items_to_recommend <- 10000
recommender <- Recommender(data = train, method = "IBCF")
ratings.impute.ibcf <- predict(object = recommender, newdata = test, n = items_to_recommend, type = "ratings")
ratings.impute.ibcf <- ratings.impute.ibcf

# SoftImpute

train.full <- rbind(as.matrix(train@data), as.matrix(test@data))
train.full[train.full == 0] <- NA

fits <- softImpute(train.full,trace=TRUE, type="svd")
ratings.impute.soft <- complete(train.full, fits)
ratings.impute.soft <- as(ratings.impute.soft, "realRatingMatrix")

ratings.impute.soft <- ratings.impute.soft[449:560,]

# primePCA
eigen_refine = function(V_cur, X, Omega, trace.it=F){
  # X and Omega are RsparseMatrix
  n = dim(X)[1]
  d = dim(V_cur)[1]
  K = dim(V_cur)[2]
  results = complete(V_cur, X, Omega, K, trace.it=trace.it)
  U_hat = results$u
  residual_all = results$res
  S_hat = X
  S_hat@x = as.vector(do.call(rbind, residual_all))
  S_hat = as(S_hat, "CsparseMatrix")
  X_hat = splr(S_hat, U_hat, V_cur)
  return(X_hat)
}
select_rows = function(X, rows){
  # X is a RsparseMatrix
  index = do.call(c, sapply(rows, function(row) (X@p[row] + 1):X@p[row + 1], simplify=F))
  Xs = X
  Xs@j = X@j[index]
  Xs@x = X@x[index]
  obs_per_row = sapply(rows, function(row) X@p[row + 1] - X@p[row])
  Xs@p = as.integer(c(0, cumsum(obs_per_row)))
  Xs@Dim[1] = length(rows)
  return(Xs)
}
sample_filter_op = function(thresh, V, Omega, prob=1){
  # Omega is RsparseMatrix
  
  d = dim(V)[1]
  K = dim(V)[2]
  n = dim(Omega)[1]
  selected = sapply(1:n, function(i){
    
    if (Omega@p[i] == Omega@p[i + 1]){
      obs_num = 0                             
    }
    else{
      omega_i_col = Omega@j[(Omega@p[i] + 1):Omega@p[i + 1]] + 1
      obs_num = length(omega_i_col)            
    }
    
    if (obs_num <= K){
      return(FALSE)
    }
    else {
      # print(i)
      # print(omega_i_col)
      # print(V[omega_i_col, ])
      sigmas = svd(V[omega_i_col, ])$d
      if (tail(sigmas, 1) >=  sqrt(obs_num / d) / thresh){
        rv = runif(1)
        if (rv < prob){
          return(TRUE)                                               
        }
        else{
          return(FALSE)
        }
      }
      else {
        return(FALSE)
      }
    }
  })
  return(which(selected))
  
}
col_scale = function(X, center=T, normalize=F){
  
  X_center = as(X, "Incomplete")
  X_center = biScale(X_center, row.center=FALSE, row.scale=FALSE, col.center=center, col.scale=normalize)
  X_center = as(X_center, "CsparseMatrix")    
  return(X_center)
}
get_omega = function(X){
  
  Omega = X
  num_nonzero = length(Omega@x)    
  Omega@x = rep(1, num_nonzero)    
  return(Omega)
}
complete = function(V_cur, X, Omega, K, trace.it=F){
  # X and Omega are RsparseMatrix
  # return all the principal scores as a list U_hat
  
  n = dim(X)[1]
  if (trace.it) {
    results = sapply(1:n, function(i){
      omega_col = Omega@j[(Omega@p[i] + 1):Omega@p[i + 1]] + 1
      x = X@x[(Omega@p[i] + 1):Omega@p[i + 1]]
      V_part = V_cur[omega_col, ]
      u_hat = ginv(V_part) %*% x
      return(list(u=u_hat, residual=x - V_part %*% u_hat))
    }, simplify=F)
  }   
  else {
    results = sapply(1:n, function(i){
      omega_col = Omega@j[(Omega@p[i] + 1):Omega@p[i + 1]] + 1
      x = X@x[(Omega@p[i] + 1):Omega@p[i + 1]]
      V_part = V_cur[omega_col, ]
      u_hat = ginv(V_part) %*% x
      return(list(u=u_hat, residual=x - V_part %*% u_hat))
    }, simplify=F)        
  }
  U_hat = sapply(results, function(i) i$u)
  if (K > 1){
    U_hat = t(U_hat)
  }
  else{
    U_hat = matrix(U_hat, ncol=1)
  }
  residual_all = sapply(results, function(i) i$residual, simplify=F)
  return(list(u=U_hat, res=residual_all))
  
}
K = 1
v_tilde <- primePCA(train.full, K, center = F)$V_cur
X_center = col_scale(train.full, F, F)
X_center = as(X_center, "RsparseMatrix")
Omega = get_omega(X_center)
rows = sample_filter_op(10, v_tilde, Omega, prob=1)
Xs = select_rows(X_center, rows)
Os = select_rows(Omega, rows)
ratings.impute.prime <- as.matrix(eigen_refine(v_tilde, Xs, Os))[449:560,]
ratings.impute.prime <- as(ratings.impute.prime, "realRatingMatrix")

# Evaluation
accuracy.ubcf = calcPredictionAccuracy(x = ratings.impute.ubcf, data = eval, byUser = TRUE)
accuracy.ibcf = calcPredictionAccuracy(x = ratings.impute.ibcf, data = eval, byUser = TRUE)
accuracy.soft = calcPredictionAccuracy(x = ratings.impute.soft, data = eval, byUser = TRUE)
accuracy.prime = calcPredictionAccuracy(x = ratings.impute.prime, data = eval, byUser = TRUE)


accuracy <- as.data.frame(rbind(accuracy.ubcf, accuracy.ibcf, accuracy.soft, accuracy.prime))
accuracy <- accuracy[-2]
accuracy$Type <- rep(c('UBFC', 'IBCF','softImpute','primePCA'), each=112)
result <- accuracy %>%  gather('error', 'value', RMSE, MAE)

g <- ggplot(data = result, mapping = aes(x=Type, y=value)) +
  geom_boxplot() +
  facet_wrap(~error, scales = "free") +
  # 添加标题、注释等
  labs(
    x = "Methods",
    y = "Error of Imputed Entries"
  ) +
  # 修改主题样式
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    text = element_text(color = "gray20"),
    axis.text = element_text(face = "italic", size = 11),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "gray40", size = 0.5),
    axis.line.y = element_blank(),
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    legend.position="bottom"
  )

ggsave("ml10m.eps", g, width = 8, height = 4, dpi = 300, units = "in", device="eps")
