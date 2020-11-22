library(missForest)
library(missMDA)
library(ggplot2)

accuracies <- data.frame()
mses <- data.frame()


data("mtcars")
dataset <- mtcars
category <- c(8, 9)
dataset$vs <- as.factor(dataset$vs)
dataset$am <- as.factor(dataset$am)
nbdim <- estim_ncpFAMD(dataset)$ncp
for (prob in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  for (boot in 1:100) {
    set.seed(boot)
    dataset.miss <- prodNA(dataset, noNA = prob)
    Omega <- is.na(dataset.miss)
    dataset.impute.rf <- missForest(dataset.miss, maxiter = 10, verbose = FALSE)
    dataset.impute.famd <- imputeFAMD(dataset.miss, ncp = nbdim)
    mse.rf <- sqrt(sum((dataset[-category] - dataset.impute.rf$ximp[-category]) ^ 2 * Omega[-category]) / sum(Omega[-category]))
    mse.famd <- sqrt(sum((dataset[-category] - dataset.impute.famd$completeObs[-category]) ^ 2 * Omega[-category]) / sum(Omega[-category]))
    accuracy.rf <- sum((dataset[,category] == dataset.impute.rf$ximp[,category]) * Omega[,category]) / sum(Omega[,category])
    
    
    levels(dataset.impute.famd$completeObs$vs) <- c('0', '1')
    levels(dataset.impute.famd$completeObs$am) <- c('0', '1')
    
    accuracy.famd <- sum((dataset[,category] == dataset.impute.famd$completeObs[,category]) * Omega[,category]) / sum(Omega[,category])
    mses <- rbind(mses, c(mse.rf, 'Random Forest', 'mtcars', prob))
    mses <- rbind(mses, c(mse.famd, 'iterative FAMD', 'mtcars', prob))
    accuracies <- rbind(accuracies, c(accuracy.rf, 'Random Forest', 'mtcars', prob))
    accuracies <- rbind(accuracies, c(accuracy.famd, 'iterative FAMD', 'mtcars', prob))
  }
}


data("iris")
dataset <- iris
nbdim <- estim_ncpFAMD(dataset)$ncp
category <- c(5)
for (prob in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  for (boot in 1:100) {
    set.seed(boot)
    dataset.miss <- prodNA(dataset, noNA = prob)
    Omega <- is.na(dataset.miss)
    dataset.impute.rf <- missForest(dataset.miss, maxiter = 10, verbose = FALSE)
    dataset.impute.famd <- imputeFAMD(dataset.miss, ncp = nbdim)
    mse.rf <- sqrt(sum((dataset[-category] - dataset.impute.rf$ximp[-category]) ^ 2 * Omega[-category]) / sum(Omega[-category]))
    mse.famd <- sqrt(sum((dataset[-category] - dataset.impute.famd$completeObs[-category]) ^ 2 * Omega[-category]) / sum(Omega[-category]))
    accuracy.rf <- sum((dataset[,category] == dataset.impute.rf$ximp[,category]) * Omega[,category]) / sum(Omega[,category])
    accuracy.famd <- sum((dataset[,category] == dataset.impute.famd$completeObs[,category]) * Omega[,category]) / sum(Omega[,category])
    mses <- rbind(mses, c(mse.rf, 'Random Forest', 'iris', prob))
    mses <- rbind(mses, c(mse.famd, 'iterative FAMD', 'iris', prob))
    accuracies <- rbind(accuracies, c(accuracy.rf, 'Random Forest', 'iris', prob))
    accuracies <- rbind(accuracies, c(accuracy.famd, 'iterative FAMD', 'iris', prob))
  }
}


names(mses) <- c('MSE', 'Method', 'Dataset', 'Prob')
mses$MSE <- as.numeric(mses$MSE)
mses$Method <- as.factor(mses$Method)
mses$Dataset <- as.factor(mses$Dataset)

g <- ggplot(data = mses, mapping = aes(x=Prob, y=MSE, fill=Method)) +
  geom_boxplot() +
  facet_wrap(~Dataset, scales = "free") +
  # 添加标题、注释等
  labs(
    x = "Proportion of Missing Entries",
    y = "MSE of Imputed Entries"
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
ggsave("mse_mixed_dataset.eps", g, width = 6, height = 4, dpi = 300, units = "in", device="eps")

names(accuracies) <- c('Accuracy', 'Method', 'Dataset', 'Prob')
accuracies$Accuracy <- as.numeric(accuracies$Accuracy)
accuracies$Method <- as.factor(accuracies$Method)
accuracies$Dataset <- as.factor(accuracies$Dataset)

g <- ggplot(data = accuracies, mapping = aes(x=Prob, y=Accuracy, fill=Method)) +
  geom_boxplot() +
  facet_wrap(~Dataset, scales = "free") +
  # 添加标题、注释等
  labs(
    x = "Proportion of Missing Entries",
    y = "Accuracy of Imputed Entries"
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
ggsave("accuracy_mixed_dataset.eps", g, width = 6, height = 4, dpi = 300, units = "in", device="eps")