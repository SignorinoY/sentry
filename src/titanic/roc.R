library(plotROC)
library(MLmetrics)
pred.true <- read.csv("./ground_truth.csv")

pred.test <- data.frame(D = pred.true$Survived, M1 = pred.test.mean, M2 = pred.test.rf, M3 = pred.test.famd, stringsAsFactors = FALSE)
longdata <- melt_roc(pred.test, "D", c("M1", "M2", 'M3'), names=c("Mean", "Random Forest", "iterative FAMD"))

pred.test.y <- ifelse(pred.test$M1 > .5, 1, 0)
Accuracy(pred.test.y,pred.test$D)
Precision(pred.test.y,pred.test$D)
Recall(pred.test.y,pred.test$D)
AUC(pred.test.y,pred.test$D)

pred.test.y <- ifelse(pred.test$M2 > .5, 1, 0)
Accuracy(pred.test.y,pred.test$D)
Precision(pred.test.y,pred.test$D)
Recall(pred.test.y,pred.test$D)
AUC(pred.test.y,pred.test$D)

pred.test.y <- ifelse(pred.test$M3 > .5, 1, 0)
Accuracy(pred.test.y,pred.test$D)
Precision(pred.test.y,pred.test$D)
Recall(pred.test.y,pred.test$D)
AUC(pred.test.y,pred.test$D)

submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = pred.test.y)
write.csv(submit, file = "forcast-3.csv", row.names = FALSE)

g <- ggplot(longdata, aes(d = D, m = M, color = name)) +
  geom_roc(n.cuts = 0) +
  # 添加标题、注释等
  labs(
    color ='Method',
    x = "True Postive Fraction",
    y = "False Postive Fraction"
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
ggsave("titanic_roc.eps", g, width = 5, height = 4, dpi = 300, units = "in", device="eps")