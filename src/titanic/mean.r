library(titanic)
library(dplyr)

## 特征工程

train <- titanic_train
test <- titanic_test
data <- bind_rows(train, test)
data[data == ""] <- NA

data$Name<-as.character(data$Name)
data$Title<-sapply(data$Name,FUN = function(x){strsplit(x,split = "[,.]")[[1]][2]})
data$Title<-sub(" ","",data$Title)
officer <- c("Capt","Col", "Don", "Dr", "Major", "Rev")
royalty <- c("Dona","Lady","the Countess", "Sir", "Jonkheer")
data$Title[data$Title == "Mlle"] <- "Miss"
data$Title[data$Title == "Ms"] <- "Miss"
data$Title[data$Title == "Mme"] <- "Mrs"
data$Title[data$Title %in% royalty] <- "Royalty"
data$Title[data$Title %in% officer] <- "Officer"

ticket.count <- aggregate(data$Ticket, by = list(data$Ticket), function(x) sum(!is.na(x)))
data$TicketCount <- apply(data, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
data$TicketCount <- factor(sapply(data$TicketCount, function(x) ifelse(x > 1, 'Share', 'Unique')))

data$Fsize <- data$SibSp + data$Parch + 1

data <- data[-c(1,4,9,11)]

# 数据预处理

data$Age[is.na(data$Age)] <- mean(data$Age, na.rm=TRUE)
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm=TRUE)
data$Embarked[is.na(data$Embarked)] <- 'C'

data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
data$Title <- as.factor(data$Title)

train <- data[1:891,]
test <- data[892:1309,]

# 模型训练及预测

model <- glm(Survived ~ ., family=binomial(link='logit'), data=train)

pred.test.mean <- predict(model, test)