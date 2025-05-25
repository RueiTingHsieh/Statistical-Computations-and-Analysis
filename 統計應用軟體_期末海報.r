data = read.csv("clash royal info.csv")

cor.test(data$Usage...., data$Win...., method = "pearson")
plot(data$Usage...., data$Win....,
     main = "使用率 vs. 勝率",
     xlab = "使用率 (%)",
     ylab = "勝率 (%)",
     col = "blue",
     pch = 19,
     cex = 1.5,
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1
)
out1 = lm(data$Win.... ~ data$Usage....)
abline(out1, col = "red", lwd = 2)

cor.test(data$Elixir_cost, data$Win...., method = "spearman")
data$Elixir_cost = as.factor(data$Elixir_cost)
plot(data$Elixir_cost, data$Win....,
     main = "卡片費用 vs. 勝率",
     xlab = "卡片費用",
     ylab = "勝率 (%)",
     col = "blue",
     pch = 19,
     cex = 1.5,
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1
)
out2 = lm(data$Win.... ~ data$Elixir_cost)
abline(out2, col = "red", lwd = 2)
'''
boxplot(data$Win.... ~ data$Elixir_cost,
        main = "不同聖水花費對勝率的分佈",
        xlab = "卡片費用",
        ylab = "勝率 (%)",
        col = "lightgreen",
        border = "darkgreen")
'''

anova_result = aov(data[["Win...."]] ~ Type, data = data)
summary(anova_result)

library(ggplot2)
ggplot(data, aes(x = Type, y = Win...., fill = Type)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red") +
  labs(title = "不同卡牌類型的勝率分布",
       x = "卡牌類型", y = "勝率 (%)") +
  theme_minimal() +
  theme(legend.position = "none")

