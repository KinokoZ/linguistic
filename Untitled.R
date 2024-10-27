library(rstanarm)
library(dplyr)
library(lme4)

# 拆分数据集
ceb_click <- perception_complete %>%
  filter(lang_source == "ceb", target == "click")

ceb_knock <- perception_complete %>%
  filter(lang_source == "ceb", target == "knock")

spn_click <- perception_complete %>%
  filter(lang_source == "spn", target == "click")

spn_knock <- perception_complete %>%
  filter(lang_source == "spn", target == "knock")

ceb <- perception_complete %>%
  filter(lang_source == "ceb")

spn <- perception_complete %>%
  filter(lang_source == "spn")


# # 拟合模型
# modelC <- lmer(rating ~ vk + (1 | target), data = ceb)
# 
# 
# modelS <- lmer(rating ~ vk + (1 | target), data = spn)
# 
# # 查看结果
# summary(model1)
# library(ggplot2)
# # 进行预测
# # 添加预测值到数据框
# spn$predicted <- predict(modelS)
# ceb$predicted <- predict(modelC)
# 
# # 可视化
# ggplot(spn, aes(x = vk, y = rating)) +
#   geom_line(aes(y = predicted), color = "black", size = 1) +  # 绘制预测线
#   labs(title = "Linear Mixed Effects Model: Actual vs Predicted",
#        x = "vk",
#        y = "Rating") +
#   scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
#   theme_minimal()
# 
# ggplot(ceb, aes(x = vk, y = rating)) +
#   geom_line(aes(y = predicted), color = "black", size = 1) +  # 绘制预测线
#   labs(title = "Linear Mixed Effects Model: Actual vs Predicted",
#        x = "vk",
#        y = "Rating") +
#   scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
#   theme_minimal()



library(ordinal)
library(ggeffects)
library(emmeans)
ceb_click <- ceb_click %>%
  mutate(across(where(is.character), as.factor))
ceb_click$rating <- as.ordered(ceb_click$rating)

modelCC <- clmm(rating ~ vk +(1|speaker), data = ceb_click,link="probit")

ceb_knock <- ceb_knock %>%
  mutate(across(where(is.character), as.factor))
ceb_knock$rating <- as.ordered(ceb_knock$rating)


modelCK <- clmm(rating ~ vk +(1|speaker), data = ceb_knock,link="probit")

spn_click <- spn_click %>%
  mutate(across(where(is.character), as.factor))
spn_click$rating <- as.ordered(spn_click$rating)


modelSC <- clmm(rating ~ vk +(1|speaker), data = spn_click,link="probit")


spn_knock <- spn_knock %>%
  mutate(across(where(is.character), as.factor))
spn_knock$rating <- as.ordered(spn_knock$rating)
modelSK <- clmm(rating ~ vk +(1|speaker), data = spn_knock,link="probit")


predicted_effects <- ggpredict(modelCC, terms = "vk")

plot(predicted_effects)


emm_results <- emmeans(modelCC, ~ vk)


# 绘制边际均值图
plot(emm_results)


predicted_effects <- ggpredict(modelCK, terms = "vk")

plot(predicted_effects)


emm_results <- emmeans(modelCK, ~ vk)

# 绘制边际均值图
plot(emm_results)


predicted_effects <- ggpredict(modelSC, terms = "vk")

plot(predicted_effects)


emm_results <- emmeans(modelSC, ~ vk)

# 绘制边际均值图
plot(emm_results)


predicted_effects <- ggpredict(modelSK, terms = "vk")

plot(predicted_effects)


emm_results <- emmeans(modelSK, ~ vk)

# 绘制边际均值图
plot(emm_results)


library(sjPlot)

plot_model(modelCC, terms = c("1|2","2|3","3|4","4|5","5|6","6|7","7|8","8|9","9|10"))  

plot_model(modelCK, terms = c("1|2","2|3","3|4","4|5","5|6","6|7","7|8","8|9","9|10"))  

plot_model(modelSC, terms = c("1|2","2|3","3|4","4|5","5|6","6|7","7|8","8|9","9|10"))  

plot_model(modelSK, terms = c("1|2","2|3","3|4","4|5","5|6","6|7","7|8","8|9","9|10"))  

