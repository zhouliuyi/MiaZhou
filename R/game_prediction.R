# game prediction
require(tidyverse)
require(data.table)
require(pacman)
p_load("corrplot")

setwd("C:/")
train_df = fread("tap4fun/tap_fun_train.csv")



c_mat = cor(as.matrix(train_df[, c(-1,-2,-109)]))
corrplot(c_mat, method = "circle", tl.cex = 0.3, type="upper", order="hclust")

fit = lm(prediction_pay_price ~ wood_reduce_value + bd_healing_spring_level + sr_guest_troop_capacity_level + sr_healing_space_level + pvp_lanch_count, data = train_df)
summary(fit)

cf = coefficients(fit)

train_df$x = cf[1] + cf[2] * train_df$wood_reduce_value + cf[3] * train_df$bd_healing_spring_level + cf[4] * train_df$sr_guest_troop_capacity_level + cf[5] * train_df$sr_healing_space_level + cf[6] * train_df$pvp_lanch_count

ggplot(train_df, aes(x=x, y=prediction_pay_price)) + geom_point() + geom_smooth(se=FALSE, method="lm")

test_df = fread("tap4fun/tap_fun_test.csv")
test_df$prediction_pay_price = cf[1] + cf[2] * test_df$wood_reduce_value + cf[3] * test_df$bd_healing_spring_level + cf[4] * test_df$sr_guest_troop_capacity_level + cf[5] * test_df$sr_healing_space_level + cf[6] * test_df$pvp_lanch_count

write.csv(test_df, file =  "./result_test.csv")
