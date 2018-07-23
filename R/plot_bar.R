require(tidyverse)

df = read_csv("C:/Users/Z.Zhou/Desktop/600090.csv", )

df %>% ggplot(mapping = aes(x=left_coins)) +
    geom_histogram(breaks =c(0,100,500,1000)) + theme_bw()

df = df %>% mutate(group = case_when(
    left_coins >=0 & left_coins < 100 ~ "0-100",
    left_coins >=100 & left_coins < 500 ~ "100-500",
    left_coins >=500 & left_coins < 1000 ~ "500-1000",
    left_coins > 1000 ~ ">1000"
)) %>% 
    mutate(group = factor(group, levels = c("0-100", "100-500","500-1000",">1000")))

df = df %>% mutate(level_group = case_when(
    level_id >=0 & level_id < 100 ~ "0-100",
    level_id >=100 & level_id < 500 ~ "100-500",
    level_id >=500 & level_id < 1000 ~ "500-1000",
    level_id > 1000 ~ ">1000"
)) %>% mutate(level_group = factor(level_group, levels = c("0-100", "100-500","500-1000",">1000")))

plot_bar = function(df, col){
    df %>% ggplot(mapping = aes_string(x=col)) +
        geom_bar() + theme_bw() +
        theme(axis.text.x = element_text(size = 12,
                                          vjust = 0.5, hjust = 0.5,
                                          angle = 45))
}

plot_density = function(df, col){
    df %>% ggplot(mapping = aes_string(x=col)) +
        geom_density() + theme_bw() +
        theme(axis.text.x = element_text(size = 12,
                                         vjust = 0.5, hjust = 0.5,
                                         angle = 45))
}

plot_bar(df, "source")
plot_bar(df, "level_id")
plot_bar(df, "item_id")
plot_bar(df, "group")
plot_bar(df, "level_group")

plot_density(df, "left_coins") + xlim(0,200)
plot_density(df, "level_id")

require(readxl)

pd_600099 = read_excel("C:/Users/Z.Zhou/Desktop/600099.xlsx",
                       sheet = 1)

str(pd_600099)












#-----------------------------------------------------------
pd_600090 = read_excel("C:/Users/Z.Zhou/Desktop/60090_0719.xlsx",
                            sheet = 1)
pd_600090 = pd_600090 %>% filter(`用户`==1) 

table(pd_600090$act_date)

str(pd_600090)


pd_600090 %>% group_by(`当前所在关卡`) %>% 
    summarise(N=n()) %>% arrange(desc(N)) -> level_number1
write_csv(level_number1, path = "C:/Users/Z.Zhou/Desktop/game600090_level_number.csv")

ggplot(level_number1, aes(x=`当前所在关卡`, y=N)) +
    geom_line() 

plot(density(pd_600090$`当前所在关卡`,na.rm=TRUE),
     main = "关卡人数分布密度图")

ggplot(pd_600090, aes(x=`当前所在关卡`)) +
    geom_freqpoly()

pd_600090 %>% group_by(`当前所在关卡`) %>% 
    summarise(Mean = mean(`累积使用tips数`)) %>% 
    arrange(desc(Mean)) -> tip_mean1
write_csv(tip_mean1, path = "C:/Users/Z.Zhou/Desktop/game600090_tip_mean.csv")

ggplot(tip_mean1, aes(x=`当前所在关卡`, y=Mean)) +
    geom_line() + geom_smooth(method="lm") + theme_bw() + ylab("人均使用tips数")

lm(Mean ~ `当前所在关卡`, data = tip_mean1)

# ---------------------------------
pd_600099 = read_excel("C:/Users/Z.Zhou/Desktop/600099.xlsx",
                       sheet = 1)
pd_600099 = pd_600099 %>% filter(`用户`==1) 

table(pd_600099$act_date)

str(pd_600099)


pd_600099 %>% group_by(`当前所在关卡`) %>% 
    summarise(N=n()) %>% arrange(desc(N)) -> level_number2
write_csv(level_number2, path = "C:/Users/Z.Zhou/Desktop/game600099_level_number.csv")

ggplot(level_number2, aes(x=`当前所在关卡`, y=N)) +
    geom_line() 

plot(density(pd_600099$`当前所在关卡`,na.rm=TRUE),
     main = "关卡人数分布密度图")

pd_600099 %>% group_by(`当前所在关卡`) %>% 
    summarise(Mean = mean(`累积使用tips数`)) %>% 
    arrange(desc(Mean)) -> tip_mean2
write_csv(tip_mean2, path = "C:/Users/Z.Zhou/Desktop/game600099_tip_mean.csv")

ggplot(tip_mean2, aes(x=`当前所在关卡`, y=Mean)) +
    geom_line() + geom_smooth() + theme_bw() + ylab("人均使用tips数")
