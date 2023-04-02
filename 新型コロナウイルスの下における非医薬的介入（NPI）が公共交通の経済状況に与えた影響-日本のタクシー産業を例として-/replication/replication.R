#新型コロナウイルスの下における非医薬的介入(NPI)が公共交通機関の経済状況に与えた影響-日本のタクシー産業を例として-
#分析再現コード
#作成者:早稲田大学現代政治経済研究所:高根晴
###############
#セットアップ #
###############

#使うパッケージのダウンロード
install.packages('pacman')
pacman::p_load(tidyverse,estimatr,gridExtra,choroplethr,choroplethrAdmin1,
               summarytools,MatchIt,modelsummary,cobalt,rsample, modelsummary,patchwork,xtable)
#データのセットアップ(パス構成:./data/*.csv)
#メインの分析に用いるデータ:図3,表1,表2,表3,図4,付表1,付図1-3
df_main　<- read.csv("data/main_data.csv")
#本文図1の作成のためのデータ
df_tr2020 <- read.csv("data/transportation_2020.csv")
#本文図2の作成のためのデータ
df_vdem <- read.csv("data/vdem.csv")
#表4作成のためのデータ
df_mob_2020 <- read.csv("data/mobility_report_2020.csv")
df_mob_2021 <- read.csv("data/mobility_report_2021.csv")

###############
#本文図1の作成#
###############
#グラフの作成
f1 <- df_tr2020  %>%
  ggplot(aes(x = month,y = loss,linetype = 交通機関))+
  geom_hline(yintercept = 1, linetype = 1,colour = "black")+
  scale_x_continuous(breaks = seq(1,12,by = 1))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "月（2020年）",y = "需要数（対2019年同月比）",title = "2020年における公共交通の需要数の変化")+
  geom_line()+
  theme_bw()
#確認
plot(f1)

###############
#本文図2の作成#
###############
#規制の強さデータの抜き出し
df_restriction_level <- df_vdem %>%filter(year == 2020) %>%select(country_name,year,v2cvgovres_0)
#年をfactor型に変換
df_restriction_level<-  df_restriction_level %>% mutate(year = as.factor(df_restriction_level$year))
#日本ダミーの設定
df_restriction_level  <-  df_restriction_level  %>% mutate(日本 = ifelse(country_name == "Japan",1,0))
df_restriction_level  <-  df_restriction_level %>% mutate(日本 = as.factor(df_restriction_level$日本))

#グラフの作成
f2 <- df_restriction_level  %>% 
  ggplot()+
  geom_boxplot(aes(x = year,y = v2cvgovres_0))+
  geom_jitter(aes(x = year,y = v2cvgovres_0))+
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 0)+
  #日本の数値に水平線を引く
  geom_hline(yintercept = df_restriction_level$v2cvgovres_0[df_restriction_level$日本==1],linetype = 3)+
  geom_label(aes(x=0.5,y= 0.64),label = "日本")+
  labs(x = "　年　", y = "移動の規制の度合い",
       title = "2020年COVID19の下における移動の規制の度合いの国別比較")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#確認
plot(f2)

###############
#メイン分析　#
###############
###########
#図3の作成#
###########
#全国レベルのデータを月ごとに損失を集計して,2回目緊急事態宣言あり都道府県を区別しておく
df_loss_group_by_date_and_second <- aggregate(x = df_main$loss_from_2019,by =list(df_main$final_date_month,df_main$second),mean)
#変数名の添付
colnames(df_loss_group_by_date_and_second) <-  c("final_date_month","seconds","loss_from_2019")

df_loss_group_by_date_and_second <- df_loss_group_by_date_and_second  %>% mutate(緊急事態宣言二回目発令都道府県　= as.factor(df_loss_group_by_date_and_second$second),
                                             #月の時系列的な並べ替え        
                                             final_date_month = factor((df_loss_group_by_date_and_second$final_date_month),levels = c("2020/2/29","2020/3/31","2020/4/30","2020/5/31",
                                                                                   "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                                                   "2020/10/31","2020/11/30","2020/12/31","2021/1/31",
                                                                                   "2021/2/28","2021/3/31")))


#グラフの作成
f3 <- df_loss_group_by_date_and_second   %>% 
  ggplot(aes(x =as.numeric(final_date_month), y = loss_from_2019,linetype = 緊急事態宣言二回目発令都道府県))+
  geom_line()+
  geom_hline(yintercept = 1, linetype = 1,colour = "black")+
  #緊急事態宣言月の範囲を可視化
  annotate("rect",xmin = 3,xmax = 5,ymin = 0,ymax=1.2,alpha = .3,fill = "gray")+
  annotate("rect",xmin =12 ,xmax = 14,ymin = 0,ymax=1.2,alpha = .3,fill = "gray")+
  scale_x_continuous(breaks = seq(1,14,by = 1),
                     labels = c("2020.2","3","4","5","6","7","8","9","10","11","12","2021.1",
                               "2","3"))+
  labs(x = " 月 ",y = "営業収入（対 2019年比）",title = "2020年2月-2021年3月におけるタクシーの営業収入の変化（対前年度比)",
       caption = )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

plot(f3)

#######################################
#使用する変数の記述統計（表1）の部分#
#######################################
#create descriptive stat 
names(df_main)
#平均値の算出と格納
variables_mean <- df_main %>% summarise("タクシーの営業収入 (対2019年比)" =round(mean(loss_from_2019),digits = 2),
                                "死亡率 (人口10万人あたり)" =round(mean(deaths_per_population),digits = 2),
                                "感染率 (人口10万人あたり)"= round(mean(cases_per_population),digits = 2),
                                "気温(月平均) " = round(mean(average_temperature),digits=2),
                                "失業率(月平均:%)"= round(mean(unemployment_rate_percent),digits = 2),
                                "65歳以上人口 (2019年)" = round(mean(over_age65_population),digits = 2),
                                "人口密度（1haあたり, 2019年）" = round(mean(population_per_density_per_ha),digits = 2),
                                "ICT従業者比率(2018年)"　= round(mean(prop_workers_in_it_sector),digits = 2))

#縦データに変換
colnames_variables_mean <- colnames(variables_mean)                    
variables_mean <- gather(variables_mean,key = "変数",
                  value = "平均",colnames_variables_mean)  

#標準偏差の算出と格納
variables_sd <- df_main %>% summarise("タクシーの営業収入 (対2019年比)" =round(sd(loss_from_2019),digits = 2),
                                                  "死亡率 (人口10万人あたり)" =round(sd(deaths_per_population),digits = 2),
                                                  "感染率 (人口10万人あたり)"= round(sd(cases_per_population),digits = 2),
                                                  "気温(月平均) " = round(sd(average_temperature),digits=2),
                                                  "失業率(月平均:%)"= round(sd(unemployment_rate_percent),digits = 2),
                                                  "65歳以上人口 (2019年)" = round(sd(over_age65_population),digits = 2),
                                                  "人口密度（1haあたり, 2019年）" = round(sd(population_per_density_per_ha),digits = 2),
                                                  "ICT従業者比率(2018年)"　= round(sd(prop_workers_in_it_sector),digits = 2))
#縦データに変換
variables_sd <- gather(variables_sd,key = "変数",
                         value = "標準偏差",colnames_variables_mean)  

#最小値
variables_min <- df_main %>% summarise("タクシーの営業収入 (対2019年比)" =round(min(loss_from_2019),digits = 2),
                                "死亡率 (人口10万人あたり)" =round(min(deaths_per_population),digits = 2),
                                "感染率 (人口10万人あたり)"= round(min(cases_per_population),digits = 2),
                                "気温(月平均) " = round(min(average_temperature),digits=2),
                                "失業率(月平均:%)"= round(min(unemployment_rate_percent),digits = 2),
                                "65歳以上人口 (2019年)" = round(min(over_age65_population),digits = 2),
                                "人口密度（1haあたり, 2019年）" = round(min(population_per_density_per_ha),digits = 2),
                                "ICT従業者比率(2018年)"　= round(min(prop_workers_in_it_sector),digits = 2))
#縦データに変換
variables_min <- gather(variables_min,key = "変数",
                       value = "最小値",colnames_variables_mean) 

#最大値
variables_max <- df_main %>% summarise("タクシーの営業収入 (対2019年比)" =round(max(loss_from_2019),digits = 2),
                                       "死亡率 (人口10万人あたり)" =round(max(deaths_per_population),digits = 2),
                                       "感染率 (人口10万人あたり)"= round(max(cases_per_population),digits = 2),
                                       "気温(月平均) " = round(max(average_temperature),digits=2),
                                       "失業率(月平均:%)"= round(max(unemployment_rate_percent),digits = 2),
                                       "65歳以上人口 (2019年)" = round(max(over_age65_population),digits = 2),
                                       "人口密度（1haあたり, 2019年）" = round(max(population_per_density_per_ha),digits = 2),
                                       "ICT従業者比率(2018年)"　= round(max(prop_workers_in_it_sector),digits = 2))

#縦データに変換
variables_max <- gather(variables_max,key = "変数",
                 value = "最大値",colnames_variables_mean) 

#上で求めて格納した変数ごとの平均値、標準偏差、最小値、最大値の結合（付表1の作成）
df_descriptive_stat <- left_join(variables_mean,variables_sd,by="変数")
df_descriptive_stat <- left_join(df_descriptive_stat,variables_min,by="変数")
df_descriptive_stat <- left_join(df_descriptive_stat,variables_max,by="変数")

#付表1の書き出し(latexを使用)
xtable(df_descriptive_stat, label="tb-ref", caption="変数の記述統計")

###########################
#全体の分析（表3のモデル）#
###########################
#モデル1
model1_1 <- lm(loss_from_2019 ~ state_of_emergency,data = df_main) 
summary(model1_1)

#モデル2
model1_2 <-lm_robust(loss_from_2019 ~ state_of_emergency + log(cases_per_population+1) + log(cases_lag+1) + average_temperature+log(unemployment_rate_percent) + log(over_age65_population)+log(population_per_density_per_ha)+log(prop_workers_in_it_sector),
                      data = df_main,clusters = pref,se_type = "CR0")
summary(model1_2)

#モデル3
model1_3 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) + log(over_age65_population)+log(population_per_density_per_ha)+log(prop_workers_in_it_sector),
                       data = df_main,clusters = pref,se_type = "CR0")
summary(model1_3)

#モデル4
model1_4 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + average_temperature+log(unemployment_rate_percent),
                    　  data = df_main,fixed_effects =~ pref,clusters = pref,se_type = "CR0")
summary(model1_4)

#モデル5
model1_5 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                        data = df_main,fixed_effects =~ pref,clusters = pref,se_type = "CR0")
summary(model1_5)

#モデル6
model1_6 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                        data = df_main,fixed_effects =~ pref,clusters = pref,se_type = "CR0")
summary(model1_6)

#モデル7
#年固定効果用に用いるyearダミーの作成
df_main <- df_main %>% mutate(year = ifelse(final_date_month  %in%  c("2020/2/29","2020/3/31","2020/4/30","2020/5/31",
                                                       "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                       "2020/10/31","2020/11/30","2020/12/31"),2020,2021))

#分析
model1_7 <- lm_robust(loss_from_2019  ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                        data = df_main,fixed_effects =~ pref + year,clusters = pref,se_type = "CR0")
summary(model1_7)

#モデル1-1~1-7までの表への出力(表1の書き出し)
#モデルをリスト形式でまとめる
colnames(df_main)
list_model_1 <- list(model1_1,model1_2,model1_3,model1_4,model1_5,model1_6,model1_7)
msummary(list_model_1, estimate = "{estimate}({std.error})",statistic = NULL, 
         title = "分析結果（全体）",
         coef_map = c("state_of_emergency" = "緊急事態宣言","log(deaths_per_population + 1)"="LN(死亡率+1)","log(deaths_lag + 1)"="LN（前月死亡率+1)",
                      "log(cases_per_population + 1)"="LN(感染率+1)","log(cases_lag + 1)"="LN(前月感染率+1)",
                      "average_temperature" = "気温",
                      "log(unemployment_rate_percent)" = "LN(失業率)",
                      "log(over_age65_population)"　= "LN(65歳以上人口)",
                      "log(population_per_density_per_ha)" = "LN(人口密度)",
                      "log(prop_workers_in_it_sector)" = "LN(ICT従業者比率)",
                      "(Intercept)" = "定数項"),"latex")

colnames(df_main)

##################################
#1回目、2回目の比較（表3のモデル)#
##################################

#モデル1-1
model_2_1_1 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) + log(over_age65_population)+log(population_per_density_per_ha)+log(prop_workers_in_it_sector),
                          data = df_main %>% filter(final_date_month  %in% c("2020/2/29","2020/3/31","2020/4/30","2020/5/31",
                                               "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                               "2020/10/31","2020/11/30","2020/12/31")),clusters = pref,se_type = "CR0")
summary(model_2_1_1)

#モデル1-2
model_2_1_2 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                          fixed_effects =~ pref,data = df_main %>% filter(final_date_month  %in% c("2020/2/29","2020/3/31","2020/4/30","2020/5/31",
                                                                            "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                                            "2020/10/31","2020/11/30","2020/12/31")),clusters = pref,se_type = "CR0")
summary(model_2_1_2)

#モデル2-1
model_2_2_1 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) + log(over_age65_population)+log(population_per_density_per_ha)+log(prop_workers_in_it_sector),
                          data = df_main %>% filter(final_date_month  %in% c("2020/2/29","2020/3/31",
                                                                             "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                                             "2020/10/31","2020/11/30","2020/12/31","2021/1/31",
                                                                             "2021/2/28","2021/3/31")),clusters = pref,se_type = "CR0")
summary(model_2_2_1)

#モデル2-2
model_2_2_2 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) ,
                         fixed_effects =~ pref, data = df_main %>% filter(final_date_month  %in% c("2020/2/29","2020/3/31",
                                                                                                    "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                                                                    "2020/10/31","2020/11/30","2020/12/31","2021/1/31",
                                                                                                    "2021/2/28","2021/3/31")),clusters = pref,se_type = "CR0")

summary(model_2_2_2)
#モデル2-3
model_2_2_3 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) ,
                          fixed_effects =~ final_date_month + pref, data = df_main %>% filter(final_date_month  %in% c("2020/2/29","2020/3/31",
                                                                            "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                                            "2020/10/31","2020/11/30","2020/12/31","2021/1/31",
                                                                            "2021/2/28","2021/3/31")),clusters = pref,se_type = "CR0")

summary(model_2_2_3)

#モデル1-1~2-3までの表への出力(表2の書き出し)
#モデルをリスト形式でまとめる
list_model_2 <- list(model_2_1_1,model_2_1_2,model_2_2_1,model_2_2_2,model_2_2_3)
msummary(list_model_2 , estimate = "{estimate}({std.error})",statistic = NULL, 
         title = "分析結果（全体）",
         coef_map = c("state_of_emergency" = "緊急事態宣言","log(deaths_per_population + 1)"="LN(死亡率+1)","log(deaths_lag + 1)"="LN（前月死亡率+1)",
                      "average_temperature" = "気温",
                      "log(unemployment_rate_percent)" = "LN(失業率)",
                      "log(over_age65_population)"　= "LN(65歳以上人口)",
                      "log(population_per_density_per_ha)" = "LN(人口密度)",
                      "log(prop_workers_in_it_sector)" = "LN(ICT従業者比率)",
                      "(Intercept)" = "定数項"),"latex")

####################################
#マッチング（付図1,2,3,表4のモデル)#
####################################
#マッチング
#マハラノビス最近マッチング(ATT)   
df_mhmt_att <- matchit(state_of_emergency ~ deaths_per_population+deaths_lag+average_temperature+unemployment_rate_percent+over_age65_population+population_per_density_per_ha+prop_workers_in_it_sector, 
                       data = na.omit(df_main), estimand = "ATT",
                       method = "nearest", distance = "mahalanobis")
#マハラノビス最近マッチング(ATC) 
df_mhmt_atc <- matchit(state_of_emergency ~ deaths_per_population+deaths_lag+average_temperature+unemployment_rate_percent+over_age65_population+population_per_density_per_ha+prop_workers_in_it_sector, 
                           data = na.omit(df_main), estimand = "ATC",
                           method = "nearest", distance = "mahalanobis")
#cem
df_cem <- matchit(state_of_emergency ~ deaths_per_population+deaths_lag+average_temperature+unemployment_rate_percent+over_age65_population+population_per_density_per_ha+prop_workers_in_it_sector, 
                           data = na.omit(df_main), 
                           method = "cem")

#バランスチェック
#可視化用に変数名を日本語化
v <- data.frame(old = c("deaths_per_population", "deaths_lag", "average_temperature", "unemployment_rate_percent", 
                        "over_age65_population", "population_per_density_per_ha","prop_workers_in_it_sector"),
                new = c("死亡率", "前月死亡率", "気温", 
                        "失業率", "65歳以上人口", "人口密度","ICT従業者比率"))

#付図1
af_1 <- love.plot(df_mhmt_att, threshold = 0.1, abs = TRUE, grid = TRUE, 
         shapes = c(18, 20), color = c("tomato", "royalblue"), 
         var.names = v,
         sample.names = c("調整前", "調整後"),
         title = "マッチングによる標準化差分の変化:表4モデル1(MHNearest:ATT)") +
         labs(x = "標準化差分(Standardized mean difference)の絶対値",
         shape = "", size = "", stroke = "", colour = "")
#確認
plot(af_1)

#付図2
af_2 <- love.plot(df_mhmt_atc, threshold = 0.1, abs = TRUE, grid = TRUE, 
          shapes = c(18, 20), color = c("tomato", "royalblue"), 
          var.names = v,
          sample.names = c("調整前", "調整後"),
          title = "マッチングによる標準化差分の変化:表4モデル2(MHNearest:ATC)") +
          labs(x = "標準化差分(Standardized mean difference)の絶対値",
          shape = "", size = "", stroke = "", colour = "")

#確認
plot(af_2)


#付図3
af_3 <- love.plot(df_cem , threshold = 0.1, abs = TRUE, grid = TRUE, 
          shapes = c(18, 20), color = c("tomato", "royalblue"), 
          var.names = v,
          sample.names = c("調整前", "調整後"),
          title = "マッチングによる標準化差分の変化:表4モデル3(CEM)") +
          labs(x = "標準化差分(Standardized mean difference)の絶対値",
          shape = "", size = "", stroke = "", colour = "")
plot(af_3)

#マッチングのデータをモデル分析用のデータセットに書き出し
df_mhmt_att_a<- match.data(df_mhmt_att)
df_mhmt_atc_a <- match.data(df_mhmt_atc)
df_cem_a <- match.data(df_cem)


#表3のモデル
#モデル1
colnames(df_mhmt_att_a)
model_3_1 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) +average_temperature+log(unemployment_rate_percent),
                      data = df_mhmt_att_a,clusters = pref,fixed_effects = ~pref,
                      se_type = "CR0",weights = weights)
summary(model_3_1)

#モデル2
model_3_2 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) +average_temperature+log(unemployment_rate_percent),
                       data = df_mhmt_atc_a,clusters = pref,fixed_effects = ~pref,
                       se_type = "CR0",weights = weights)

summary(model_3_2)

#モデル3
model_3_3 <- lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) +average_temperature+log(unemployment_rate_percent),
                       data = df_cem_a,clusters = pref,fixed_effects = ~pref,
                       se_type = "CR0",weights = weights)
summary(model_3_3)

#モデル1~3までの表への出力(表3の書き出し)
#モデルをリスト形式でまとめる
list_model_3 <- list(model_3_1,model_3_2,model_3_3)
msummary(list_model_3, estimate = "{estimate}({std.error})",statistic = NULL, 
                    title = "分析結果（全体）",
                    coef_map = c("state_of_emergency" = "緊急事態宣言","log(deaths_per_population + 1)"="LN(死亡率+1)","log(deaths_lag + 1)"="LN（前月死亡率+1)",
                                            "average_temperature" = "気温",
                                            "log(unemployment_rate_percent)" = "LN(失業率)",
                                            "log(over_age65_population)"　= "LN(65歳以上人口)",
                                            "log(population_per_density_per_ha)" = "LN(人口密度)",
                                            "(Intercept)" = "定数項"),"latex")


#######################################
#ブートストラップによる係数の推定(図4)#
#######################################
#seedのセット
set.seed(50)
#モデル6:表2
#3000回のブートストラップ
bt_1 <- bootstraps(na.omit(df_main),times = 3000)$splits %>%
  map(~ lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                  data = analysis(.),fixed_effects =~ pref,clusters = pref,
                  se_type = "CR0")) %>% 
  map(tidy) %>% 
  bind_rows(.id = "bootstrap.replicate")


bt_1 <- bt_1 %>% filter(term == "state_of_emergency")
summary(bt_1)

#可視化
#係数
plot_bt_1 <- bt_1 %>% ggplot(aes(x= estimate))+
  geom_density() +
  labs(x = "β",y = "頻度",title = "係数（モデル6:表2）",
       caption = "")+
  geom_vline(xintercept = median(bt_1$estimate),linetype = 2)+
  #geom_label(aes(x=median(bt_1$estimate),y= 15),label = "中央値:-0.245")+
  theme_bw()
plot(plot_bt_1)
#標準誤差
plot_bt_1_se <- bt_1 %>% ggplot(aes(x= std.error))+
  geom_density()+
  labs(x = "se",y = "頻度",title = "標準誤差",
       caption = "")+
  theme_bw()+
  geom_vline(xintercept = median(bt_1$std.error),linetype = 2)+
  #geom_label(aes(x = median(bt_1$std.error),y= 15),label = "中央値:0.010")+
  theme_bw()

plot(plot_bt_1_se)


#モデル1-2:表3
#3000回のブートストラップ
bt_2 <- bootstraps(df_main %>% filter(final_date_month %in% c("2020/2/29","2020/3/31","2020/4/30","2020/5/31",
                                                 "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                 "2020/10/31","2020/11/30","2020/12/31")),times = 3000)$splits %>%
  map(~ lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent) ,
                  data = analysis(.),fixed_effects =~ pref,clusters = pref,
                  se_type = "CR0")) %>% 
  map(tidy) %>% 
  bind_rows(.id = "bootstrap.replicate")

bt_2 <- bt_2 %>% filter(term == "state_of_emergency")
summary(bt_2)


#可視化
#係数
plot_bt_2 <- bt_2 %>% ggplot(aes(x= estimate))+
  geom_density() +
  labs(x = "β",y = "頻度",title = "係数（モデル1-2:表3）",
       caption = "")+
  geom_vline(xintercept = median(bt_2$estimate),linetype = 2)+
  #geom_label(aes(x=-0.30,y= 15),label = "中央値:-0.297")+
  theme_bw()

#標準誤差　
plot_bt_2_se <- bt_2 %>% ggplot(aes(x= std.error))+
  geom_density()+
  labs(x = "se",y = "頻度",title = "標準誤差",
       caption = "")+
  geom_vline(xintercept = median(bt_2$std.error),linetype = 2)+
  #geom_label(aes(x=0.01,y= 15),label = "中央値:0.009")+
  theme_bw()


#モデル2-3:表2
#3000回のブートストラップ
bt_3 <- bootstraps(df_main %>% filter(final_date_month %in%c("2020/2/29","2020/3/31",
                                                "2020/6/30","2020/7/31","2020/8/31","2020/9/30",
                                                "2020/10/31","2020/11/30","2020/12/31","2021/1/31",
                                                "2021/2/28","2021/3/31")),times = 3000)$splits %>%
  map(~ lm_robust(loss_from_2019 ~ state_of_emergency + log(deaths_per_population+1) + log(deaths_lag+1) + average_temperature+log(unemployment_rate_percent),
                  data = analysis(.),fixed_effects =~ pref+final_date_month,clusters = pref,se_type = "CR0")) %>% 
  map(tidy) %>% 
  bind_rows(.id = "bootstrap.replicate")


bt_3 <- bt_3 %>% filter(term == "state_of_emergency")
summary(bt_3)

#可視化
plot_bt_3 <- bt_3 %>% ggplot(aes(x= estimate))+
  geom_density() +
  labs(x = "β",y = "頻度",title = "係数（モデル2-3:表3）",
       caption = "")+
  geom_vline(xintercept = median(bt_3$estimate),linetype = 2)+
  #geom_label(aes(x=0.004,y= 15),label = "中央値:0.003")+
  theme_bw()

plot_bt_3_se <- bt_3 %>% ggplot(aes(x= std.error))+
  geom_density()+
  labs(x = "se",y = "頻度",title = "標準誤差",
       caption = "垂直線:中央値")+
  geom_vline(xintercept = median(bt_3$std.error),linetype = 2)+
  #geom_label(aes(x=0.023,y= 15),label = "中央値:0.022")+
  theme_bw()

#図4への書き出し
grid.arrange(plot_bt_1,plot_bt_2,plot_bt_3,plot_bt_1_se,plot_bt_2_se,plot_bt_3_se,ncol = 3,
            top="3000回のブートストラップによる係数の推定")

####################################################
#1回緊急宣言とそうでない時期の人々の移動の違い(表4)#
####################################################

#全国レベルデータの抽出と2020年と2021年の結合
mobility_2020 <- df_mob_2020 %>% filter(sub_region_1 == "")
mobility_2021 <- df_mob_2021 %>% filter(sub_region_1 == "")
mobility_jpn_all <- rbind(mobility_2020,mobility_2021)
#日付をdate型へと変換
mobility_jpn_all　<- mobility_jpn_all %>% mutate(date = as.Date(mobility_jpn_all$date))
#データをサンプル期間の3月31日まで絞る
mobility_jpn_all <- mobility_jpn_all  %>% filter(date <= "2021-03-31")

#1回目の緊急自体宣言の期間
mobility_jpn_firsts <- mobility_jpn_all  %>% filter(date　>= "2020-04-07" & date　<= "2020-05-25")
mobility_jpn_others <- mobility_jpn_all  %>% filter(date　< "2020-04-07" | date　> "2020-05-25")

#F検定
#小売店、娯楽施設
var.test(mobility_jpn_firsts$retail_and_recreation_percent_change_from_baseline,
         mobility_jpn_others$retail_and_recreation_percent_change_from_baseline)#p-value = 0.6003

#食料品、薬局
var.test(mobility_jpn_firsts$grocery_and_pharmacy_percent_change_from_baseline,
         mobility_jpn_others$grocery_and_pharmacy_percent_change_from_baseline)#p-value = 0.4421

#乗換駅
var.test(mobility_jpn_firsts$transit_stations_percent_change_from_baseline,
         mobility_jpn_others$transit_stations_percent_change_from_baseline)#p-value = 0.658

#職場
var.test(mobility_jpn_firsts$workplaces_percent_change_from_baseline,
         mobility_jpn_others$workplaces_percent_change_from_baseline) #p-value = 0.2783

#住宅
var.test(mobility_jpn_firsts$residential_percent_change_from_baseline,
         mobility_jpn_others$residential_percent_change_from_baseline)#p-value = 0.020

#公園
var.test(mobility_jpn_firsts$parks_percent_change_from_baseline,
         mobility_jpn_others$parks_percent_change_from_baseline)# p-value = 0.7832

#t検定（表4）
#小売店、娯楽施設
t.test(mobility_jpn_firsts$retail_and_recreation_percent_change_from_baseline,
       mobility_jpn_others$retail_and_recreation_percent_change_from_baseline,var.equal=T,paired=F) 
#食料品、薬局
t.test(mobility_jpn_firsts$grocery_and_pharmacy_percent_change_from_baseline,
       mobility_jpn_others$grocery_and_pharmacy_percent_change_from_baseline,var.equal=T,paired=F) 

#乗換駅
t.test(mobility_jpn_firsts$transit_stations_percent_change_from_baseline,
       mobility_jpn_others$transit_stations_percent_change_from_baseline,var.equal=T,paired=F)

#職場
t.test(mobility_jpn_firsts$workplaces_percent_change_from_baseline,
       mobility_jpn_others$workplaces_percent_change_from_baseline,var.equal=T,paired=F)
#住宅
#F検定の結果、等分散でない
t.test(mobility_jpn_firsts$residential_percent_change_from_baseline,
       mobility_jpn_others$residential_percent_change_from_baseline,var.equal=F,paired=F)
#公園
t.test(mobility_jpn_firsts$parks_percent_change_from_baseline,
       mobility_jpn_others$parks_percent_change_from_baseline,var.equal=T,paired=F)
#表4については、table.texに出力コード記載
