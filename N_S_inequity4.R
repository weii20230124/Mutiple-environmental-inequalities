#N
library(nlme)
library(lme4)
library(piecewiseSEM)
library(QuantPsyc)
library(glmmTMB)
library(ineq)
library(fields)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)

# 读取数据
dataN <- read.csv("D:\\昆士兰\\SEM-100-total\\SEM-N.csv", header = TRUE, row.names = 1)
dataS <- read.csv("D:\\昆士兰\\SEM-100-total\\SEM-S.csv", header = TRUE, row.names = 1)
dataN <- as.data.frame(dataN)
dataS <- as.data.frame(dataS)

# 提取 CTR_MN_NM_12 列为 "Japan" 或 "South Korea" 的所有行
dataS_Japan_SouthKorea <- dataS[dataS$CTR_MN_NM %in% c("Japan", "South Korea"), ]
dataS_Japan_SouthKorea <- as.data.frame(dataS_Japan_SouthKorea)
# 换掉CO2_3的位置和名字，让两个数据一致
col_names <- names(dataS_Japan_SouthKorea)
new_order <- col_names
idx1 <- which(col_names == "CO2_3")
idx2 <- which(col_names == "CO2_1")
new_order[c(idx1, idx2)] <- new_order[c(idx2, idx1)]
dataS_Japan_SouthKorea <- dataS_Japan_SouthKorea[, new_order]
col_names <- names(dataS_Japan_SouthKorea)
idx1 <- which(col_names == "CO2_3")
idx2 <- which(col_names == "CO2_1")
col_names[c(idx1, idx2)] <- col_names[c(idx2, idx1)]
names(dataS_Japan_SouthKorea) <- col_names
# 按共同列合并
dataN <- dataN[, !(names(dataN) == "OBJECTID")]
dataN <- rbind(dataN, dataS_Japan_SouthKorea)



# 全球南方   
dataS$CO2_per_capita <- dataS$CO2_1 / dataS$pop1
dataS$CO2_per_capita2000 <- dataS$CO2_2 / dataS$pop2
dataS$CO2_per_capita1990 <- dataS$CO2_3 / dataS$pop3
mat <- as.matrix(dataS$CO2_per_capita1990)
# 使用 apply 函数检查每一行是否包含 -Inf选择不包含 -Inf 的行，整行去除
rows_with_inf <- apply(mat, 1, function(row) any(is.infinite(row)))
dataS <- dataS[!rows_with_inf, ]
mat <- as.matrix(dataS$CO2_per_capita2000)
rows_with_inf <- apply(mat, 1, function(row) any(is.infinite(row)))
dataS <- dataS[!rows_with_inf, ]
dataS$PM25_per_capita <- dataS$PM2.5_1 / dataS$pop1
dataS$PM25_per_capita2000 <- dataS$PM2.5_4 / dataS$pop2
summary(dataS$PM25_per_capita)
dataS <- dataS[!rownames(dataS) %in% c("621", "1532"), ]
mat <- as.matrix(dataS$PM25_per_capita2000)
rows_with_inf <- apply(mat, 1, function(row) any(is.infinite(row)))
dataS <- dataS[!rows_with_inf, ]
dataS <- dataS[-c(3492,3247,9395,7068,3537), ]
# 删除 CTR_MN_NM_12 列为 "Japan" 和 "South Korea" 的行
dataS <- dataS[!(dataS$CTR_MN_NM %in% c("Japan", "South Korea")), ]
dataS <- as.data.frame(dataS)

summary(dataS$PM25_per_capita2000)
dataS$TheilPM25_contribution <- (dataS$PM25_per_capita ) 
dataS$TheilPM25_contribution2000 <- (dataS$PM25_per_capita2000)
dataS$TheilPM25_Change <- (dataS$TheilPM25_contribution2000 - dataS$TheilPM25_contribution)/dataS$TheilPM25_contribution2000
summary(dataS$TheilPM25_Change)
lower_bound <- quantile(dataS$TheilPM25_Change, 0.01, na.rm = TRUE)
upper_bound <- quantile(dataS$TheilPM25_Change, 0.99, na.rm = TRUE)
dataS$TheilPM25_Change <- pmax(pmin(dataS$TheilPM25_Change, upper_bound), lower_bound)
summary(dataS$TheilPM25_Change)

dataS$GR_Change <- (dataS$GR1-dataS$GR2)/dataS$GR2
summary(dataS$GR_Change)
lower_bound <- quantile(dataS$GR_Change, 0.02, na.rm = TRUE)
upper_bound <- quantile(dataS$GR_Change, 0.98, na.rm = TRUE)
dataS$GR_Change <- pmax(pmin(dataS$GR_Change, upper_bound), lower_bound)
summary(dataS$GR_Change)

dataS$EnvChange <- dataS$GR_Change
# dataS$EnvChange <- dataS$TheilPM25_Change
summary(dataS$EnvChange)

dataS$CO2_per_capita2015_1990 <-  (dataS$CO2_per_capita1990-dataS$CO2_per_capita)/dataS$CO2_per_capita1990
dataS$GDP_per_capita2015 <-  dataS$GDP3/dataS$pop1
lower_bound <- quantile(dataS$CO2_per_capita2015_1990, 0.05, na.rm = TRUE)
upper_bound <- quantile(dataS$CO2_per_capita2015_1990, 0.95, na.rm = TRUE)
dataS$CO2_per_capita2015_1990 <- pmax(pmin(dataS$CO2_per_capita2015_1990, upper_bound), lower_bound)
summary(dataS$CO2_per_capita2015_1990)
# 确保数据是一个数据框，并选择需要的列
mat <- as.matrix(dataS$CO2_per_capita2015_1990)
rows_with_inf <- apply(mat, 1, function(row) any(is.infinite(row)))
dataS <- dataS[!rows_with_inf, ]
summary(dataS$CO2_per_capita2015_1990)






# 输出数据的前几行
head(dataN)
dataN <- dataN[-c(1433, 1431,1434,1172), ]

# 计算人均CO2排放
dataN$CO2_per_capita <- dataN$CO2_3 / dataN$pop1
dataN$CO2_per_capita2000 <- dataN$CO2_2 / dataN$pop2
dataN$CO2_per_capita1990 <- dataN$CO2_1 / dataN$pop3
dataN$PM25_per_capita <- dataN$PM2.5_1 / dataN$pop1
dataN$PM25_per_capita2000 <- dataN$PM2.5_4 / dataN$pop2
dataN$TheilPM25_contribution <- (dataN$PM25_per_capita ) 
dataN$TheilPM25_contribution2000 <- (dataN$PM25_per_capita2000 )
dataN$TheilPM25_Change <- (dataN$TheilPM25_contribution2000 - dataN$TheilPM25_contribution)/dataN$TheilPM25_contribution2000
summary(dataN$TheilPM25_Change)
lower_bound <- quantile(dataN$TheilPM25_Change, 0.05, na.rm = TRUE)
upper_bound <- quantile(dataN$TheilPM25_Change, 0.95, na.rm = TRUE)
dataN$TheilPM25_Change <- pmax(pmin(dataN$TheilPM25_Change, upper_bound), lower_bound)
summary(dataN$TheilPM25_Change)
dataN$GR_Change <- (dataN$GR1-dataN$GR2)/dataN$GR2
summary(dataN$GR_Change)
lower_bound <- quantile(dataN$GR_Change, 0.005, na.rm = TRUE)
upper_bound <- quantile(dataN$GR_Change, 0.995, na.rm = TRUE)
dataN$GR_Change <- pmax(pmin(dataN$GR_Change, upper_bound), lower_bound)
summary(dataN$GR_Change)

dataN$EnvChange <- dataN$GR_Change
# dataN$EnvChange <- dataN$TheilPM25_Change 
summary(dataN$EnvChange)

# dataN$CO2_per_capita2015_1990 <-  dataN$CO2_per_capita1990-dataN$CO2_per_capita
dataN$CO2_per_capita2015_1990 <-  (dataN$CO2_per_capita1990-dataN$CO2_per_capita)/dataN$CO2_per_capita1990
dataN$GDP_per_capita2015 <-  dataN$GDP3/dataN$pop1
lower_bound <- quantile(dataN$CO2_per_capita2015_1990, 0.05, na.rm = TRUE)
upper_bound <- quantile(dataN$CO2_per_capita2015_1990, 0.95, na.rm = TRUE)
dataN$CO2_per_capita2015_1990 <- pmax(pmin(dataN$CO2_per_capita2015_1990, upper_bound), lower_bound)
summary(dataN$CO2_per_capita2015_1990)








# 合并全球南北方画散点图
col_names <- names(dataS)
new_order <- col_names
idx1 <- which(col_names == "CO2_3")
idx2 <- which(col_names == "CO2_1")
new_order[c(idx1, idx2)] <- new_order[c(idx2, idx1)]
dataS_Gai <- dataS[, new_order]
col_names <- names(dataS_Gai)
idx1 <- which(col_names == "CO2_3")
idx2 <- which(col_names == "CO2_1")
col_names[c(idx1, idx2)] <- col_names[c(idx2, idx1)]
names(dataS_Gai) <- col_names
dataHebing <- rbind(dataN, dataS_Gai)
dataHebing <- dataHebing[dataHebing$GDP_per_capita2015 > 0, ]

dataHebing$log_GDP <- log(dataHebing $GDP_per_capita2015)

ggplot(dataHebing , aes(x =GDP_per_capita2015, y =CO2_per_capita2015_1990)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "GDP_per_capita2015 vs CO2_per_capita2015_1990",
       x = "GDP_per_capita2015",
       y = "CO2_per_capita2015_1990") +
  theme_minimal()



# 基础散点图示例
plot(dataHebing$GDP_per_capita2015, dataHebing$TheilPM25_Change,
     main = "散点图：GDP_per_capita2015 与TheilPM25_Change的关系",
     xlab = "GDP_per_capita2015",
     ylab = "TheilPM25_Change",
     pch = 19,           # 使用实心圆点
     col = "blue")       # 点的颜色设为蓝色
# 三个环境值改善：CO2_per_capita2015_1990     GR_Change     TheilPM25_Change
# 三个环境值：CO2_per_capita    GR1     PM25_per_capita
library(ggplot2)
library(minpack.lm)

# 对 GDP 取对数
dataHebing$log_GDP <- log(dataHebing$GDP_per_capita2015)

# 对数回归拟合
log_model <- lm(CO2_per_capita2015_1990 ~ log_GDP, data = dataHebing)

# 绘制拟合结果
ggplot(dataHebing, aes(x = GDP_per_capita2015, y = CO2_per_capita2015_1990)) +
  geom_point(color = "blue", alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ log(x), color = "red", size = 1.5) +
  labs(title = "对数模型拟合",
       x = "GDP_per_capita2015",
       y = "CO2_per_capita2015_1990") +
  theme_minimal()
# 先对 GDP 和 CO2 取对数
dataHebing$log_CO2 <- log(abs(dataHebing$CO2_per_capita2015_1990) + 1)  # 避免 log(负值) 出错
dataHebing$log_GDP <- log(dataHebing$GDP_per_capita2015)

# 线性回归拟合 log-log 关系（幂律模型）
power_law_model <- lm(log_CO2 ~ log_GDP, data = dataHebing)
summary(power_law_model)

# 提取拟合参数
a <- exp(coef(power_law_model)[1])
b <- coef(power_law_model)[2]

# 绘制幂律拟合曲线
ggplot(dataHebing, aes(x = GDP_per_capita2015, y = CO2_per_capita2015_1990)) +
  geom_point(color = "blue", alpha = 0.5) +
  stat_function(fun = function(x) a * x^b, color = "red", size = 1.5) +
  labs(title = "幂律模型拟合",
       x = "GDP_per_capita2015",
       y = "CO2_per_capita2015_1990") +
  theme_minimal()
# 设定初始参数
start_list <- list(a = max(dataHebing$CO2_per_capita2015_1990), b = 0.0001)

# 指数衰减拟合
exp_model <- nlsLM(CO2_per_capita2015_1990 ~ a * (1 - exp(-b * GDP_per_capita2015)),
                   data = dataHebing, start = start_list)

# 提取拟合参数
a_fit <- coef(exp_model)["a"]
b_fit <- coef(exp_model)["b"]

# 绘制指数衰减拟合曲线
ggplot(dataHebing, aes(x = GDP_per_capita2015, y = CO2_per_capita2015_1990)) +
  geom_point(color = "blue", alpha = 0.5) +
  stat_function(fun = function(x) a_fit * (1 - exp(-b_fit * x)), color = "red", size = 1.5) +
  labs(title = "指数衰减拟合",
       x = "GDP_per_capita2015",
       y = "CO2_per_capita2015_1990") +
  theme_minimal()
summary(log_model)         # 对数模型
summary(power_law_model)   # 幂律模型
summary(exp_model)         # 指数衰减模型











# 第一版南北两个属性对比
par(oma = c(0, 0, 0, 0))  # 减少外边距以减少行间距
# 设置布局，定义图例和散点图的区域宽度比例 # 创建 2x3 的布局
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), 
       widths = c(1, 4, 4), heights = c(0.8, 0.8))  # 调整宽度和高度比例
# 绘制图之前减少每个子图的边距
par(mar = c(0.1, 2, 2, 2))  # 减少子图的边距

# 第一个区域：绘制垂直的颜色条
par(mar = c(4, 4, 4, 2))  # 设置边距，适应图例的显示
gdp_scaled <- dataS$GDP_per_capita2015/ max(dataS$GDP_per_capita2015)
GDP_per_capita2015S <- dataS$GDP_per_capita2015
GDP_per_capita2015N <- dataN$GDP_per_capita2015
# 获取 GDP_per_capita2015N 的排序索引和按升序排列的原始数据
gdp_per_capita_sorted <- dataS$GDP_per_capita2015[order(dataS$GDP_per_capita2015)]
# 使用颜色渐变方案 1: 使用多种颜色渐变（红-黄-绿-蓝）
# color_palette <- colorRampPalette(c("#159FD7", "#B7DB29","#E4B112","#B7617D","#9281DD"))(100)
color_palette <- colorRampPalette(c("#B7617D","#E4B112", "#B7DB29","#159FD7"))(100)
# 定义参数，方便修改节点值
threshold_1 <- 8000
threshold_2 <- 18000
# 定义三个区间的映射函数
map_values <- function(x) {
  if (x <= threshold_1) {
    # 0 到 threshold_1 线性映射到颜色条的前 50%
    linear_scaled_value <- rescale(x, to = c(0, 50), from = c(0, threshold_1))
    return(linear_scaled_value)
  } else if (x <= threshold_2) {
    # threshold_1 到 threshold_2 线性映射到 50 到 90
    linear_scaled_value <- rescale(x, to = c(50, 90), from = c(threshold_1, threshold_2))
    return(linear_scaled_value)
  } else {
    # 大于 threshold_2 的部分线性映射到 90 到 100
    linear_scaled_value <- rescale(x, to = c(90, 100), from = c(threshold_2, max(gdp_per_capita_sorted)))
    return(linear_scaled_value)
  }
}
# 定义反向映射函数以获得对应的 GDP 值
inverse_map_values <- function(mapped_value) {
  if (mapped_value <= 50) {
    # 对应于 0 到 threshold_1 的区间
    gdp_value <- rescale(mapped_value, to = c(0, threshold_1), from = c(0, 50))
    return(gdp_value)
  } else if (mapped_value <= 90) {
    # 对应于 threshold_1 到 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_1, threshold_2), from = c(50, 90))
    return(gdp_value)
  } else {
    # 对应于大于 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_2, max(gdp_per_capita_sorted)), from = c(90, 100))
    return(gdp_value)
  }
}
# 绘制垂直的颜色条
image(
  x = 1,
  y = seq(0, 1, length.out = 100),
  z = t(matrix(seq(0, 1, length.out = 100), ncol = 1)),
  col = color_palette,
  axes = FALSE,
  xlab = "",
  ylab = ""
)
# 计算十分位的刻度位置及对应的实际 GDP 值
tick_positions <- seq(0, 100, by = 10)  # 0%, 10%, ..., 100%
gdp_ticks <- sapply(tick_positions, inverse_map_values)  # 计算每个位置对应的 GDP 值
normalized_positions <- tick_positions / 100  # 将刻度位置归一化到 [0, 1] 区间
# 在右侧添加 y 轴刻度，显示颜色条位置的实际 GDP 对应的原始数据值
axis(
  side = 4,
  at = normalized_positions,
  labels = round(gdp_ticks),
  las = 1,
  tick = TRUE,
  cex.axis = 0.6,
  tck = -0.02
)
# 在左侧添加 y 轴标签
mtext("GDP per Capita", side = 2, line = 1, cex = 0.8)


# 第二个区域：绘制散点图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 将人口数据标准化，以适合绘图大小
# population_scaled <- dataN$pop1 / max(dataN$pop1) * 10
population_scaled <- dataN$pop1 / max(dataS$pop1) * 10
pop1<-dataN$pop1
# 直接对北半球 GDP 进行映射，并找到颜色
mapped_values_N <- sapply(GDP_per_capita2015N, map_values)  # 使用相同的映射函数将北半球 GDP 映射到颜色条范围
mapped_colors_N <- color_palette[ceiling(mapped_values_N)]  # 使用颜色条的颜色生成北半球的映射颜色
dataN$mapped_colors <- mapped_colors_N
# 为颜色添加透明度，假设透明度为 0.6
north_mapped_colors_alpha <- adjustcolor(mapped_colors_N, alpha.f = 0.6)
# 绘制散点图，使用颜色和大小表示不同维度
plot(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     xlab = "",#Environmental inequality mitigation
     ylab = "Carbon dioxide improvement per capita",
     main = "",#Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions
     pch = 19,
     col = north_mapped_colors_alpha,  # 使用按百分位排序的颜色映射到每个数据点
     cex = population_scaled)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     labels = dataN$name2,
     pos = 4, cex = 0.7, col = "black")


# 第三个区域：绘制图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 定义一个向量，用于保存每个城的数据点所在的象限
quadrants <- rep(NA, nrow(dataN))
# 使用条件判断来确定每个数据点属于哪个象限
quadrants[dataN$EnvChange > 0 & dataN$CO2_per_capita2015_1990 > 0] <- "Quadrant Ⅰ"
quadrants[dataN$EnvChange < 0 & dataN$CO2_per_capita2015_1990 > 0] <- "Quadrant Ⅱ"
quadrants[dataN$EnvChange < 0 & dataN$CO2_per_capita2015_1990 < 0] <- "Quadrant Ⅲ"
quadrants[dataN$EnvChange > 0 & dataN$CO2_per_capita2015_1990 < 0] <- "Quadrant Ⅳ"
dataN$Quadrant <- quadrants
# 统计每个象限的个数# 计算比例
quadrant_counts <- table(dataN$Quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)
# # 创建 ggplot 图形
# 确保每个象限的数据都是数值类型并去掉 NA 值"orange", "blue", "green", "purple"
colors <- c( "#549F9A","#074166", "#C30078", "#9A5747")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ")
# 确保每个象限的数据都是数值类型并去掉 NA 值
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]]))
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           xlim = c(0, 50000), ylim = c(0, max(density_data$y) * 1.1),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小, cex.axis = 1.1
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")
# 第四个区域：绘制图# 添加点大小的图例
par(mar = c(0, 0, 0, 0))  # 清空边距，使得图例可以在整个区域中显示
plot.new()  # 创建一个新的空白绘图区域
population_scaled <- dataS$pop1 / max(dataS$pop1) * 10
legend("center", legend = c('50M', '100M', '150M'),
       pch = 19,
       pt.cex = c(5000000 / max(dataS$pop1) * 10, 
                  10000000 / max(dataS$pop1) * 10, 
                  15000000 / max(dataS$pop1) * 10), 
       inset = c(0, 0), x.intersp = 1.4, y.intersp = 2.5,  # x.intersp 和 y.intersp 控制间距
       text.width = 0.1, adj = c(0, 0.5),
       col = rgb(0.5, 0.5, 0.5, 0.5),
       bty = "n",
       horiz = FALSE,  # 设置为垂直显示
       title = "Population Size"
)


# 第五个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 将人口数据标准化，以适合绘图大小
population_scaled <- dataS$pop1 / max(dataS$pop1) * 10
pop1<-dataS$pop1
# 删除 GDP3 列中值为 0 的行
dataS <- dataS[dataS$GDP3 != 0, ]
south_gdp_mapped <- sapply(dataS$GDP_per_capita2015 , map_values)#GDP_per_capita2015S    #gdp_per_capita_sorted
south_mapped_colors <- color_palette[ceiling(south_gdp_mapped)]
dataS$mapped_colors <- south_mapped_colors
# 为颜色添加透明度，假设透明度为 0.6
south_mapped_colors_alpha <- adjustcolor(south_mapped_colors, alpha.f = 0.6)
# 绘制散点图，使用颜色和大小表示不同维度
plot(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     xlab = "Greenness Improvement",
     ylab = "Carbon dioxide improvement per capita",
     # main = "Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions",
     pch = 19,
     col = south_mapped_colors_alpha,  # 使用按百分位排序的颜色映射到每个数据点
     cex = population_scaled)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     labels = dataS$name1,
     pos = 4, cex = 0.7, col = "black")

# 第六个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 定义一个向量，用于保存每个城市的数据点所在的象限
quadrants <- rep(NA, nrow(dataS))
# 使用条件判断来确定每个数据点属于哪个象限
quadrants[dataS$EnvChange > 0 & dataS$CO2_per_capita2015_1990 > 0] <- "Quadrant Ⅰ"
quadrants[dataS$EnvChange < 0 & dataS$CO2_per_capita2015_1990 > 0] <- "Quadrant Ⅱ"
quadrants[dataS$EnvChange < 0 & dataS$CO2_per_capita2015_1990 < 0] <- "Quadrant Ⅲ"
quadrants[dataS$EnvChange > 0 & dataS$CO2_per_capita2015_1990 < 0] <- "Quadrant Ⅳ"
dataS$Quadrant <- quadrants
# 统计每个象限的个数
quadrant_counts <- table(dataS$Quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)
# 生成四个象限的密度估计图
# par(mar = c(4, 4, 2, 2))  # 减小边距以增加绘图区域
colors <- c( "#549F9A","#074166", "#C30078", "#9A5747")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ")
# 确保每个象限的数据都是数值类型并去掉 NA 值
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]]))
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      max_gdp <- ceiling(max(dataS$GDP_per_capita2015, na.rm = TRUE))  # 获取 GDP per capita 的最大值
      breakpoint <- 10000
      adjusted_max <- breakpoint + (max_gdp - breakpoint) * 0.0125  # 将 10000 以上的部分压缩为原来的 1.25%
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "GDP per capita (thousand US$)", ylab = "Density", 
           xlim = c(0, adjusted_max), ylim = c(0, max(density_data$y) * 1.5), xaxt = "n")  # 设置x轴范围为0到调整后的最大值并隐藏默认x轴标签
      
      # 自定义x轴刻度，其中断点之前显示5个分位点，断点之后显示最大值
      axis_breaks <- c(seq(0, breakpoint, length.out = 5), breakpoint)
      axis_labels <- as.character(axis_breaks)
      axis_labels[length(axis_labels)] <- as.character(breakpoint)  # 使用最大值替代标签
      axis(1, at = c(axis_breaks, adjusted_max), labels = c(axis_labels, as.character(max_gdp)), 
           cex.axis = 1, las = 1)
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, lty = 1, bty = "n")
















# 2. 拟合 Logistic 模型
# -------------------------------
# 使用非线性最小二乘法 nls() 进行拟合
# 给定初始参数：L 取环境指标数据的最大值，x0 取 gdp 的中位数，k 给个较小的正值
# 三个环境值：CO2_per_capita    GR1     PM25_per_capita
# 三个环境值改善：CO2_per_capita2015_1990     GR_Change        TheilPM25_Change

start_list <- list(L = max(dataN$GR1 ), k = 0.0001, x0 = median(dataN$GDP_per_capita2015))
fit <- nls(GR1  ~ L / (1 + exp(-k * (GDP_per_capita2015 - x0))),
           data = dataN,
           start = start_list)
summary(fit)
gdp_fit <- seq(min(dataN$GDP_per_capita2015), max(dataN$GDP_per_capita2015), length.out = 200)
env_fit <- predict(fit, newdata = data.frame(gdp = gdp_fit))
fit_df <- data.frame(gdp = gdp_fit, env = env_fit)
ggplot(data = dataN, aes(x = gdp, y = env)) +
  geom_point(color = "blue", size = 2) +
  geom_line(data = fit_df, aes(x = gdp, y = env), color = "red", size = 1) +
  labs(title = "Logistic 模型拟合示例",
       x = "人均GDP",
       y = "环境指标") +
  theme_minimal()


















# 新图三个属性 南北分开
# 假设 dataN 是数据框，包含 GR_Change、TheilPM25_Change 和 CO2_per_capita2015_1990 列
# 首先根据象限划分数据
split_data <- function(data) {
  data$quadrant <- with(data, 
                        ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 1,
                               ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 5,
                                      ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 3,
                                             ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 < 0, 7,
                                                    ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 2,
                                                           ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 6,
                                                                  ifelse(GR_Change < 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 4,
                                                                         8))))))))
  return(data)
}

# 将数据分成8个象限
dataN <- split_data(dataN)
# 统计每个象限的个数及比例
quadrant_counts <- table(dataN$quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)


quadrants <- character(nrow(dataN))
quadrants[dataN$quadrant == 1] <- "Quadrant Ⅰ"
quadrants[dataN$quadrant == 2] <- "Quadrant Ⅱ"
quadrants[dataN$quadrant == 3] <- "Quadrant Ⅲ"
quadrants[dataN$quadrant == 4] <- "Quadrant Ⅳ"
quadrants[dataN$quadrant == 5] <- "Quadrant Ⅴ"
quadrants[dataN$quadrant == 6] <- "Quadrant Ⅵ"
quadrants[dataN$quadrant == 7] <- "Quadrant Ⅶ"
quadrants[dataN$quadrant == 8] <- "Quadrant Ⅷ"
dataN$Quadrant <- quadrants

par(oma = c(0, 0, 0, 0))  # 减少外边距以减少行间距
# 设置布局，定义图例和散点图的区域宽度比例 # 创建 2x3 的布局
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), 
       widths = c(1, 4, 4), heights = c(0.8, 0.8))  # 调整宽度和高度比例
# 绘制图之前减少每个子图的边距
par(mar = c(0.1, 2, 2, 2))  # 减少子图的边距

# 第一个区域：绘制垂直的颜色条
par(mar = c(4, 4, 4, 2))  # 设置边距，适应图例的显示
gdp_scaled <- dataS$GDP_per_capita2015/ max(dataS$GDP_per_capita2015)
GDP_per_capita2015S <- dataS$GDP_per_capita2015
GDP_per_capita2015N <- dataN$GDP_per_capita2015
# 获取 GDP_per_capita2015N 的排序索引和按升序排列的原始数据
gdp_per_capita_sorted <- dataS$GDP_per_capita2015[order(dataS$GDP_per_capita2015)]
# 使用颜色渐变方案 1: 使用多种颜色渐变（红-黄-绿-蓝）
# color_palette <- colorRampPalette(c("#159FD7", "#B7DB29","#E4B112","#B7617D","#9281DD"))(100)
color_palette <- colorRampPalette(c("#B7617D","#E4B112", "#B7DB29","#159FD7"))(100)
# 定义参数，方便修改节点值
threshold_1 <- 8000
threshold_2 <- 18000
# 定义三个区间的映射函数
map_values <- function(x) {
  if (x <= threshold_1) {
    # 0 到 threshold_1 线性映射到颜色条的前 50%
    linear_scaled_value <- rescale(x, to = c(0, 50), from = c(0, threshold_1))
    return(linear_scaled_value)
  } else if (x <= threshold_2) {
    # threshold_1 到 threshold_2 线性映射到 50 到 90
    linear_scaled_value <- rescale(x, to = c(50, 90), from = c(threshold_1, threshold_2))
    return(linear_scaled_value)
  } else {
    # 大于 threshold_2 的部分线性映射到 90 到 100
    linear_scaled_value <- rescale(x, to = c(90, 100), from = c(threshold_2, max(gdp_per_capita_sorted)))
    return(linear_scaled_value)
  }
}
# 定义反向映射函数以获得对应的 GDP 值
inverse_map_values <- function(mapped_value) {
  if (mapped_value <= 50) {
    # 对应于 0 到 threshold_1 的区间
    gdp_value <- rescale(mapped_value, to = c(0, threshold_1), from = c(0, 50))
    return(gdp_value)
  } else if (mapped_value <= 90) {
    # 对应于 threshold_1 到 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_1, threshold_2), from = c(50, 90))
    return(gdp_value)
  } else {
    # 对应于大于 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_2, max(gdp_per_capita_sorted)), from = c(90, 100))
    return(gdp_value)
  }
}
# 绘制垂直的颜色条
image(
  x = 1,
  y = seq(0, 1, length.out = 100),
  z = t(matrix(seq(0, 1, length.out = 100), ncol = 1)),
  col = color_palette,
  axes = FALSE,
  xlab = "",
  ylab = ""
)
# 计算十分位的刻度位置及对应的实际 GDP 值
tick_positions <- seq(0, 100, by = 10)  # 0%, 10%, ..., 100%
gdp_ticks <- sapply(tick_positions, inverse_map_values)  # 计算每个位置对应的 GDP 值
normalized_positions <- tick_positions / 100  # 将刻度位置归一化到 [0, 1] 区间
# 在右侧添加 y 轴刻度，显示颜色条位置的实际 GDP 对应的原始数据值
axis(
  side = 4,
  at = normalized_positions,
  labels = round(gdp_ticks),
  las = 1,
  tick = TRUE,
  cex.axis = 0.6,
  tck = -0.02
)
# 在左侧添加 y 轴标签
mtext("GDP per Capita", side = 2, line = 1, cex = 0.8)


# 第二个区域：绘制散点图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 将人口数据标准化，以适合绘图大小
# population_scaled <- dataN$pop1 / max(dataN$pop1) * 10
population_scaled <- dataN$pop1 / max(dataS$pop1) * 10
pop1<-dataN$pop1
# 直接对北半球 GDP 进行映射，并找到颜色
mapped_values_N <- sapply(GDP_per_capita2015N, map_values)  # 使用相同的映射函数将北半球 GDP 映射到颜色条范围
mapped_colors_N <- color_palette[ceiling(mapped_values_N)]  # 使用颜色条的颜色生成北半球的映射颜色
dataN$mapped_colors <- mapped_colors_N
# 为颜色添加透明度，假设透明度为 0.6
north_mapped_colors_alpha <- adjustcolor(mapped_colors_N, alpha.f = 0.6)
# 绘制散点图，使用颜色和大小表示不同维度
plot(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     xlab = "Greenness Improvement",#Environmental inequality mitigation
     ylab = "Carbon dioxide per capita improvement",
     main = "",#Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions
     pch = 19,
     col = dataN$mapped_colors,  # 使用按百分位排序的颜色映射到每个数据点
     cex = dataN$pop1 / max(dataS$pop1) * 10)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     labels = dataN$name2,
     pos = 4, cex = 0.7, col = "black")


# 第三个区域：绘制图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
plot(dataN$TheilPM25_Change, dataN$CO2_per_capita2015_1990,
     xlab = "PM2.5 per capita improvement",#Environmental inequality mitigation
     ylab = "Carbon dioxide per capita improvement",
     main = "",#Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions
     pch = 19,
     col = dataN$mapped_colors,  # 使用按百分位排序的颜色映射到每个数据点
     cex = dataN$pop1 / max(dataS$pop1) * 10)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     labels = dataN$name2,
     pos = 4, cex = 0.7, col = "black")

# 第四个区域：绘制图# 添加点大小的图例
par(mar = c(0, 0, 0, 0))  # 清空边距，使得图例可以在整个区域中显示
plot.new()  # 创建一个新的空白绘图区域
population_scaled <- dataS$pop1 / max(dataS$pop1) * 10
legend("center", legend = c('50M', '100M', '150M'),
       pch = 19,
       pt.cex = c(5000000 / max(dataS$pop1) * 10, 
                  10000000 / max(dataS$pop1) * 10, 
                  15000000 / max(dataS$pop1) * 10), 
       inset = c(0, 0), x.intersp = 1.4, y.intersp = 2.5,  # x.intersp 和 y.intersp 控制间距
       text.width = 0.1, adj = c(0, 0.5),
       col = rgb(0.5, 0.5, 0.5, 0.5),
       bty = "n",
       horiz = FALSE,  # 设置为垂直显示
       title = "Population Size"
)


# 第五个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
plot(dataN$EnvChange, dataN$TheilPM25_Change,
     xlab = "Greenness Improvement",#Environmental inequality mitigation
     ylab = "PM2.5 per capita improvement",
     main = "",#Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions
     pch = 19,
     col = dataN$mapped_colors,  # 使用按百分位排序的颜色映射到每个数据点
     cex = dataN$pop1 / max(dataS$pop1) * 10)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataN$EnvChange, dataN$CO2_per_capita2015_1990,
     labels = dataN$name2,
     pos = 4, cex = 0.7, col = "black")

# 第六个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 更新 ggplot 部分的颜色与标签
colors <- c("#329845", "#AED185","#276C9E" ,"#A3C9D5", "#FDF6A4","#F8984F","#C2ABC8","#912C2C")
# colors <- c("#549F9A", "#074166", "#C30078", "#9A5747", "#A0C4FF", "#FFC6A5", "#FFADAD", "#9BF6FF")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ", 
                     "Quadrant Ⅴ", "Quadrant Ⅵ", "Quadrant Ⅶ", "Quadrant Ⅷ")
# GDP   计算所有象限的全局最大 y 值
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           xlim = c(0, 50000), ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")








ZuiDaPop<-max(dataS$pop1)
# 保存现在的数据库，制作全球地图NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
library(tidyverse)
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),  # 仅使用 map_id 匹配地图区域
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # coord_quickmap() +  # 添加适当的地图投影，确保比例正确
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +  # 固定比例，设置坐标范围
  geom_point(
    data = dataN,
    aes(
      x = GCPNT_LON, 
      y = GCPNT_LAT, 
      color = Quadrant, 
      size = pop1 / ZuiDaPop * 10  # 在 aes() 外部引用 dataS$pop1
    ),
    alpha = 0.5
  ) +
  labs(
    size = "Population", # 修改颜色图例名称
    # color = "象限分类"     # 修改大小图例名称
  ) +
  # scale_color_brewer(palette = "Dark2") +  # 设置颜色方案
  # scale_color_manual(values = c("#329845", "#FDF6A4", "#AED185","#F8984F","#276C9E" ,"#C2ABC8","#A3C9D5","#912C2C")) +  # 自定义颜色
  scale_color_manual(values = c("#329845", "#AED185","#276C9E" ,"#A3C9D5", "#FDF6A4","#F8984F","#C2ABC8","#912C2C")) +  # 自定义颜色
  # scale_color_manual(values = c("#66BC98", "#D96558", "#AAD09D","#EFA143","#E3EA96" ,"#B43970","#FCDC89","#692F7C")) +
  theme_minimal()


















# 全球南SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
dataS <- split_data(dataS)
# 统计每个象限的个数及比例
quadrant_counts <- table(dataS$quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)
quadrants <- character(nrow(dataS))
quadrants[dataS$quadrant == 1] <- "Quadrant Ⅰ"
quadrants[dataS$quadrant == 2] <- "Quadrant Ⅱ"
quadrants[dataS$quadrant == 3] <- "Quadrant Ⅲ"
quadrants[dataS$quadrant == 4] <- "Quadrant Ⅳ"
quadrants[dataS$quadrant == 5] <- "Quadrant Ⅴ"
quadrants[dataS$quadrant == 6] <- "Quadrant Ⅵ"
quadrants[dataS$quadrant == 7] <- "Quadrant Ⅶ"
quadrants[dataS$quadrant == 8] <- "Quadrant Ⅷ"
dataS$Quadrant <- quadrants

par(oma = c(0, 0, 0, 0))  # 减少外边距以减少行间距
# 设置布局，定义图例和散点图的区域宽度比例 # 创建 2x3 的布局
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), 
       widths = c(1, 4, 4), heights = c(0.8, 0.8))  # 调整宽度和高度比例
# 绘制图之前减少每个子图的边距
par(mar = c(0.1, 2, 2, 2))  # 减少子图的边距

# 第一个区域：绘制垂直的颜色条
par(mar = c(4, 4, 4, 2))  # 设置边距，适应图例的显示
gdp_scaled <- dataS$GDP_per_capita2015/ max(dataS$GDP_per_capita2015)
GDP_per_capita2015S <- dataS$GDP_per_capita2015
GDP_per_capita2015N <- dataN$GDP_per_capita2015
# 获取 GDP_per_capita2015N 的排序索引和按升序排列的原始数据
gdp_per_capita_sorted <- dataS$GDP_per_capita2015[order(dataS$GDP_per_capita2015)]
# 使用颜色渐变方案 1: 使用多种颜色渐变（红-黄-绿-蓝）
# color_palette <- colorRampPalette(c("#159FD7", "#B7DB29","#E4B112","#B7617D","#9281DD"))(100)
color_palette <- colorRampPalette(c("#B7617D","#E4B112", "#B7DB29","#159FD7"))(100)
# 定义参数，方便修改节点值
threshold_1 <- 8000
threshold_2 <- 18000
# 定义三个区间的映射函数
map_values <- function(x) {
  if (x <= threshold_1) {
    # 0 到 threshold_1 线性映射到颜色条的前 50%
    linear_scaled_value <- rescale(x, to = c(0, 50), from = c(0, threshold_1))
    return(linear_scaled_value)
  } else if (x <= threshold_2) {
    # threshold_1 到 threshold_2 线性映射到 50 到 90
    linear_scaled_value <- rescale(x, to = c(50, 90), from = c(threshold_1, threshold_2))
    return(linear_scaled_value)
  } else {
    # 大于 threshold_2 的部分线性映射到 90 到 100
    linear_scaled_value <- rescale(x, to = c(90, 100), from = c(threshold_2, max(gdp_per_capita_sorted)))
    return(linear_scaled_value)
  }
}
# 定义反向映射函数以获得对应的 GDP 值
inverse_map_values <- function(mapped_value) {
  if (mapped_value <= 50) {
    # 对应于 0 到 threshold_1 的区间
    gdp_value <- rescale(mapped_value, to = c(0, threshold_1), from = c(0, 50))
    return(gdp_value)
  } else if (mapped_value <= 90) {
    # 对应于 threshold_1 到 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_1, threshold_2), from = c(50, 90))
    return(gdp_value)
  } else {
    # 对应于大于 threshold_2 的区间
    gdp_value <- rescale(mapped_value, to = c(threshold_2, max(gdp_per_capita_sorted)), from = c(90, 100))
    return(gdp_value)
  }
}
# 绘制垂直的颜色条
image(
  x = 1,
  y = seq(0, 1, length.out = 100),
  z = t(matrix(seq(0, 1, length.out = 100), ncol = 1)),
  col = color_palette,
  axes = FALSE,
  xlab = "",
  ylab = ""
)
# 计算十分位的刻度位置及对应的实际 GDP 值
tick_positions <- seq(0, 100, by = 10)  # 0%, 10%, ..., 100%
gdp_ticks <- sapply(tick_positions, inverse_map_values)  # 计算每个位置对应的 GDP 值
normalized_positions <- tick_positions / 100  # 将刻度位置归一化到 [0, 1] 区间
# 在右侧添加 y 轴刻度，显示颜色条位置的实际 GDP 对应的原始数据值
axis(
  side = 4,
  at = normalized_positions,
  labels = round(gdp_ticks),
  las = 1,
  tick = TRUE,
  cex.axis = 0.6,
  tck = -0.02
)
# 在左侧添加 y 轴标签
mtext("GDP per Capita", side = 2, line = 1, cex = 0.8)


# 第二个区域：绘制散点图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
population_scaled <- dataS$pop1 / max(dataS$pop1) * 10
pop1<-dataS$pop1
# 删除 GDP3 列中值为 0 的行
dataS <- dataS[dataS$GDP3 != 0, ]
south_gdp_mapped <- sapply(dataS$GDP_per_capita2015 , map_values)#GDP_per_capita2015S    #gdp_per_capita_sorted
south_mapped_colors <- color_palette[ceiling(south_gdp_mapped)]
dataS$mapped_colors <- south_mapped_colors
# 为颜色添加透明度，假设透明度为 0.6
south_mapped_colors_alpha <- adjustcolor(south_mapped_colors, alpha.f = 0.6)
# 绘制散点图，使用颜色和大小表示不同维度
plot(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     xlab = "Greenness Improvement",
     ylab = "Carbon dioxide per capita improvement",
     # main = "Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions",
     pch = 19,
     col = south_mapped_colors_alpha,  # 使用按百分位排序的颜色映射到每个数据点
     cex = population_scaled)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     labels = dataS$name1,
     pos = 4, cex = 0.7, col = "black")


# 第三个区域：绘制图
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
plot(dataS$TheilPM25_Change, dataS$CO2_per_capita2015_1990,
     xlab = "PM2.5 per capita improvement",
     ylab = "Carbon dioxide per capita improvement",
     # main = "Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions",
     pch = 19,
     col = south_mapped_colors_alpha,  # 使用按百分位排序的颜色映射到每个数据点
     cex = population_scaled)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     labels = dataS$name1,
     pos = 4, cex = 0.7, col = "black")


# 第四个区域：绘制图# 添加点大小的图例
par(mar = c(0, 0, 0, 0))  # 清空边距，使得图例可以在整个区域中显示
plot.new()  # 创建一个新的空白绘图区域
population_scaled <- dataS$pop1 / max(dataS$pop1) * 10
legend("center", legend = c('50M', '100M', '150M'),
       pch = 19,
       pt.cex = c(5000000 / max(dataS$pop1) * 10, 
                  10000000 / max(dataS$pop1) * 10, 
                  15000000 / max(dataS$pop1) * 10), 
       inset = c(0, 0), x.intersp = 1.4, y.intersp = 2.5,  # x.intersp 和 y.intersp 控制间距
       text.width = 0.1, adj = c(0, 0.5),
       col = rgb(0.5, 0.5, 0.5, 0.5),
       bty = "n",
       horiz = FALSE,  # 设置为垂直显示
       title = "Population Size"
)


# 第五个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
plot(dataS$EnvChange, dataS$TheilPM25_Change,
     xlab = "Greenness ImprovementPM2.5 per capita improvement",
     ylab = "PM2.5 per capita improvement",
     # main = "Relationship between Environmental Inequality and Increase or decrease in per capita carbon emissions",
     pch = 19,
     col = south_mapped_colors_alpha,  # 使用按百分位排序的颜色映射到每个数据点
     cex = population_scaled)  # 这里假设 population_scaled 是对人口数据的缩放，用于表示点的大小
# 添加垂直和水平线，划分四个象限
abline(v = 0, col = "grey", lty = 2)
abline(h = 0, col = "grey", lty = 2)
# 根据需要添加标签，标注重要数据点
text(dataS$EnvChange, dataS$CO2_per_capita2015_1990,
     labels = dataS$name1,
     pos = 4, cex = 0.7, col = "black")


# 第六个区域：绘制图# 添加点大小的图例
par(mar = c(4, 5, 4, 2))  # 设置边距，适应散点图的显示
# 更新 ggplot 部分的颜色与标签
colors <- c("#329845", "#AED185","#276C9E" ,"#A3C9D5", "#FDF6A4","#F8984F","#C2ABC8","#912C2C")
# colors <- c("#549F9A", "#074166", "#C30078", "#9A5747", "#A0C4FF", "#FFC6A5", "#FFADAD", "#9BF6FF")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ", 
                     "Quadrant Ⅴ", "Quadrant Ⅵ", "Quadrant Ⅶ", "Quadrant Ⅷ")
# GDP   计算所有象限的全局最大 y 值
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           # xlim = c(0, 3000),
           ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")




# 保存现在的数据库，制作全球地图SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
library(tidyverse)
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),  # 仅使用 map_id 匹配地图区域
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # coord_quickmap() +  # 添加适当的地图投影，确保比例正确
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +  # 固定比例，设置坐标范围
  geom_point(
    data = dataS,
    aes(
      x = GCPNT_LON, 
      y = GCPNT_LAT, 
      color = Quadrant, 
      size = pop1 / ZuiDaPop * 10  # 在 aes() 外部引用 dataS$pop1
    ),
    alpha = 0.5
  ) +
  labs(
    color = "Quadrant",
    size = "Population"
  ) +
  # scale_color_brewer(palette = "Set3") +  # 设置颜色方案
  # scale_color_manual(values = c("#329845", "#FDF6A4", "#AED185","#F8984F","#276C9E" ,"#C2ABC8","#A3C9D5","#912C2C")) +  # 自定义颜色
  scale_color_manual(values = c("#329845", "#AED185","#276C9E" ,"#A3C9D5", "#FDF6A4","#F8984F","#C2ABC8","#912C2C")) +  # 自定义颜色
    # scale_color_manual(values = c("#66BC98", "#D96558", "#AAD09D","#EFA143","#E3EA96" ,"#B43970","#FCDC89","#692F7C")) +
  theme_minimal()




# 全球合并地图NNNNNNNNNNNNNNNNNNNNSSSSSSSSSSSSSSSSSSSSSSNNNNNNNNNNSSSSS
dataNS <- rbind(dataN, dataS)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),  # 仅使用 map_id 匹配地图区域
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # coord_quickmap() +  # 添加适当的地图投影，确保比例正确
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +  # 固定比例，设置坐标范围
  geom_point(
    data = dataNS,
    aes(
      x = GCPNT_LON, 
      y = GCPNT_LAT, 
      color = Quadrant, 
      size = pop1 / ZuiDaPop * 10  # 在 aes() 外部引用 dataS$pop1
    ),
    alpha = 0.5
  ) +
  labs(
    color = "Quadrant",
    size = "Population"
  ) +
  # scale_color_brewer(palette = "Set3") +  # 设置颜色方案
  scale_color_manual(values = c("#329845", "#FDF6A4", "#AED185","#F8984F","#276C9E" ,"#C2ABC8","#A3C9D5","#912C2C")) +  # 自定义颜色
  # scale_color_manual(values = c("#66BC98", "#D96558", "#AAD09D","#EFA143","#E3EA96" ,"#B43970","#FCDC89","#692F7C")) +
  theme_minimal()








# 三维图全球北
library(plotly)
library(htmlwidgets)
library(webshot2)
# 定义阈值
threshold <- 1
fixed_size <- 1
# 修改点大小逻辑，设置小于阈值的点为固定值
point_sizes <- dataN$pop1 / max(dataS$pop1) * 80
point_sizes[point_sizes < threshold] <- fixed_size
# 创建三维交互式散点图
fig <- plot_ly(
  data = dataN,
  x = ~GR_Change,
  y = ~TheilPM25_Change,
  z = ~CO2_per_capita2015_1990,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    color = ~mapped_colors, #mapped_colors_N,  
    size = point_sizes,  # 点大小
    opacity = 1  # 确保点完全填充
  )
)
# 设置布局
fig <- fig %>% layout(
  title = "3D Scatter Plot",
  scene = list(
    xaxis = list(title = "Greenness"),
    yaxis = list(title = "PM25"),
    zaxis = list(title = "CO2")
  )
)
# 设置布局并让坐标轴在 (0, 0, 0) 交叉
fig <- fig %>% layout(
  scene = list(
    xaxis = list(
      title = "Greenness",
      zeroline = TRUE,  # 显示零线
      range = c(-0.2, 0.5),  # 确保 0 在范围内
      zerolinecolor = 'black',  # 零线颜色
      zerolinewidth = 2
    ),
    yaxis = list(
      title = "PM25",
      zeroline = TRUE,
      range = c(-0.3, 0.4),
      zerolinecolor = 'black',
      zerolinewidth = 2
    ),
    zaxis = list(
      title = "CO2",
      zeroline = TRUE,
      range = c(-5, 1),
      zerolinecolor = 'black',
      zerolinewidth = 2
    )
  )
)

# 显示图形
fig
saveWidget(fig, "3DScatterPlot.html", selfcontained = TRUE)
browseURL("3DScatterPlot.html")


# 假设 dataN 是数据框，包含 GR_Change、TheilPM25_Change 和 CO2_per_capita2015_1990 列
# 首先根据象限划分数据
split_data <- function(data) {
  data$quadrant <- with(data, 
                        ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 1,
                               ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 2,
                                      ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 3,
                                             ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 < 0, 4,
                                                    ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 5,
                                                           ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 6,
                                                                  ifelse(GR_Change < 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 7,
                                                                         8))))))))
  return(data)
}
# 将数据分成8个象限
dataN <- split_data(dataN)
# 获取所有象限的唯一值
quadrants <- unique(dataN$quadrant)
# 自定义视角配置（每个象限一个固定视角）
camera_views <- list(
  list(eye = list(x = 1.5, y = 1.5, z = 1.5)),  # 象限1视角
  list(eye = list(x = -1.5, y = 1.5, z = 1.5)), # 象限2视角
  list(eye = list(x = 1.5, y = -1.5, z = 1.5)), # 象限3视角
  list(eye = list(x = -1.5, y = -1.5, z = 1.5)),# 象限4视角
  list(eye = list(x = 1.5, y = 1.5, z = -1.5)), # 象限5视角
  list(eye = list(x = -1.5, y = 1.5, z = -1.5)),# 象限6视角
  list(eye = list(x = 1.5, y = -1.5, z = -1.5)),# 象限7视角
  list(eye = list(x = -1.5, y = -1.5, z = -1.5)) # 象限8视角
)

# 循环生成三维图
for (q in quadrants) {
  # 筛选当前象限的数据
  data_quadrant <- subset(dataN, quadrant == q)
  
  # 创建三维交互式散点图
  fig <- plot_ly(
    data = data_quadrant,
    x = ~GR_Change,
    y = ~TheilPM25_Change,
    z = ~CO2_per_capita2015_1990,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      color = ~mapped_colors,  
      size = point_sizes,  # 确保点大小正确缩放
      opacity = 1  # 确保点完全填充
    )
  )
  
  # 设置布局
  fig <- fig %>% layout(
    title = paste("3D Scatter Plot - Quadrant", q),
    scene = list(
      xaxis = list(title = "Greenness"),
      yaxis = list(title = "PM25"),
      zaxis = list(title = "CO2"),
      camera = camera_views[[q]]  # 使用对应象限的视角
    )
  )
  
  # 保存为HTML文件
  filename <- paste0("3DScatterPlot_Quadrant", q, ".html")
  saveWidget(fig, filename, selfcontained = TRUE)
  
  # 打开生成的图形（可选）
  browseURL(filename)
}




# 画八象限密度分布图
# 创建象限标签
quadrants <- character(nrow(dataN))
quadrants[dataN$quadrant == 1] <- "Quadrant Ⅰ"
quadrants[dataN$quadrant == 2] <- "Quadrant Ⅱ"
quadrants[dataN$quadrant == 3] <- "Quadrant Ⅲ"
quadrants[dataN$quadrant == 4] <- "Quadrant Ⅳ"
quadrants[dataN$quadrant == 5] <- "Quadrant Ⅴ"
quadrants[dataN$quadrant == 6] <- "Quadrant Ⅵ"
quadrants[dataN$quadrant == 7] <- "Quadrant Ⅶ"
quadrants[dataN$quadrant == 8] <- "Quadrant Ⅷ"
dataN$Quadrant <- quadrants

# 统计每个象限的个数及比例
quadrant_counts <- table(dataN$Quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)

# 更新 ggplot 部分的颜色与标签
colors <- c("#549F9A", "#074166", "#C30078", "#9A5747", "#A0C4FF", "#FFC6A5", "#FFADAD", "#9BF6FF")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ", 
                     "Quadrant Ⅴ", "Quadrant Ⅵ", "Quadrant Ⅶ", "Quadrant Ⅷ")
# GDP   计算所有象限的全局最大 y 值
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           xlim = c(0, 50000), ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataN$GDP_per_capita2015[dataN$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")


# 人口   计算所有象限的全局最大 y 值
# 对 pop1 取对数，处理长尾分布（避免 log(0) 的问题，增加一个小值）
dataN$log_pop1 <- log(dataN$pop1 + 1)
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataN$log_pop1[dataN$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataN$log_pop1[dataN$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           # xlim = c(0, 50000),
           ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataN$log_pop1[dataN$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")












# 三维图全球南
fig <- plot_ly(
  data = dataS,
  x = ~GR_Change,
  y = ~TheilPM25_Change,
  z = ~CO2_per_capita2015_1990,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    color = ~mapped_colors, # 使用 rgba 颜色
    size = dataS$pop1 / max(dataS$pop1) * 80,  # 点大小
    opacity = 1  # 确保点完全填充
  )
)
# 设置布局
fig <- fig %>% layout(
  title = "3D Scatter Plot",
  scene = list(
    xaxis = list(title = "Greenness"),
    yaxis = list(title = "PM25"),
    zaxis = list(title = "CO2")
  )
)
# 显示图形
fig
saveWidget(fig, "3DScatterPlot.html", selfcontained = TRUE)
browseURL("3DScatterPlot.html")



# 假设 dataS 是数据框，包含 GR_Change、TheilPM25_Change 和 CO2_per_capita2015_1990 列
# 首先根据象限划分数据
split_data <- function(data) {
  data$quadrant <- with(data, 
                        ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 1,
                               ifelse(GR_Change >= 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 2,
                                      ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 3,
                                             ifelse(GR_Change >= 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 < 0, 4,
                                                    ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 >= 0, 5,
                                                           ifelse(GR_Change < 0 & TheilPM25_Change >= 0 & CO2_per_capita2015_1990 < 0, 6,
                                                                  ifelse(GR_Change < 0 & TheilPM25_Change < 0 & CO2_per_capita2015_1990 >= 0, 7,
                                                                         8))))))))
  return(data)
}
# 将数据分成8个象限
dataS <- split_data(dataS)
# 获取所有象限的唯一值
quadrants <- unique(dataS$quadrant)
# 自定义视角配置（每个象限一个固定视角）
camera_views <- list(
  list(eye = list(x = 1.5, y = 1.5, z = 1.5)),  # 象限1视角
  list(eye = list(x = -1.5, y = 1.5, z = 1.5)), # 象限2视角
  list(eye = list(x = 1.5, y = -1.5, z = 1.5)), # 象限3视角
  list(eye = list(x = -1.5, y = -1.5, z = 1.5)),# 象限4视角
  list(eye = list(x = 1.5, y = 1.5, z = -1.5)), # 象限5视角
  list(eye = list(x = -1.5, y = 1.5, z = -1.5)),# 象限6视角
  list(eye = list(x = 1.5, y = -1.5, z = -1.5)),# 象限7视角
  list(eye = list(x = -1.5, y = -1.5, z = -1.5)) # 象限8视角
)

# 循环生成三维图
for (q in quadrants) {
  # 筛选当前象限的数据
  data_quadrant <- subset(dataS, quadrant == q)
  
  # 创建三维交互式散点图
  fig <- plot_ly(
    data = data_quadrant,
    x = ~GR_Change,
    y = ~TheilPM25_Change,
    z = ~CO2_per_capita2015_1990,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      color = ~mapped_colors,  
      size = data_quadrant$pop1 / max(dataS$pop1, na.rm = TRUE) * 80,  # 确保点大小正确缩放
      opacity = 1  # 确保点完全填充
    )
  )
  
  # 设置布局
  fig <- fig %>% layout(
    title = paste("3D Scatter Plot - Quadrant", q),
    scene = list(
      xaxis = list(title = "Greenness"),
      yaxis = list(title = "PM25"),
      zaxis = list(title = "CO2"),
      camera = camera_views[[q]]  # 使用对应象限的视角
    )
  )
  
  # 保存为HTML文件
  filename <- paste0("3DScatterPlot_Quadrant", q, ".html")
  saveWidget(fig, filename, selfcontained = TRUE)
  
  # 打开生成的图形（可选）
  browseURL(filename)
}




# 画八象限密度分布图
# 创建象限标签
quadrants <- character(nrow(dataS))
quadrants[dataS$quadrant == 1] <- "Quadrant Ⅰ"
quadrants[dataS$quadrant == 2] <- "Quadrant Ⅱ"
quadrants[dataS$quadrant == 3] <- "Quadrant Ⅲ"
quadrants[dataS$quadrant == 4] <- "Quadrant Ⅳ"
quadrants[dataS$quadrant == 5] <- "Quadrant Ⅴ"
quadrants[dataS$quadrant == 6] <- "Quadrant Ⅵ"
quadrants[dataS$quadrant == 7] <- "Quadrant Ⅶ"
quadrants[dataS$quadrant == 8] <- "Quadrant Ⅷ"
dataS$Quadrant <- quadrants

# 统计每个象限的个数及比例
quadrant_counts <- table(dataS$Quadrant)
total_count <- sum(quadrant_counts)
quadrant_proportions <- quadrant_counts / total_count
print(quadrant_proportions)

# 更新 ggplot 部分的颜色与标签
colors <- c("#549F9A", "#074166", "#C30078", "#9A5747", "#A0C4FF", "#FFC6A5", "#FFADAD", "#9BF6FF")
quadrant_labels <- c("Quadrant Ⅰ", "Quadrant Ⅱ", "Quadrant Ⅲ", "Quadrant Ⅳ", 
                     "Quadrant Ⅴ", "Quadrant Ⅵ", "Quadrant Ⅶ", "Quadrant Ⅷ")
# GDP   计算所有象限的全局最大 y 值
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           xlim = c(0, 10000), ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataS$GDP_per_capita2015[dataS$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")


# 人口   计算所有象限的全局最大 y 值
# 对 pop1 取对数，处理长尾分布（避免 log(0) 的问题，增加一个小值）
dataS$log_pop1 <- log(dataS$pop1 + 1)
all_y_max <- 0
for (i in 1:length(quadrant_labels)) {
  data_subset <- na.omit(as.numeric(dataS$log_pop1[dataS$Quadrant == quadrant_labels[i]]))
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    all_y_max <- max(all_y_max, max(density_data$y))
  }
}
# 绘制密度图
for (i in 1:length(quadrant_labels)) {
  # 提取属于特定象限的 GDP per capita 数据，并移除 NA
  data_subset <- na.omit(as.numeric(dataS$log_pop1[dataS$Quadrant == quadrant_labels[i]]))
  
  # 仅当有有效数据时才计算密度
  if (length(data_subset) > 1) {
    density_data <- density(data_subset)
    if (i == 1) {
      plot(density_data, col = colors[i], lwd = 2, main = "",
           xlab = "", #GDP per capita (thousand US$)
           ylab = "Density", 
           # xlim = c(11, 12),
           ylim = c(0, all_y_max),
           cex.lab = 1.2)  # 设置x轴范围和y轴范围，并调整标签大小
    } else {
      lines(density_data, col = colors[i], lwd = 2)
    }
  }
}
# 添加垂直线，表示各象限的中位数
for (i in 1:length(quadrant_labels)) {
  median_value <- median(dataS$log_pop1[dataS$Quadrant == quadrant_labels[i]], na.rm = TRUE)
  abline(v = median_value, col = colors[i], lty = 2, lwd = 1.5)
}
# 添加图例
legend("topright", legend = quadrant_labels, col = colors, lwd = 2, bty = "n")



















# 三角图
library(ggtern)
# 只摘抄需要的属性进行合并
columns_to_select <- c("CO2_per_capita", "PM25_per_capita", "GR1", "CO2_per_capita2000", "PM25_per_capita2000", "GR2","GCPNT_LON","GCPNT_LAT","pop1")
dataN <- dataN[, columns_to_select]
dataS <- dataS[, columns_to_select]

# 合并两个数据集并添加类别信息
dataN$category <- "Global North"
dataS$category <- "Global South"
data_combined <- rbind(dataN, dataS)

# 归一化处理并计算A, B, C的占比
data_combined$A1 <- (data_combined$CO2_per_capita - min(data_combined$CO2_per_capita)) / (max(data_combined$CO2_per_capita) - min(data_combined$CO2_per_capita))
data_combined$B1 <- (data_combined$PM25_per_capita - min(data_combined$PM25_per_capita)) / (max(data_combined$PM25_per_capita) - min(data_combined$PM25_per_capita))
data_combined$C1 <- (data_combined$GR1 - min(data_combined$GR1)) / (max(data_combined$GR1) - min(data_combined$GR1))

# 计算每个记录的总和
data_combined$total <- data_combined$A1 + data_combined$B1 + data_combined$C1

# 计算每个变量在总和中的占比
data_combined$A <- data_combined$A1 / data_combined$total
data_combined$B <- data_combined$B1 / data_combined$total
data_combined$C <- data_combined$C1 / data_combined$total

# 删除非有限值的行
data_combined <- data_combined[is.finite(data_combined$A) & is.finite(data_combined$B) & is.finite(data_combined$C), ]
# 辅助函数用于添加垂线的多个段
add_segment <- function(p, x_start, y_start, z_start, x_end, y_end, z_end, n_segments = 10) {
  for (i in seq_len(n_segments)) {
    alpha_value <- 0.9 - (i - 1) * 0.08  # 透明度递减
    p <- p + annotate("segment",
                      x = x_start + (x_end - x_start) * (i - 1) / n_segments,
                      y = y_start + (y_end - y_start) * (i - 1) / n_segments,
                      z = z_start + (z_end - z_start) * (i - 1) / n_segments,
                      xend = x_start + (x_end - x_start) * i / n_segments,
                      yend = y_start + (y_end - y_start) * i / n_segments,
                      zend = z_start + (z_end - z_start) * i / n_segments,
                      color = "grey", alpha = alpha_value, size = 1.2)
  }
  return(p)
}

# 使用类别信息绘制三元图
p <- ggtern(data = data_combined, aes(x = A, y = B, z = C, color = category)) +
  geom_point(aes(size = total), alpha = 0.3) +  # 根据类别绘制不同颜色的点
  labs(title = " ",
       T = "CO2",
       L = "PM25",
       R = "GREEN") +
  theme_bw() +
  theme_hidegrid()  # 去掉内部格网


# # 使用类别信息绘制三元图
# p <- ggtern(data = data_combined, aes(x = A, y = B, z = C, color = category)) +
#   geom_point(aes(size = total), alpha = 0.3) +  # 根据类别绘制不同颜色的点
#   stat_density_tern(aes(fill = after_stat(level)), geom = 'polygon', alpha = 0.3, bdl = 0.01) +
#   labs(title = " ",
#        T = "CO2",
#        L = "PM25",
#        R = "GREEN") +
#   theme_bw() +
#   theme_hidegrid()  # 去掉内部格网


# 添加三条垂线
p <- add_segment(p, x_start = 1, y_start = 0, z_start = 0, x_end = 0, y_end = 0.5, z_end = 0.5)  # 从顶点 A 到对边 BC 的垂线
p <- add_segment(p, x_start = 0, y_start = 1, z_start = 0, x_end = 0.5, y_end = 0, z_end = 0.5)  # 从顶点 B 到对边 AC 的垂线
p <- add_segment(p, x_start = 0, y_start = 0, z_start = 1, x_end = 0.5, y_end = 0.5, z_end = 0)  # 从顶点 C 到对边 AB 的垂线

# 显示绘图
p




library(ggtern)
# 只摘抄需要的属性进行合并
columns_to_select <- c("CO2_per_capita", "PM25_per_capita", "GR1", "CO2_per_capita2000", "PM25_per_capita2000", "GR2","GCPNT_LON","GCPNT_LAT","pop1")
dataN <- dataN[, columns_to_select]
dataS <- dataS[, columns_to_select]

# 合并两个数据集并添加类别信息
dataN$category <- "Global North"
dataS$category <- "Global South"
data_combined <- rbind(dataN, dataS)

# 归一化处理并计算A, B, C的占比
# data_combined$A1 <- (data_combined$CO2_per_capita - min(data_combined$CO2_per_capita)) / (max(data_combined$CO2_per_capita) - min(data_combined$CO2_per_capita))
data_combined$A1 <- (log(data_combined$CO2_per_capita) - min(log(data_combined$CO2_per_capita))) / (max(log(data_combined$CO2_per_capita)) - min(log(data_combined$CO2_per_capita)))
data_combined$B1 <- (data_combined$PM25_per_capita - min(data_combined$PM25_per_capita)) / (max(data_combined$PM25_per_capita) - min(data_combined$PM25_per_capita))
data_combined$C1 <- (data_combined$GR1 - min(data_combined$GR1)) / (max(data_combined$GR1) - min(data_combined$GR1))

# 计算每个记录的总和
data_combined$total <- data_combined$A1 + data_combined$B1 + data_combined$C1

# 计算每个变量在总和中的占比
data_combined$A <- data_combined$A1 / data_combined$total
data_combined$B <- data_combined$B1 / data_combined$total
data_combined$C <- data_combined$C1 / data_combined$total

# 定义分类函数，基于均值线划分类型
classify_points <- function(row, means) {
  A_mean <- means["A"]
  B_mean <- means["B"]
  C_mean <- means["C"]
  if (row["A"] >= A_mean && row["B"] < B_mean && row["C"] < C_mean) {
    return("高碳区")
  } else if (row["A"] < A_mean && row["B"] >= B_mean && row["C"] < C_mean) {
    return("高污染区")
  } else if (row["A"] < A_mean && row["B"] < B_mean && row["C"] >= C_mean) {
    return("高绿化度区")
  } else if (row["A"] >= A_mean && row["B"] >= B_mean && row["C"] < C_mean) {
    return("低绿碳污平衡区")
  } else if (row["A"] >= A_mean && row["C"] >= C_mean && row["B"] < B_mean) {
    return("低污染碳绿平衡区")
  } else {
    return("低碳绿污平衡区")
  }
}
# 过滤掉包含非有限值的数据
data_combined_filtered <- data_combined[is.finite(data_combined$A) & is.finite(data_combined$B) & is.finite(data_combined$C), ]
# 对每个类别（南方和北方）分别分类
data_combined_filtered$class <- NA
categories <- unique(data_combined_filtered$category)
# 计算全球北方和南方的均值并添加类别标签
global_means <- aggregate(. ~ category, data_combined, mean)
for (cat in categories) {
  cat_means <- global_means[global_means$category == cat, ]
  data_combined_filtered$class[data_combined_filtered$category == cat] <- 
    apply(data_combined_filtered[data_combined_filtered$category == cat, c("A", "B", "C")], 1, classify_points, cat_means)
}
# 分别计算全球南方和北方的每种类型占比
type_counts_by_category <- table(data_combined_filtered$category, data_combined_filtered$class)
type_percentages_by_category <- prop.table(type_counts_by_category, margin = 1) * 100
# 输出占比结果
print(type_percentages_by_category)


# # 使用类别信息绘制三元图并分面
# p <- ggtern(data = data_combined, aes(x = A, y = B, z = C, color = category)) +
#   geom_point(alpha = 0.3) +  # 根据类别绘制不同颜色的点size = total  aes(size = 0.01), 
#   stat_density_tern(aes(fill = ..level..), geom = "polygon", alpha = 0.2) +  # 添加点密度等高线
#   scale_fill_viridis_c() +  # 使用 Viridis 色标来显示等高线
#   labs(title = " ",
#        T = "PM",
#        L = "CO2",
#        R = "GR") +
#   theme_bw() +
#   theme_showgrid() +  # 显示内部格网theme_hidegrid() +  # 去掉内部格网
#   facet_wrap(~category, ncol = 2)  # 左右并列显示全球北方和南方

# 使用类别信息绘制三元图并分面，并加上点密度等高线
p <- ggtern(data = data_combined, aes(x = A, y = B, z = C, color = category)) +
  geom_point(shape = 16, alpha = 0.3, size = 0.05) +  # 使用实心圆，并将点的大小进一步减小
  stat_density_tern(aes(fill = ..level..), geom = "polygon", alpha = 0.2, bins = 10) +  # 添加点密度等高线，调整等高线的数量为10
  scale_fill_viridis_c(direction = -1) + scale_color_viridis_c(direction = -1) +  # 反转颜色等级，使得密度高的颜色深
  labs(title = " ",
       T = "PM",
       L = "CO2",
       R = "GR") +
  theme_bw() +
  theme_showgrid() +  # 显示内部格网
  facet_wrap(~category, ncol = 2) +  # 左右并列显示全球北方和南方
  guides(color = guide_legend(override.aes = list(size = 3), order = 1), fill = guide_legend(order = 2), linetype = guide_legend(order = 3))  # 分开绘制点和线图例

# 过滤掉包含非有限值的数据
# data_combined_filtered <- data_combined[is.finite(data_combined$A) & is.finite(data_combined$B) & is.finite(data_combined$C), ]
# 使用类别信息绘制三元图并分面，并加上点密度等高线
p <- ggtern(data = data_combined_filtered, aes(x = A, y = B, z = C, color = category)) +
  stat_density_tern(aes(fill = ..level..), geom = "polygon", alpha = 0.2, bins = 5, bdl = 0.01) +  # 添加点密度等高线，调整等高线的数量为10
  scale_fill_gradient(low = "lightgreen", high = "darkblue") +  # 反转颜色等级，使得密度高的颜色深
  geom_point(shape = 16, alpha = 0.3, size = 0.05) +  # 使用实心圆，并将点的大小进一步减小
  labs(title = " ",
       T = "PM",
       L = "CO2",
       R = "GR") +
  theme_bw() +
  theme_showgrid() +  # 显示内部格网
  facet_wrap(~category, ncol = 2) +  # 左右并列显示全球北方和南方
  guides(color = guide_legend(override.aes = list(size = 3), order = 1), fill = guide_legend(order = 2), linetype = guide_legend(order = 3))  # 分开绘制点和线图例

# 打印图表
print(p)

# 计算全球北方和南方的均值并添加类别标签
global_means <- aggregate(. ~ category, data_combined, mean)
# 为每个类别添加均值线
p <- p +
  geom_segment(
    data = global_means,
    aes(x = A, y = 0, z = 1 - A, xend = A, yend = 1 - A, zend = 0, linetype = category),
    color = "#DD7C4F", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = B, z = 1 - B, xend = 1 - B, yend = B, zend = 0, linetype = category),
    color = "#6C61AF", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = 1 - C, z = C, xend = 1 - C, yend = 0, zend = C, linetype = category),
    color = "#629C35", size = 1
  )

# 显示绘图
p


# 使用类别信息绘制三元图并分面，并加上点密度等高线
p <- ggtern(data = data_combined_filtered, aes(x = A, y = B, z = C, color = category)) +
  geom_segment(  # 将均值线放在最底层
    data = global_means,
    aes(x = A, y = 0, z = 1 - A, xend = A, yend = 1 - A, zend = 0, linetype = category),
    color = "#074166", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = B, z = 1 - B, xend = 1 - B, yend = B, zend = 0, linetype = category),
    color = "#6C61AF", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = 1 - C, z = C, xend = 1 - C, yend = 0, zend = C, linetype = category),
    color = "#CC011F", size = 1
  ) +
  geom_point(shape = 16, alpha = 0.3, size = 0.05) +  # 使用实心圆，并将点的大小进一步减小
  stat_density_tern(aes(fill = ..level..), geom = "polygon", alpha = 0.1, bins = 5, bdl = 0.01) +  # 添加点密度等高线，调整等高线的数量为10
  scale_fill_gradient(low = "lightgreen", high = "darkblue") +  # 反转颜色等级，使得密度高的颜色深
  labs(title = " ",
       T = "PM",
       L = "CO2",
       R = "GR") +
  theme_bw() +
  theme_showgrid() +  # 显示内部格网
  facet_wrap(~category, ncol = 2) +  # 左右并列显示全球北方和南方
  guides(color = guide_legend(override.aes = list(size = 3), order = 1), fill = guide_legend(order = 2), linetype = guide_legend(order = 3))  # 分开绘制点和线图例
# 打印图表
print(p)













# 等值线10内面积
library(ggtern)
library(sp)

# 确保过滤掉非有限值的数据
data_combined_filtered <- data_combined_filtered[is.finite(data_combined_filtered$A) & 
                                                   is.finite(data_combined_filtered$B) & 
                                                   is.finite(data_combined_filtered$C), ]

# 按类别分组
categories <- unique(data_combined_filtered$category)
areas_by_category <- list()  # 用于存储每个类别的面积结果

for (cat in categories) {
  # 选择当前类别数据
  data_category <- subset(data_combined_filtered, category == cat)
  
  # 设置 bdl 参数并绘制三元图
  density_data <- ggtern(data = data_category, aes(x = A, y = B, z = C)) +
    stat_density_tern(
      aes(fill = ..level..), 
      geom = "polygon", 
      alpha = 0.2, 
      bins = 10, 
      bdl = 0.01  # 设置检测下限
    ) +
    labs(title = paste("Category:", cat),
         T = "PM",
         L = "CO2",
         R = "GR") +
    theme_bw()
  
  # 提取等值线数据
  density_layer <- ggplot_build(density_data)$data[[1]]
  
  # 检查并过滤非有限值
  density_layer <- density_layer[is.finite(density_layer$x) & is.finite(density_layer$y), ]
  
  # 过滤等值线10以内的数据
  density_filtered <- density_layer[density_layer$level <= 10, ]
  
  # 检查过滤后的数据是否为空
  if (nrow(density_filtered) > 0) {
    # 按组分割数据
    density_filtered_split <- split(density_filtered, density_filtered$group)
    
    # 定义函数：按组计算多边形面积
    calculate_polygon_area <- function(group_data) {
      if (nrow(group_data) < 3) return(0)  # 至少需要3个点形成多边形
      poly <- Polygon(cbind(group_data$x, group_data$y))
      return(poly@area)
    }
    
    # 计算每组面积
    areas <- sapply(density_filtered_split, calculate_polygon_area)
    
    # 汇总总面积
    total_area <- sum(areas, na.rm = TRUE)
    areas_by_category[[cat]] <- total_area
  } else {
    areas_by_category[[cat]] <- 0  # 如果没有数据点，面积为0
  }
}

# 打印每个类别的结果
for (cat in names(areas_by_category)) {
  print(paste(cat, "等值线10以内的总面积为：", areas_by_category[[cat]]))
}








# 分别筛选类别为 "A" 和 "B" 的数据
dataN <- data_combined_filtered[data_combined_filtered$category == "Global North", ]
dataS <- data_combined_filtered[data_combined_filtered$category == "Global South", ]
# 保存现在的数据库，制作全球地图NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
library(tidyverse)
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),  # 仅使用 map_id 匹配地图区域
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # coord_quickmap() +  # 添加适当的地图投影，确保比例正确
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +  # 固定比例，设置坐标范围
  geom_point(
    data = dataN,
    aes(
      x = GCPNT_LON, 
      y = GCPNT_LAT, 
      color = class, 
      size = pop1 / ZuiDaPop * 10  # 在 aes() 外部引用 dataS$pop1
    ),
    alpha = 0.5
  ) +
  labs(
    size = "Population", # 修改颜色图例名称
    # color = "象限分类"     # 修改大小图例名称
  ) +
  # scale_color_brewer(palette = "Set2") +  # 设置颜色方案
  scale_color_manual(values = c("#F8984F","#FCDC89","#A3C9D5","#329845","#912C2C" ,"#276C9E" )) +  # 自定义颜色
  # scale_color_manual(values = c("#66BC98", "#D96558", "#AAD09D","#EFA143","#E3EA96" ,"#B43970","#FCDC89","#692F7C")) +
  theme_minimal()



ggplot() +
  geom_map(
    data = world, map = world,
    aes(map_id = region),  # 仅使用 map_id 匹配地图区域
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # coord_quickmap() +  # 添加适当的地图投影，确保比例正确
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +  # 固定比例，设置坐标范围
  geom_point(
    data = dataS,
    aes(
      x = GCPNT_LON, 
      y = GCPNT_LAT, 
      color = class, 
      size = pop1 / ZuiDaPop * 10  # 在 aes() 外部引用 dataS$pop1
    ),
    alpha = 0.5
  ) +
  labs(
    size = "Population", # 修改颜色图例名称
    # color = "象限分类"     # 修改大小图例名称
  ) +
  # scale_color_brewer(palette = "Set2") +  # 设置颜色方案
  scale_color_manual(values = c("#F8984F","#FCDC89","#A3C9D5","#329845","#912C2C" ,"#276C9E" )) +  # 自定义颜色
  # scale_color_manual(values = c("#66BC98", "#D96558", "#AAD09D","#EFA143","#E3EA96" ,"#B43970","#FCDC89","#692F7C")) +
  theme_minimal()








# # 全球计算合并均值
# global_means <- colMeans(data_combined[, c("A", "B", "C")])
# # 更新分类函数，基于合并均值分类
# classify_points <- function(row) {
#   A_mean <- global_means["A"]
#   B_mean <- global_means["B"]
#   C_mean <- global_means["C"]
#   if (row["A"] >= A_mean && row["B"] < B_mean && row["C"] < C_mean) {
#     return("高碳区")
#   } else if (row["A"] < A_mean && row["B"] >= B_mean && row["C"] < C_mean) {
#     return("高污染区")
#   } else if (row["A"] < A_mean && row["B"] < B_mean && row["C"] >= C_mean) {
#     return("高绿化度区")
#   } else if (row["A"] >= A_mean && row["B"] >= B_mean && row["C"] < C_mean) {
#     return("低绿碳污平衡区")
#   } else if (row["A"] >= A_mean && row["C"] >= C_mean && row["B"] < B_mean) {
#     return("低污染碳绿平衡区")
#   } else {
#     return("低碳绿污平衡区")
#   }
# }
# 
# # 对每一行分类
# data_combined$class <- apply(data_combined[, c("A", "B", "C")], 1, classify_points)
# # 计算每个类别的类型占比
# type_counts_by_category <- table(data_combined$category, data_combined$class)
# type_percentages_by_category <- prop.table(type_counts_by_category, margin = 1) * 100
# print(type_percentages_by_category)
# # 计算每个类别的类型占比
# total_counts <- sum(type_counts_by_category)
# type_percentages_by_category <- type_counts_by_category / total_counts * 100
# type_percentages_by_category
# row_totals <- rowSums(type_percentages_by_category)
# row_totals
# # 计算合并均值
# global_means <- colMeans(data_combined_filtered[, c("A", "B", "C")], na.rm = TRUE)
# 
# # 绘制三元图
# p <- ggtern(data = data_combined_filtered, aes(x = A, y = B, z = C, color = category)) +
#   geom_segment(  # 添加均值线 (A)
#     aes(x = global_means["A"], y = 0, z = 1 - global_means["A"], 
#         xend = global_means["A"], yend = 1 - global_means["A"], zend = 0),
#     color = "#DD7C4F", size = 1
#   ) +
#   geom_segment(  # 添加均值线 (B)
#     aes(x = 0, y = global_means["B"], z = 1 - global_means["B"], 
#         xend = 1 - global_means["B"], yend = global_means["B"], zend = 0),
#     color = "#6C61AF", size = 1
#   ) +
#   geom_segment(  # 添加均值线 (C)
#     aes(x = 0, y = 1 - global_means["C"], z = global_means["C"], 
#         xend = 1 - global_means["C"], yend = 0, zend = global_means["C"]),
#     color = "#629C35", size = 1
#   ) +
#   geom_point(shape = 16, alpha = 0.3, size = 0.05) +  # 使用实心圆，减小点的大小
#   stat_density_tern(aes(fill = ..level..), geom = "polygon", alpha = 0.1, bins = 5, bdl = 0.01) +  # 添加点密度等高线
#   scale_fill_gradient(low = "lightgreen", high = "darkblue") +  # 渐变填充
#   labs(title = " ",
#        T = "PM",
#        L = "CO2",
#        R = "GR") +
#   theme_bw() +
#   theme_showgrid() +  # 显示内部网格
#   facet_wrap(~category, ncol = 2) +  # 根据类别分面
#   guides(color = guide_legend(override.aes = list(size = 3), order = 1), 
#          fill = guide_legend(order = 2), 
#          linetype = guide_legend(order = 3))  # 图例设置
# 
# # 显示绘图
# print(p)
# 
# 
# 
# 
# 
# 

# 创建一个用于填充背景的三角形数据框
triangle_fill <- data.frame(
  A = c(0, 1, 0),
  B = c(0, 0, 1),
  C = c(1, 0, 0)
)

p <- ggtern(data = data_combined, aes(x = A, y = B, z = C)) +
  geom_polygon(data = triangle_fill, aes(x = A, y = B, z = C), 
               fill = "lightgray", color = "gray", alpha = 0.5) +  # 填充浅灰色
  labs(title = " ",
       T = "PM",
       L = "CO2",
       R = "GR") +
  theme_bw() +
  theme_showgrid()   # 显示内部格网
  # theme_hidegrid()
# p <- ggtern(data = data_combined, aes(x = A, y = B, z = C)) +
#   # geom_point(aes(size = total), alpha = 0.3) +  # 根据类别绘制不同颜色的点
#   labs(title = " ",
#        T = "PM",
#        L = "CO2",
#        R = "GR") +
#   theme_bw() +
#   theme_showgrid()   # 显示内部格网theme_hidegrid() +  # 去掉内部格网
# 为每个类别添加均值线
p <- p +
  geom_segment(
    data = global_means,
    aes(x =0.333, y = 0, z = 0.667, xend = 0.333, yend = 0.667, zend = 0, linetype = "solid"),
    color = "#074166", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = 0.333, z = 0.667, xend = 0.667, yend = 0.333, zend = 0, linetype = "solid"),
    color = "#6C61AF", size = 1
  ) +
  geom_segment(
    data = global_means,
    aes(x = 0, y = 0.667, z = 0.333, xend = 0.667, yend = 0, zend = 0.333, linetype = "solid"),
    color = "#CC011F", size = 1
  )
# 手动绘制几个已知点来检查坐标轴是否正确
p
# 
# 
# 
# 
# 
# 
# 
# data_column<-data_combined$B
# hist(data_column, 
#      main = "数据列的直方图",  # 设置直方图的标题
#      xlab = "值",             # 设置x轴标签
#      ylab = "频数",           # 设置y轴标签
#      col = "lightblue",      # 设置直方图的颜色
#      border = "black")       # 设置柱边框颜色
# 
# 
# 
# 
# 
# data_combined <- data_combined[(data_combined$GR2 - min(data_combined$GR2)) != 0, ]
# data_combined <- data_combined[(log(data_combined$CO2_per_capita2000) - min(log(data_combined$CO2_per_capita2000))) != 0, ]
# 
# # 归一化处理并计算A, B, C的占比
# # data_combined$A2 <- (data_combined$CO2_per_capita2000 - min(data_combined$CO2_per_capita2000)) / (max(data_combined$CO2_per_capita2000) - min(data_combined$CO2_per_capita2000))
# 
# data_combined$A2 <- (log(data_combined$CO2_per_capita2000) - min(log(data_combined$CO2_per_capita2000))) / (max(log(data_combined$CO2_per_capita200)) - min(log(data_combined$CO2_per_capita2000)))
# data_combined$B2 <- (data_combined$PM25_per_capita2000 - min(data_combined$PM25_per_capita2000)) / (max(data_combined$PM25_per_capita2000) - min(data_combined$PM25_per_capita2000))
# data_combined$C2 <- (data_combined$GR2 - min(data_combined$GR2)) / (max(data_combined$GR2) - min(data_combined$GR2))
# 
# 
# 
# # data_combined$A2 <- (data_combined$CO2_per_capita2000 - min(data_combined$CO2_per_capita)) / (max(data_combined$CO2_per_capita) - min(data_combined$CO2_per_capita))
# # data_combined$B2 <- (data_combined$PM25_per_capita2000 - min(data_combined$PM25_per_capita)) / (max(data_combined$PM25_per_capita2000) - min(data_combined$PM25_per_capita))
# # data_combined$C2 <- (data_combined$GR2 - min(data_combined$GR2)) / (max(data_combined$GR1) - min(data_combined$GR2))
# 
# # 计算每个记录的总和
# data_combined$total2 <- data_combined$A2 + data_combined$B2 + data_combined$C2
# 
# # 计算每个变量在总和中的占比
# data_combined$AA <- data_combined$A2 / data_combined$total2
# data_combined$BB <- data_combined$B2 / data_combined$total2
# data_combined$CC <- data_combined$C2 / data_combined$total2
# 
# 
# # 辅助函数用于添加垂线的多个段
# add_segment <- function(p, x_start, y_start, z_start, x_end, y_end, z_end, n_segments = 10) {
#   for (i in seq_len(n_segments)) {
#     alpha_value <- 0.9 - (i - 1) * 0.08  # 透明度递减
#     p <- p + annotate("segment",
#                       x = x_start + (x_end - x_start) * (i - 1) / n_segments,
#                       y = y_start + (y_end - y_start) * (i - 1) / n_segments,
#                       z = z_start + (z_end - z_start) * (i - 1) / n_segments,
#                       xend = x_start + (x_end - x_start) * i / n_segments,
#                       yend = y_start + (y_end - y_start) * i / n_segments,
#                       zend = z_start + (z_end - z_start) * i / n_segments,
#                       color = "grey", alpha = alpha_value, size = 1.2)
#   }
#   return(p)
# }
# 
# # 使用类别信息绘制三元图并分面
# p <- ggtern(data = data_combined, aes(x = AA, y = BB, z = CC, color = category)) +
#   geom_point(aes(size = total2), alpha = 0.3) +  # 根据类别绘制不同颜色的点
#   labs(title = " ",
#        T = "CO2",
#        L = "PM",
#        R = "GR") +
#   theme_bw() +
#   theme_hidegrid() +  # 去掉内部格网
#   facet_wrap(~category, ncol = 2)  # 左右并列显示全球北方和南方
# 
# # 添加三条垂线
# p <- add_segment(p, x_start = 1, y_start = 0, z_start = 0, x_end = 0, y_end = 0.5, z_end = 0.5)  # 从顶点 A 到对边 BC 的垂线
# p <- add_segment(p, x_start = 0, y_start = 1, z_start = 0, x_end = 0.5, y_end = 0, z_end = 0.5)  # 从顶点 B 到对边 AC 的垂线
# p <- add_segment(p, x_start = 0, y_start = 0, z_start = 1, x_end = 0.5, y_end = 0.5, z_end = 0)  # 从顶点 C 到对边 AB 的垂线
# 
# # 计算全球北方和南方的均值并添加类别标签
# global_means <- aggregate(. ~ category, data_combined, mean)
# p <- p +
#   geom_segment(
#     data = global_means,
#     aes(x = AA, y = 0, z = 1 - AA, xend = AA, yend = 1 - AA, zend = 0, linetype = category),
#     color = "blue", size = 1
#   ) +
#   geom_segment(
#     data = global_means,
#     aes(x = 0, y = BB, z = 1 - BB, xend = 1 - BB, yend = BB, zend = 0, linetype = category),
#     color = "red", size = 1
#   ) +
#   geom_segment(
#     data = global_means,
#     aes(x = 0, y = 1 - CC, z = CC, xend = 1 - CC, yend = 0, zend = CC, linetype = category),
#     color = "green", size = 1
#   )
# 
# # 显示绘图
# p
# 

