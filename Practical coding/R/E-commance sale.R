library(tidyverse)
library(emmeans)
library(janitor)
library(DoE.base)
library(FSA)
library(agricolae)
library(scales)
library(lubridate)

df <- read_csv("C:/Users/asus/Documents/Kaggle project/E-Commerce Business Analysis/ecommerce_sales_data.csv")

#-----------------------------------------------------------
#Profit Density Distribution by Region
ggplot(df, aes(x = Profit, fill = Region)) +
  geom_density(alpha = 0.4) + 
  theme_minimal() +
  labs(title = "Profit Density Distribution by Region (Before Cleaning)",
       x = "Profit",
       y = "Density") +
  facet_wrap(~`Region`)

#Profit Density Distribution by Product Name
ggplot(df, aes(x = Profit, fill = `Product Name`)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Profit Density Distribution by Product Name (Before Cleaning)",
       x = "Profit",
       y = "Density") +
  facet_wrap(~`Product Name`) 

#Profit Density Distribution by Category
ggplot(df, aes(x = Profit, fill = Category)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Profit Density Distribution by Category (Before Cleaning)",
       x = "Profit", 
       y = "Density") +
  facet_wrap(~`Category`)

#-----------------------------------------------------------
## Clean data
clean_df <- df %>%
  clean_names(.) %>%
  mutate(across(c(product_name, region, category), as.factor),
         across(c(sales, quantity, profit), as.numeric)) %>%
  drop_na() %>%
  mutate(profit_adj = log(profit + 1)) %>%
  mutate(sales_adj = log(sales + 1)) %>%
  mutate(Year_Quarter = paste0(year(order_date), "-Q", quarter(order_date)))

#-----------------------------------------------------------
#Distribution After Cleaning
ggplot(clean_df, aes(x = profit_adj, fill = region)) +
  geom_density(alpha = 0.4) + # ใช้ alpha เพื่อให้สีโปร่งแสงเห็นซ้อนกันได้
  theme_minimal() +
  labs(title = "Profit Density Distribution by Region (After Cleaning)",
       x = "Profit",
       y = "Density") +
  facet_wrap(~`region`)

ggplot(clean_df, aes(x = profit_adj, fill = product_name)) +
  geom_density(alpha = 0.4) + # ใช้ alpha เพื่อให้สีโปร่งแสงเห็นซ้อนกันได้
  theme_minimal() +
  labs(title = "Profit Density Distribution by product_name (After Cleaning)",
       x = "Profit",
       y = "Density") +
  facet_wrap(~`product_name`)

ggplot(clean_df, aes(x = profit_adj, fill = category)) +
  geom_density(alpha = 0.4) + # ใช้ alpha เพื่อให้สีโปร่งแสงเห็นซ้อนกันได้
  theme_minimal() +
  labs(title = "Profit Density Distribution by category (After Cleaning)",
       x = "Profit",
       y = "Density") +
  facet_wrap(~category)

#-----------------------------------------------------------
# General Graph
ggplot(clean_df, aes(x= category, y = profit, fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Electronics" = "#2793A9", 
                               "Accessories" = "#D9534F", 
                               "Office" = "#9B59B6")) +
  labs(title = "Category vs Profit",
       x = "Category",
       y = "Profit")

ggplot(clean_df, aes(x= category, y = quantity, fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Electronics" = "#2793A9", 
                               "Accessories" = "#D9534F", 
                               "Office" = "#9B59B6")) +
  labs(title = "Category vs Quantity",
       x = "Category",
       y = "Quantity")

ggplot(clean_df, aes(x= sales, y = profit, color = category)) +
  geom_point(alpha = 0.5, size = 2) +
  facet_wrap(~category) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_color_manual(values = c("Electronics" = "#2793A9", 
                                "Accessories" = "#D9534F", 
                                "Office" = "#9B59B6")) +
  labs(title = "Sales vs Profit",
       x = "sales",
       y = "Profit")

ggplot(clean_df, aes(x = Year_Quarter, y = profit, fill = category)) + # Changed color to fill
  geom_bar(stat = "identity") +
  facet_wrap(~category) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Electronics" = "#2793A9", 
                               "Accessories" = "#D9534F", 
                               "Office" = "#9B59B6")) +
  labs(title = "Quarterly Profit Performance",
       subtitle = "Smoothing daily volatility to see long-term growth",
       x = "Fiscal Quarter",
       y = "Total Profit ($)") +
  theme(legend.position = "none")

summary_quarter <- clean_df %>%
  group_by(Year_Quarter, category) %>%
  summarise( 
    TotalProfit = sum(profit, na.rm = TRUE),
    TotoalQuantity = sum(quantity), na.rm = TRUE,
    .groups = "drop") %>%
  group_by(category) %>%
  summarise(
    mean_profit = mean(TotalProfit),
    std_profit = sd(TotalProfit),
    var_profit = var(TotalProfit),
    CV = std_profit/mean_profit)

print(summary_quarter)

#-----------------------------------------------------------
#Analysis ANOVA and Duncan
profit_model <- aov(profit_adj ~ region * category * product_name, data = clean_df)
summary(profit_model)

duncan_region <- duncan.test(profit_model, "region", alpha = 0.05)
print(duncan_region)
plot(duncan_region,horiz=TRUE,las=1)

clean_df$group_combo <- interaction(clean_df$region, clean_df$category)

combo_model <- aov(profit_adj ~ group_combo, data = clean_df)
summary(combo_model)

duncan_combo <- duncan.test(combo_model,"group_combo", alpha = 0.05)
print(duncan_combo)
par(mar = c(5, 12, 4, 2))
plot(duncan_combo,horiz=TRUE,las=1)

ggplot(clean_df, aes(x = region, y = profit)) +
  geom_bar(stat = "identity", fill = "#2793A9") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Region vs Profit",
       x = "Region Sold",
       y = "Total Profit")

summary_product <- clean_df %>%
  group_by(group_combo) %>%
  summarise(
    Total_profit = sum(profit),
    Mean_profit = mean(profit),
    Totol_quantity = sum(quantity),
    AVG = Total_profit/Totol_quantity,
    std_profit = sd(profit),
    var_profit = var(profit),
    cv = std_profit/AVG) %>%
  select(group_combo, Total_profit, AVG, cv)

print(summary_product)

#-----------------------------------------------------------
#Correlation analysis
correlation_by_prod <- clean_df %>%
  group_by(product_name) %>%
  summarise(
    correlation = cor(quantity, profit, use = "complete.obs"),
    n_samples = n()
  ) %>%
  arrange(desc(correlation))

print(correlation_by_prod)

par(mar = c(5, 8, 4, 3))
interaction.plot(x.factor = clean_df$region, 
                 trace.factor = clean_df$category, 
                 response = clean_df$profit_adj,
                 fun = mean, 
                 type = "b", 
                 col = c("red", "blue", "green"),
                 pch = c(19, 17, 15),
                 fixed = TRUE, 
                 xlab = "Region", 
                 ylab = "Profit",
                 trace.label = "Category",
                 main = "Interaction: Region vs. Category")

ggplot(clean_df, aes(x = quantity, y = profit, color = product_name)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) + # เส้นแนวโน้มแยกตามกลุ่ม
  facet_wrap(~product_name) + # แยกเป็นคนละกรอบเพื่อความชัดเจน
  theme_minimal() +
  labs(title = "Quantity vs Profit: Relationship by Category",
       x = "Quantity Sold",
       y = "Log Profit")

