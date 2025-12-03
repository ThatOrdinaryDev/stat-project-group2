library(dplyr)
library(ggplot2)

#code for all platform global sales
general_platform_sales <- vgsales %>% group_by(Platform) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales))
ggplot(general_platform_sales, aes(x = reorder(Platform, - Tot_Global_Sales), y = Tot_Global_Sales)) + geom_col(fill = "cyan2") + geom_text(aes(label = round(Tot_Global_Sales, 2)), vjust = -0.3, size = 4)+ labs(title = "Sales by platform", x = "Platform", y = "Total Global Sales(Million)")

#code for 10 platform with highest global sales
top_ten_sale <- vgsales %>% group_by(Platform) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales)) %>% slice_head(n = 10)
ggplot(top_ten_sale, aes(x = reorder(Platform, - Tot_Global_Sales), y = Tot_Global_Sales)) + geom_col(fill = "gold") + geom_text(aes(label = round(Tot_Global_Sales, 2)), hjust = -0.1, size = 4)+ labs(title = "Top 10 platform with highest global sales", x = "Platform", y = "Total Global Sales(Million)") + theme_light() + coord_flip()

#code for all genre global sales
general_genre_sales <- vgsales %>% group_by(Genre) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales))
ggplot(general_genre_sales, aes(x = reorder(Genre, - Tot_Global_Sales), y = Tot_Global_Sales)) + geom_col(fill = "goldenrod3") + geom_text(aes(label = round(Tot_Global_Sales, 2)), vjust = -0.3, size = 6)+ labs(title = "Sales by genre", x = "Genre", y = "Total Global Sales(Million)") + theme_light()

#code for top 10 genre with highest global sales
top_genre_sale <- vgsales %>% group_by(Genre) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales)) %>% slice_head(n = 10)
ggplot(top_genre_sale, aes(x = reorder(Genre, - Tot_Global_Sales), y = Tot_Global_Sales)) + geom_col(fill = "gold") + geom_text(aes(label = round(Tot_Global_Sales, 2)), hjust = -0.1, size = 3) + labs(title = "Top 10 Genre with highest global sales", x = "Genre", y = "Total Global Sales(Million)") + theme_minimal() +coord_flip()

#code for top 10 publishers with highest global sales
top_publisher_sale <- vgsales %>% group_by(Publisher) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales)) %>% slice_head(n = 10)
ggplot(top_publisher_sale, aes(x = reorder(Publisher, - Tot_Global_Sales), y = Tot_Global_Sales)) + geom_col(fill = "gold") + geom_text(aes(label = round(Tot_Global_Sales, 2)), hjust = -0.1, size = 4)+ labs(title = "Top 10 publishers with highest global sales", x = "Publisher", y = "Total Global Sales(Million)") + theme_light() + coord_flip()

#total title released by platform
title_count <- vgsales %>% group_by(Platform) %>% summarise(total_title = n()) %>% arrange(desc(total_title)) %>% slice_head(n = 30)
ggplot(title_count, aes(x = reorder(Platform, -total_title), y = total_title)) + geom_col(fill = "gold") + geom_text(aes(label = total_title), vjust = -0.3, size = 4) + labs(title = "Total titles released by platform", x = "Platform", y = "Total titles released") + theme_minimal()

#total title released by Publisher
publisher_total_titles <- vgsales %>% group_by(Publisher) %>% summarise(total_title = n()) %>% arrange(desc(total_title)) %>% slice_head(n = 30)
ggplot(publisher_total_titles, aes(x = reorder(Publisher, -total_title), y = total_title)) + geom_col(fill = "orange") + geom_text(aes(label = total_title), hjust = -0.3, size = 4) + labs(title = "Total titles released by Publisher", x = "Publisher", y = "Total titles released") + theme_light() + coord_flip()

#global sales differ across platforms and genres
plat_sales <- vgsales %>% group_by(Platform, Genre) %>% summarise(Tot_Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>% arrange(desc(Tot_Global_Sales))

#for sum of each genre across all platforms
print(plat_sales, n = 293)

#bar chart for global sales across platforms and genres
ggplot(plat_sales, aes(x = reorder(Platform, -Tot_Global_Sales), y = Tot_Global_Sales, fill = Genre)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "global sales differ across genres and platforms", x = "platform", y = "Global sales (Million)") + theme_light()