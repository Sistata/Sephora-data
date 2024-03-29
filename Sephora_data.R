rm(list = ls())
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gplots)
library(treemapify)
library(tidytext)
library(stringr)
# Load the data
sephora_data <- read.csv('sephora_website_dataset.csv', header = TRUE)
str(sephora_data)

#I decided to re-categorize the category into:
#Perfume, Hair, Body, Makeup, Skincare, Tools, Wellness and Others to make dataset more manageable

recatagorize <- function(x) {
  return(grepl(x, sephora_data$category, ignore.case = TRUE))
  }

data <- sephora_data %>%
  mutate(
    new_categories = case_when(
      recatagorize("Body") | recatagorize("Bath") | recatagorize("Scrub") | recatagorize("Shower") | recatagorize("Neck") | recatagorize("Hand") | recatagorize("Deodorant") ~ "Body",
      recatagorize("Fragrance") | recatagorize("Cologne") | recatagorize("Perfume") ~ "Perfume",
      recatagorize("Hair") | recatagorize("Shampoo") | recatagorize("Conditioner") ~ "Hair",
      recatagorize("Eye cream") | recatagorize("Lip balm") | recatagorize("Face") | recatagorize("Facial") | recatagorize("Moisturizer") | recatagorize("aging") | recatagorize("Sheet") | recatagorize("Acne") ~ "Skincare", 
      recatagorize("Eye") | recatagorize("Eye shadow") | recatagorize("Mascara") | recatagorize("Lid") | recatagorize("Lipstick") | recatagorize("Lip gloss") | recatagorize("Cheek") | recatagorize("Concealer") | recatagorize("Foundation") | recatagorize("Blush") | recatagorize("Highlighter") | recatagorize("Bronzer") | recatagorize("BB") | recatagorize("Toners") | recatagorize("Makeup") | recatagorize("Countour")  ~ "Makeup",
      recatagorize("Sponges") | recatagorize("Mirrors") | recatagorize("Combs") | recatagorize("Tweezers") | recatagorize("Facial rollers") | recatagorize("Curlers") | recatagorize("Straighteners") | recatagorize("Brushes") | recatagorize("Irons") ~ "Tools",
      recatagorize("Diffusers") | recatagorize("Wellness") | recatagorize("Candles") ~ "Wellness",
      TRUE ~ "Others"
      )
    )

unique(data$new_categories)

# Check for the percentage of data that falls under the classified categories other than 'Others'
# Calculate the count of observations in each category
category_counts <- table(data$new_categories)
category_counts

# Exclude the "Others" category
non_others_counts <- category_counts[category_counts != category_counts["Others"]]

# Calculate the total count of observations in non-"Others" categories
total_non_others <- sum(non_others_counts)
total_non_others
# Calculate the percentage of data in non-"Others" categories
percentage_non_others <- (total_non_others / sum(category_counts)) * 100
percentage_non_others

# The re-categorization approach correctly categorized 7521 observations i.e. 82% of the total data observations

# Distribution of products in new categories. 

dist_plot <- data %>%
  ggplot(aes(new_categories)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black", size = 3) +
  scale_x_discrete(limits = c("Body", "Makeup", "Perfume", "Hair", "Skincare", "Tools", "Wellness")) +
  xlab("New categories") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dist_plot 

# We can see that the Skincare has highest number of observations followed by Makeup.

# I decided to look into the ingredients used in different categories of the products.
# Tidying up the data

ingredients = as_tibble(cbind(data$new_categories, data$ingredients)) %>%
  unnest_tokens(word, V2, token = stringr::str_split, pattern = "-") %>%
  unnest_tokens(word, word, token = stringr::str_split, pattern = ":") %>%
  unnest_tokens(word, word, token = stringr::str_split, pattern = "[.]") %>%
  rename(Category = V1) %>%
  mutate(across(where(is.character), str_trim))
head(ingredients,5)

ingredient_counts <- ingredients %>%
  group_by(Category) %>%
  count(word, sort = TRUE) %>%
  mutate(
    word = ifelse(nchar(word) < 3, " ", word),
    word = ifelse(nchar(word) > 50, " ", word),
    word = str_remove_all(word, "[(]"),
    word = str_remove_all(word, "[)]$")
    ) %>%
  filter(
    word != " ",
    word != "",
    word != ")",
    word != "unknown",
    word != "1% or more of total formula",
    word != "may contain (+/",
    word != "[+/",
    word != "wiped off or removed",
    word != "clean at sephora products are formulated without",
    word != "may contain",
    word != "may contain +/",
    word != "+/"
    )

head(ingredient_counts,5)
min(ingredient_counts$n)
max(ingredient_counts$n)
mean(ingredient_counts$n)

ingredient_counts %>%
  group_by(Category, word, n) %>%
  filter(Category != "Others") %>%
  filter(n > 150) %>%
  ggplot(aes(area = n, fill = Category, label = word, subgroup = Category)) +
  geom_treemap(star = "topleft") +
  geom_treemap_subgroup_border(col = "black", size = 1, start = "topleft") +
  geom_treemap_text(place = "topleft", col = "black", start = "topleft", min.size = 0) +
  scale_fill_brewer(palette = "Set1", type = "qual", name = "Category") +  
  theme(legend.position = "bottom")

# From the plot we can see that the most used ingredients in Skincare is Glycerin and for skincare its Mica

# Now let's explore the ingredients used in highly rated products of category makeup, skincare and hair

ingredients_1 = as.tibble(cbind(data$new_categories, data$rating, data$ingredients)) %>%
  unnest_tokens(word, V3, token = stringr::str_split, pattern = "-") %>%
  unnest_tokens(word, word, token = stringr::str_split, pattern = ":") %>%
  unnest_tokens(word, word, token = stringr::str_split, pattern = "[.]") %>%
  rename(Category = V1, Rating = V2) %>%
  mutate(across(where(is.character), str_trim))
head(ingredients_1,5)

ingredients_counts1 = 
  ingredients_1 %>%
  group_by(Category, Rating) %>%
  count(word, sort = TRUE) %>%
  mutate(
    word = ifelse(nchar(word) < 3, " ", word),
    word = ifelse(nchar(word) > 50, " ", word),
    word = str_remove_all(word, "[(]"),
    word = str_remove_all(word, "[)]$")
  ) %>%
  filter(
    word != " ",
    word != "",
    word != ")",
    word != "unknown",
    word != "1% or more of total formula",
    word != "may contain (+/",
    word != "[+/",
    word != "wiped off or removed",
    word != "clean at sephora products are formulated without",
    word != "may contain",
    word != "may contain +/",
    word != "+/"
  )
head(ingredients_counts1,5)

# Skincare plot

Skincare_plot = 
  ingredients_counts1 %>%
  group_by(Category, Rating, word, n) %>%
  filter(Category == "Skincare") %>%
  filter(n > 50) %>%
  ggplot(aes(area=n, label=n, subgroup = Rating, fill = Rating, subgroup2 = word)) +
  geom_treemap(start="topleft")

Skincare_plot +
  geom_treemap_subgroup_border(col="black", size=2) +
  geom_treemap_subgroup2_text(place="topleft", col="black", reflow = T, size=10, start="topleft") +
  geom_treemap_text(place="bottomright", col="black", size=6, start="topleft") +
  scale_fill_brewer(type = "seq", palette = "Purples")

# Makeup plot

Makeup_plot = 
  ingredients_counts1 %>%
  group_by(Category, Rating, word, n) %>%
  filter(Category == "Makeup") %>%
  filter(n > 50) %>%
  ggplot(aes(area=n, label=n, subgroup = Rating, fill = Rating, subgroup2 = word)) +
  geom_treemap(start="topleft")

Makeup_plot +
  geom_treemap_subgroup_border(col="black", size=2) +
  geom_treemap_subgroup2_text(place="topleft", col="black", reflow = T, size=10, start="topleft") +
  geom_treemap_text(place="bottomright", col="black", size=6, start="topleft") +
  scale_fill_brewer(type = "seq", palette = "Blues")


# Hair plot

Hair_plot = 
  ingredients_counts1 %>%
  group_by(Category, Rating, word, n) %>%
  filter(Category == "Hair") %>%
  filter(n > 50) %>%
  ggplot(aes(area=n, label=n, subgroup = Rating, fill = Rating, subgroup2 = word)) +
  geom_treemap(start="topleft")

Hair_plot +
  geom_treemap_subgroup_border(col="black", size=2) +
  geom_treemap_subgroup2_text(place="topleft", col="black", reflow = T, size=10, start="topleft") +
  geom_treemap_text(place="bottomright", col="black", size=6, start="topleft") +
  scale_fill_brewer(type = "seq", palette = "Greens")


## Exploring the relationship between the variables ##

numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)
heatmap.2(correlation_matrix,
          Rowv = NA, 
          Colv = NA, 
          dendrogram = "none", 
          col = colorRampPalette(c("blue", "white", "red"))(100), 
          scale = "none", 
          margins = c(5, 5),
          key = TRUE, 
          keysize = 1.5, 
          trace = "none", 
          cexRow = 0.9, 
          cexCol = 0.9, 
          cellnote = round(correlation_matrix, 2),  
          notecol = "black", 
          density.info = "none",
          main = "Correlation Heatmap")

# It looks like value price and price are highly correlated. Also, number of reviews and rating are correlated



