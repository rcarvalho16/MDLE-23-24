#############################################
#                                           #
# ISEL, MP, 2024                            #
#                                           #
# Didactic material to support the          #
# Big Data Mining course                    #
# LastUpdate: April, 24 2024.               #
# Theme: Exploratory Data Analysis          #
# VERSION: 2                                #
#                                           #
#############################################

# Context

# one-line that removes all objects except for functions:
rm(list=ls(all=TRUE))
set.seed(20240424)

# User-Defined Functions
resumetable <- function(df) {
  cat("Dataset Shape: ", paste(dim(df), collapse = "x"), "\n")
  summary <- data.frame(dtypes = sapply(df, class))
  summary$Name <- rownames(summary)
  row.names(summary) <- NULL
  summary <- summary[, c("Name", "dtypes")]
  summary$Missing <- sapply(df, function(x) sum(is.na(x)))
  summary$Uniques <- sapply(df, function(x) length(unique(x)))
  summary$First.Value <- unlist(df[1, ])
  summary$Second.Value <- unlist(df[2, ])
  summary$Third.Value <- unlist(df[3, ])
  summary$Entropy <- sapply(df, function(x) 
    round(-sum(table(x)/length(x) * log2(table(x)/length(x))), 2))
  
  return(summary)
}

CalcOutliers <- function(df_num) {
  # calculating mean and std of the array
  data_mean <- mean(df_num)
  data_std <- sd(df_num)
  
  # seting the cut line to both higher and lower values
  # You can change this value
  cut <- data_std * 3
  
  #Calculating the higher and lower cut values
  lower <- data_mean - cut
  upper <- data_mean + cut

  # creating an array of lower, higher and total outlier values 
  outliers_lower <- df_num[df_num < lower]
  outliers_higher <- df_num[df_num > upper]
  outliers_total <- df_num[df_num < lower | df_num > upper]
  
  # array without outlier values
  outliers_removed <- df_num[df_num > lower & df_num < upper]
  
  # printing lower cut values:
  cat("Lower cut values: ", round(lower), "\n")
  # printing upper cut values: 
  cat("Upper cut values: ", round(upper), "\n")
  # printing total number of values in lower cut of outliers
  cat("Identified lowest outliers: ", length(outliers_lower), "\n") 
  # printing total number of values in higher cut of outliers
  cat("Identified upper outliers: ", length(outliers_higher), "\n") 
  # printing total number of values outliers of both sides
  cat("Identified outliers: ", length(outliers_total), "\n")
  # printing total number of non outlier values
  cat("Non-outlier observations: ", length(outliers_removed), "\n")
  cat("Total percentual of Outliers: ", 
      round((length(outliers_total) / length(outliers_removed))*100, 4), 
      "%\n") # Percentual of outliers in points
  
  return(NULL)
}

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
# 2. Importing our data set in variable
df = read.csv('../data/data-130k-v2.csv', header = T, sep = ",", 
              quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
              na.strings=c("","NA"))
df <- df[,-1]

## 1. A simple histogram can be a great first step in understanding a data set.

# ------------------  TODO  ------------------
resumetable(df)

# QUESTION: Comment the following results 

# Expected results:
# Dataset Shape:  129971x13 
#                    Name    dtypes Missing Uniques
#1                country character      63      44
#2            description character       0  119955
#3            designation character   37465   37980
#4                 points   integer       0      21
#5                  price   numeric    8996     391
#6               province character      63     426
#7               region_1 character   21247    1230
#8               region_2 character   79460      18
#9            taster_name character   26244      20
#10 taster_twitter_handle character   31213      16
#11                 title character       0  118840
#12               variety character       1     708
#13                winery character       0   16757


# ------------------  TODO  ------------------
library(Hmisc)
cat("Statistics of numerical data: ")
print(describe(df))

# QUESTION: Comment the following results 

# Expected results:
#Statistics of numerical data: 
#              points          price
#count  129971.000000  120975.000000
#mean       88.447138      35.363389
#std         3.039730      41.022218
#min        80.000000       4.000000
#25%        86.000000      17.000000
#50%        88.000000      25.000000
#75%        91.000000      42.000000
#max       100.000000    3300.000000


# 2. Points Distribution

install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)

# ------------------  TODO  ------------------
# Create the countplot
countplot <- ggplot(df) +
  aes(x=points, fill=factor(points)) + 
  geom_bar(fill="darkgreen") +
  labs(title="Points Count distribution", x="Points", y="Count") +
  theme(plot.title = element_text(size=20), 
        axis.title = element_text(size=15))
ggsave("./plots/PointsDistribution.pdf")

# QUESTION: Could you explain the results? What do the plot distributions look 
# like?

# 3. Detecting Outlier Points

# User-Defined Function
# set the points into categories
cat_points <- function(points){
  if (points %in% 80:82) {
    return(0)
  } else if (points %in% 83:86) {
    return(1)
  } else if (points %in% 87:89) {
    return(2)
  } else if (points %in% 90:93) {
    return(3)
  } else if (points %in% 94:97) {
    return(4)
  } else {
    return(5)
  }
}

# Create rating category column in df
df$rating_cat <- sapply(df$points, cat_points)

# Ploting Rating categories

ggplot(df, aes(x=rating_cat, fill=rating_cat)) + 
  geom_bar(color="black") +
  scale_fill_manual(values=c("darkgreen")) +
  labs(title="Point Categories Counting Distribution", x="Categories", 
       y="Total Count") +
  theme(plot.title = element_text(size=20, face="bold"), 
        axis.text = element_text(size=15), 
        axis.title = element_text(size=15)) +
  geom_text(stat='count', aes(label=scales::percent(after_stat(count)/nrow(df))), 
            position=position_stack(vjust=0.5), size=5) +
  ylim(0, max(table(df$rating_cat)) * 1.15)
ggsave("./plots/RatingCategories.pdf")

# ------------------  TODO  -----------------

CalcOutliers(df$points)

# QUESTION: Comment the following results 

# Expected results:
# Identified lowest outliers:  0 
# Identified upper outliers:  129 
# Identified outliers:  129 
# Non-outlier observations:  129842 
# Total percentual of outliers:  0.0994 %


# ------------------ TODO: Price Distribution ------------------
# Repeat all steps as done with points: 
# Plot a price log distribution
# Outliers in prices
# Filtered price lower than 300.

g <- ggplot(data=na.omit(df), aes(x=log(price+1))) + 
  geom_histogram(color='white', fill='darkgreen',bins=30) +
  labs(title="Price Log distribuition", x="Price(Log)", y="Frequency LOG") +
  theme(plot.title = element_text(size=20), axis.text=element_text(size=15), 
        axis.title=element_text(size=15))
ggsave("./plots/PriceLogDistribution.pdf")

install.packages("tidyr")
library(tidyr)

ggplot(data=df %>% drop_na(price), aes(x=seq_along(price), y=price)) + 
  geom_point(color='darkgreen') +
  labs(title="Distribuition of prices", x="Index", y="Prices(US)") +
  theme(plot.title = element_text(size=20), axis.text=element_text(size=15), 
        axis.title=element_text(size=15))
ggsave("./plots/DistribuitionPrices.pdf")

# ------------------ TODO: Outlier Points ------------------

# Complete the function to find the outliers. Fill in the missing field
CalcOutliers(df$price %>% na.omit())

# QUESTION: Comment the following results 

# Expected results:
# Identified lowest outliers:  0 
# Identified upper outliers:  1177 
# Identified outliers:  1177 
# Non-outlier observations:  119798 
# Total percentual of Outliers:  0.9825 %

## Filtered distribution of Prices

g <- ggplot(data = subset(df, price < 300), aes(x = price))
g + geom_density(fill = "darkgreen", alpha = 0.5) +
  ggtitle("Price Distribution Filtered 300") +
  xlab("Prices(US)") +
  ylab("Frequency Distribution") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
ggsave("./plots/DistributionPrices300.pdf")

# QUESTION: What price range do the majority of the analyzed wines fall under?


# 4. Crossing prices and Points
# Let's get the price_log to better work with this feature
df['price_log'] = log(df['price'])

ggplot(data = df %>% drop_na(price_log), aes(x = points, y = price_log)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", formula = 'y ~ x') +
  xlab("Points") +
  ylab("Price (log)") +
  ggtitle("Points x Price Distribuition") +
  theme(plot.title = element_text(size = 20))
ggsave("./plots/DistributionPointsPrices.pdf")

# QUESTION: What characteristics distinguish the highest-priced wine from the 
# one with the highest score? Do you consider it relevant to use the recommendation 
# system for the entire dataset, or just a portion of it? Which one?


# ------------------ TODO: Country feature ------------------
# Plot a country of wine origin count
# Boxplot of top 20 countries by price and rating

# Load the dplyr package
library(dplyr)

# get the top 20 countries by wine count
country20 <- head(names(sort(table(df$country), decreasing=TRUE)), 20)
# Plot the count of wine origins
country1 <- head(df %>% count(country) %>% arrange(desc(n)), 20)

# Plot the count of wine origins

# Plot 1: Boxplot of Count by Country of Wine Origin
g <- ggplot(data = subset(df, country %in% country20), aes(x = country, 
                                                           fill = country)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Country Of Wine Origin Count") +
  xlab("Country's") +
  ylab("Count")
ggsave("../plots/CountryWineOriginCount.pdf")

# Plot 2: Boxplot of Price by Country
g <-ggplot(data = subset(df %>% drop_na(country), country %in% country20), 
           aes(x = country, y = price_log, fill = country)) +
  geom_boxplot() +
  ggtitle("Price by Country Of Wine Origin") +
  xlab("Country's") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PriceCountryOrigin.pdf")

# Plot 3: Boxplot of Points by Country
g <-ggplot(data = subset(df %>% drop_na(country), country %in% country20), 
           aes(x = country, y = points, fill = country)) +
  geom_boxplot() +
  ggtitle("Points by Country Of Wine Origin") +
  xlab("Country's") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PointsCountryOrigin.pdf")

# QUESTION: What are your conclusions?Are the results as you expected? Have you 
# checked online to validate the results?

#  ------------------ TODO ------------------ 
# Repeat the previous steps now for the following studies:
# (1) Taster Feature, (2) Province Feature, (3) Variety Feature and (4) Winery
# Distributions  


# ------------------ (1) Taster Feature ------------------
# Plot a taster name feature count
# Boxplot of top 20 taster name by price and rating

# Province Exploration
taster20 <- names(sort(table(df$taster_name), decreasing = TRUE))[1:20]

# Plot 1: Countplot of Taster Name
g <- ggplot(data = subset(df, taster_name %in% taster20), aes(x = taster_name, 
                                                             fill = taster_name)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Taster Name Count") +
  xlab("Tasters") +
  ylab("Count")
ggsave("../plots/CountTasterName20.pdf")

# Plot 2: Boxplot of Prices by Taster Name
g <-ggplot(data = subset(df %>% drop_na(taster_name), taster_name %in% taster20), 
           aes(x = taster_name, y = price_log, fill = taster_name)) +
  geom_boxplot() +
  ggtitle("Price by Taster Name") +
  xlab("Tasters") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PriceTasterNameDistribution.pdf")

# Plot 3: Boxplot of Points by Taster Name
g <-ggplot(data = subset(df %>% drop_na(taster_name), taster_name %in% taster20), 
           aes(x = taster_name, y = points, fill = taster_name)) +
  geom_boxplot() +
  ggtitle("Points by Taster Name") +
  xlab("Tasters") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PointsTasterName.pdf")


# ------------------ (2) Province Exploration ------------------
# Plot a province of wine origin count
# Boxplot of top 20 province by price and rating

# Province Exploration
province20 <- names(sort(table(df$province), decreasing = TRUE))[1:20]

# Plot 1: Countplot of Provinces
g <- ggplot(data = subset(df, province %in% province20), aes(x = province, 
                                                             fill = province)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Province Of Wine Origin Count") +
  xlab("Provinces") +
  ylab("Count")
ggsave("../plots/CountProvinces.pdf")

# Plot 2: Boxplot of Prices by Province
g <-ggplot(data = subset(df %>% drop_na(province), province %in% province20), 
           aes(x = province, y = price_log, fill = province)) +
  geom_boxplot() +
  ggtitle("Price by Province Of Wine Origin") +
  xlab("Provinces") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PriceProvincesOrigin.pdf")

# Plot 3: Boxplot of Points by Province
g <-ggplot(data = subset(df %>% drop_na(province), province %in% province20), 
           aes(x = province, y = points, fill = province)) +
  geom_boxplot() +
  ggtitle("Points by Province Of Wine Origin") +
  xlab("Provinces") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PointsProvincesOrigin.pdf")

# ------------------ (3) Variety Feature ------------------
# Plot a variety of wine origin count
# Boxplot of top 20 variety by price and rating

# Variety Exploration
variety20 <- names(sort(table(df$variety), decreasing = TRUE))[1:20]

# Plot 1: Countplot of Variety
g <- ggplot(data = subset(df, variety %in% variety20), aes(x = variety, 
                                                             fill = variety)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Variety Of Wine Count") +
  xlab("Variety") +
  ylab("Count")
ggsave("../plots/CountVariety20.pdf")

# Plot 2: Boxplot of Prices by Variety 20
g <-ggplot(data = subset(df %>% drop_na(variety), variety %in% variety20), 
           aes(x = variety, y = price_log, fill = variety)) +
  geom_boxplot() +
  ggtitle("Price by Variety of Wine") +
  xlab("Variety") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PriceVarietyDistribution.pdf")

# Plot 3: Boxplot of Points by Taster Name
g <-ggplot(data = subset(df %>% drop_na(variety), variety %in% variety20), 
           aes(x = variety, y = points, fill = variety)) +
  geom_boxplot() +
  ggtitle("Points Variety of Wine") +
  xlab("Variety") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PointsVariety.pdf")


# ------------------ (4) Winery Distributions ------------------
# Plot a province of wine origin count
# Boxplot of top 20 province by price and rating

# Province Exploration
winery20 <- names(sort(table(df$winery), decreasing = TRUE))[1:20]

# Plot 1: Countplot of Winery
g <- ggplot(data = subset(df, winery %in% winery20), aes(x = winery, 
                                                           fill = winery)) +
  geom_bar() +
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Winery Count") +
  xlab("Winery") +
  ylab("Count")
ggsave("../plots/Top20Winery.pdf")

# Plot 2: Boxplot of Prices by Winery
g <-ggplot(data = subset(df %>% drop_na(winery), winery %in% winery20), 
           aes(x = winery, y = price_log, fill = winery)) +
  geom_boxplot() +
  ggtitle("Price by Winery") +
  xlab("Winery") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PriceWinery.pdf")

# Plot 3: Boxplot of Points by Winery's
g <-ggplot(data = subset(df %>% drop_na(winery), winery %in% winery20), 
           aes(x = winery, y = points, fill = winery)) +
  geom_boxplot() +
  ggtitle("Points by Winery") +
  xlab("Winery") +
  ylab("Points") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 
ggsave("../plots/PointsWinery.pdf")


## Better understanding the length of reviews
df['desc_length'] = nchar(df$description)


ggsave("../plots/PointsDescriptionLength.pdf")

# 5. Wordcloud of descriptions

install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("stopwords")
library(stopwords)
stopwords <- setdiff(stopwords("en"), c("fruit", "Drink", "black", 
                                        "wine", "drink"))
install.packages("NLP")
library(NLP)
install.packages("tm")
install.packages("tmap")
library(tm)
library(tmap)


pdf(file = "../plots/WordcloudDescriptions.pdf")   


# Create a text corpus
corpus <- Corpus(VectorSource(df$description))

# Convert description column to character
corpus <- as.character(corpus)

# Remove non-ASCII characters and punctuation
corpus <- gsub("[^[:alnum:] ]", "", corpus)

wordcloud <- wordcloud(
  words = na.omit(corpus),
  #scale = c(4, 0.5),
  max.words = 300,
  min.freq = 1,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"),
  random.color = TRUE,
  use.r.layout = FALSE,
  fixed.asp = TRUE,
  stopwords = stopwords
)


plot(wordcloud, main = "WORD CLOUD - DESCRIPTION", col.axis = "black", 
     col.lab = "black")
dev.off()

#  ------------------ TODO: Wordcloud of wine titles


# You should answer, for example:
# Do provinces have the same number of wines?
# What is the distribution of prices and points by province?
# How are the countries distributed?
# Do the tasters have the same number of votes?
# What's the distribution of points and prices by taster's name?
# View the wordclouds.


# 6. TFIDF Vectorizer in the Wine Review
install.packages("SnowballC")
install.packages("tidytext")
library(SnowballC)
library(gridExtra)
library(tm)
library(dplyr)
library(tidytext)

grid = expand.grid(seq(1, 5), seq(1, 2))
names(grid) <- c("row", "col")

## THIS CODE BELOW IS PURPOSELY WRONG. CORRECT IT

# O que faz função:
# As principais operações são `tm` para processamento de texto e `dplyr`
# para manipulação de dados.
# A Função `get_top_terms` realiza as seguintes etapas:
# Pré-processamento do Texto
# (Cria um conjunto de documentos "corpus" de texto a partir dos dados 
# fornecidos.
# Converte o texto para minúsculas e remove números, pontuação e palavras
# comuns 'stop words'. Reduz à sua raiz as palavras - por exemplo,
# "correr", "corrido" e "corria" tornam-se "corr").
#
# converte o 'corpus' numa matriz de termos e documentos (TDM), 
# onde as linhas representam termos únicos
# e as colunas representam documentos.
# Remove termos raros (com uma frequência de termos inferior a 0.95).
#
# Converte a matriz de termos e documentos num data frame. 
# Renomeia as colunas do data frame,
# adicionando um prefixo para evitar problemas com nomes de colunas
# inválidos. Adiciona uma coluna 'terms' para armazenar os termos e
# uma coluna 'category' para armazenar a categoria fornecida.
#
# Agregação e Seleção dos Termos Principais utilizando a função `gather()`
# para transformar o data frame de formato amplo para longo, 
# para facilitar a agregação. Agrupa os termos por categoria e documento,
# e então calcula a contagem total de cada termo em cada categoria.
# Realiza uma segunda agregação por categoria, selecionando os 15 termos
# mais comuns em cada categoria com base na contagem total.
# Ordena os termos por categoria e pela contagem total em ordem 
# decrescente.
# Retorno do Data Frame com os Termos Principais agrupados por categoria,
# pronto para ser usado em análises posteriores.
# Em resumo, a função é útil para extrair e analisar os termos mais 
# comuns em diferentes categorias de documentos de texto, como revisões
# de produtos, comentários de clientes, etc.

# define function for getting top ranking terms
get_top_terms <- function(data, category) {
  corpus <- VCorpus(VectorSource(data))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument, language = "english")
  tdm <- TermDocumentMatrix(corpus, control = list(minWordLength = 2))
  tdm <- removeSparseTerms(tdm, 0.95)
  df_tdm <- as.data.frame(as.matrix(tdm))
  colnames(df_tdm) <- make.names(colnames(df_tdm))
  df_tdm$terms <- row.names(df_tdm)
  df_tdm$category <- category
  df_tdm %>%
    gather(key = "doc_id", value = "term_count", -terms, -category) %>%
    group_by(category, doc_id) %>%
    summarize(total_count = sum(term_count)) %>%
    group_by(category) %>%
    top_n(15, total_count) %>%
    arrange(category, desc(total_count))
}
# Na função arrange() faltava o argumento category para garantir 
# a ordenação por categoria.

#install.packages("tidyr")
library("tidyr")

df_top_terms <- lapply(unique(df$country)[1:10], function(cat) {
  data <- df %>%
    filter(country == cat) %>%
    pull(description)
  top_terms <- get_top_terms(data, cat)
  top_terms$rank <- seq(1, nrow(top_terms))
  top_terms
}) %>%
  bind_rows()

p_list <- lapply(unique(df_top_terms$category), function(cat) {
  df_plot <- df_top_terms %>%
    filter(category == cat) %>%
    mutate(term = reorder(term, rank))
  ggplot(df_plot, aes(x = term, y = total_count, fill = category)) +
    geom_col() +
    ggtitle(paste0("Wines from ", cat, " N-grams")) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = c("#F8766D", "#00BFC4", "#E76BF3", "#7CAE00", "#D55E00", "#0072B2", "#2F4B7C", "#CC79A7", "#56B4E9", "#009E73"))
})


pdf(file = "barplot.pdf")   # The directory you want to save the file in

# Step 2: Create the plot with R code
# arrange the plots in a grid
grid.arrange(grobs = p_list, layout_matrix = grid, ncol = 2)
# Step 3: Run dev.off() to create the file!
dev.off()



## Recommender System using a Collaborative Filtering method

# A small recommender system is made using Nearest Neighbors algorithm.
# Similarity is the cosine of the angle between the 2 vectors of the item vectors of A and B
# Closer the vectors, smaller will be the angle and larger the cosine


install.packages("RANN")

install.packages("Matrix")
library(Matrix)
library(RANN)

col <- c('province', 'variety', 'points')
wine1 <- df[, col]
wine1 <- wine1[complete.cases(wine1), ]
wine1 <- wine1[!duplicated(wine1[, c("province", "variety")]), ]
wine1 <- wine1[wine1$points > 85, ]
install.packages("reshape2")
library(reshape2)

wine_pivot <- reshape2::dcast(wine1, variety ~ province, value.var = "points", 
                              fill = 0)
wine_pivot_matrix <- Matrix::Matrix(as.matrix(wine_pivot[, -1]), sparse = TRUE)
sparsity <- sum(wine_pivot_matrix == 0)/(dim(wine_pivot_matrix)[1]*
                                           dim(wine_pivot_matrix)[2])
knn_model <- nn2(wine_pivot_matrix, k = 10)

## THIS CODE BELOW IS PURPOSELY WRONG. CORRECT IT
for (n in 1:5) {
  query_index <- sample(nrow(wine_pivot_matrix), 1)
  indices <- knn_model$nn.idx(wine_pivot[query_index,], k = 6)
  distances <- knn_model$nn.dists(wine_pivot[query_index,], k = 6)
  print(paste0("Recommendation for ## ", rownames(wine_pivot)[query_index], " ##:"))
  for (i in 2:6) {
    print(paste0(i-1, ": ", rownames(wine_pivot)[indices[i]], " with distance: ", distances[i]))
  }
  cat("\n")
}

# Recmmendation for ## Çalkarası ##:
# 1: Okuzgozu with distance: 0.48756655954162265
# 2: Kalecik Karasi with distance: 0.49582150971527217
# 3: Boğazkere with distance: 0.5041994308311355
# 4: Syrah-Merlot with distance: 0.584090367243181
# 5: Rosé with distance: 0.8693543565328108
# 
# 
# Recmmendation for ## Marsanne ##:
# 1: Cabernet Sauvignon-Sangiovese with distance: 0.252916228086544
# 2: Grenache-Mourvèdre with distance: 0.2854554967501951
# 3: Semillon-Chardonnay with distance: 0.3046798364137062
# 4: Counoise with distance: 0.3046828432855857
# 5: Souzao with distance: 0.30481239332281795
# 
# 
# Recmmendation for ## Weissburgunder ##:
# 1: Pinot Blanc with distance: 0.40564246556007066
# 2: Zweigelt with distance: 0.44381301062227596
# 3: Austrian Red Blend with distance: 0.48735861058478447
# 4: Grauburgunder with distance: 0.5015189358836503
# 5: Scheurebe with distance: 0.524795310624997
# 
# 
# Recmmendation for ## Cerceal ##:
# 1: Baga-Touriga Nacional with distance: 0.1896188209554306
# 2: Baga with distance: 0.22591384526102187
# 3: Maria Gomes with distance: 0.414096983579762
# 4: Touriga Nacional-Cabernet Sauvignon with distance: 0.4175134221366079
# 5: Bical with distance: 0.4269739729516353
# 
# 
# Recmmendation for ## Bombino Nero ##:
# 1: Caprettone with distance: 0.0
# 2: Bombino Bianco with distance: 0.0
# 3: Piedirosso with distance: 0.0
# 4: Pallagrello Bianco with distance: 0.0
# 5: Roviello with distance: 0.0


## TODO: Maybe it would be interesting to build an RS to find cheapest wines 
# with the same quality.