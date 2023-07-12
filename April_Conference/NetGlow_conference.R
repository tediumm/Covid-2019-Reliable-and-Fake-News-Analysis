library("tidyverse")
library("ggplot2")
library("tidytext")
library("widyr")
library("ggraph")
library("ggplot2")
library("foreign")
library('wordcloud')
library('igraph')
library("intergraph")
library("extrafont")
library("gridExtra")
library("reshape2")
library("RColorBrewer")

# ---- Databases ----
library("readxl")

covid <- read_excel("fake_new_dataset.xlsx")
covid_true <- subset(covid, subset = covid$label == "1")
covid_fake <- subset(covid, subset = covid$label == "0")

afinn <- read_csv("Afinn.csv",
  col_types = cols(word = col_character(), value = col_double())
)
bing <- read_csv("Bing.csv",
  col_types = cols(word = col_character(), sentiment = col_character())
)
nrc <- read_csv("NRC.csv",
  col_types = cols(word = col_character(), sentiment = col_character())
)

# ---- Most frequent words ----

covid_fake_new <- covid_fake %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    word != "de",
    str_detect(word, "[a-z]")
  )

covid_fake_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
    geom_col(width = 0.75, col = "white", fill = "#99d8c9")+
    coord_flip()+
    labs(title = "Common words in fake news titles")+
    theme_classic()

covid_fake_filtered <- covid_fake_new %>%
  add_count(word) %>%
  filter(n >= 10)

covid_fake_all <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE)


top_word_covid_fake <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = T) %>% 
 head(100)
  
covid_true_new <- covid_true %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    word != "de",
    str_detect(word, "[a-z]")
  )

covid_true_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n)) +
  geom_col(
    width = 0.75,
    col = "white",
    fill = "#99d8c9"
  ) +
  coord_flip() +
  labs(title = "Common words in true news titles") +
  theme_classic() 

covid_true_filtered <- covid_true_new %>%
  add_count(word) %>%
  filter(n >= 10)

top_word_covid_true <- covid_true_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
 head(100)

# ---- Fake graphs ----



top_word_covid_fake %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  theme_void(base_size = 18) +
  geom_node_text(aes(label = name), repel = TRUE)

top_word_covid_fake %>%
  graph_from_data_frame() %>%
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(colour = correlation, alpha = correlation)) +
  coord_fixed() +
  geom_node_text(aes(label = name), repel = TRUE) +
  geom_node_point() +
  theme_void(base_size = 18) +
  scale_edge_color_continuous(low = "red", high = "green") +
  theme(legend.position = "none")

# ---- Reliable graphs ----

covid_true_filtered <- covid_true_new %>%
  add_count(word) %>%
  filter(n >= 10)

top_word_covid_true <- covid_true_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
  head(100)

top_word_covid_true %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  theme_void(base_size = 18) +
  geom_node_text(aes(label = name), repel = TRUE)

top_word_covid_true %>%
  graph_from_data_frame() %>%
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(colour = correlation, alpha = correlation)) +
  coord_fixed() +
  geom_node_text(aes(label = name), repel = TRUE) +
  geom_node_point() +
  theme_void(base_size = 18) +
  scale_edge_color_continuous(low = "red", high = "green") +
  theme(legend.position = "none")

# ---- Sentimental analysis for fakes----


colsR_B <- brewer.pal(4, name = "RdBu")
#display.brewer.pal(4, name = "RdBu")

covid_fake_new %>%
  inner_join(bing, by = "word") %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = colsR_B[c(1, 4)], max.words = 400, title.size = 2,
    scale = c(3, .5)
  )


# ---- Sentimental analysis for reliable ----

covid_true_new %>%
  inner_join(bing, by = "word") %>%
  count(word, sentiment, sort = T) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = colsR_B[c(1, 4)], max.words = 400, title.size = 2,
    scale = c(3, .5)
  )


# ---- Radar Chart ----
library("radarchart")

covid_true_new1 <- covid_true_new
covid_fake_new1 <- covid_fake_new

FAKE <- c()
for (i in 1:length(covid_fake_new1$post_id)) {
  FAKE <- append(FAKE, "FAKE")
}
FAKE

RIGHT <- c()
for (i in 1:length(covid_true_new1$post_id)) {
  RIGHT <- append(RIGHT, "TRUE")
}

RIGHT

covid_fake_new1$status <- FAKE
covid_true_new1$status <- RIGHT

covid_new <- rbind(covid_fake_new1, covid_true_new1)

char_sentiment <- covid_new %>%
  inner_join(nrc, "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(status, sentiment) %>%
  count(status, sentiment) %>%
  select(status, sentiment, char_sentiment_count = n)

total_char <- covid_new %>%
  inner_join(nrc, "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(status) %>%
  select(status, total = n)

plt <- char_sentiment %>%
  inner_join(total_char, by = "status") %>%
  mutate(percent = char_sentiment_count / total * 100) %>%
  select(-char_sentiment_count, -total) %>%
  spread(status, percent) %>%
  chartJSRadar(
    showToolTipLabel = T, maxScale = 25, responsive = T,
    addDots = T,
    colMatrix = grDevices::col2rgb(colsR_B[c(1, 4)]),
    lineAlpha = 0.7, polyAlpha = 0.2
  )
plt
colsR_B[c(1, 4)]

# ----- Создаем сети по Fake новостям ----

#detach(package:igraph)
#detach(package:ergm)
#detach(package:network)
#detach(package:sna)

#library("network")
library("sna")
library("ergm")
library("igraph")

edges_F <- as.data.frame(top_word_covid_fake)
names(edges_F) <- c('ego_id', 'alter_id', 'correlation')

nodes_F <-as.data.frame(unique(edges_F$ego_id))
names(nodes_F) <- c('word')

id <- seq(1,length(nodes_F$word),1)
nodes_F<-cbind(id, nodes_F)
names(nodes_F) <- c('id','word')

edges2_F<- merge(nodes_F[,1:2], edges_F, by.x = "word", by.y="alter_id") 
head(edges2_F) #check the new edge dataset

names(edges2_F)[1]<-"alter_id"
names(edges2_F)[2]<-"alter_R_id"
edges3_F<- merge(nodes_F[,1:2], edges2_F, by.x = "word", by.y="ego_id") #data with IDs we need
names(edges3_F)[1]<-"ego_id"
names(edges3_F)[2]<-"ego_R_id"
head(edges3_F)

# Фильтруем по наличию токенов в лексиконе bing
nodes_F <- nodes_F %>% 
  left_join(bing, by=c( 'word'))
nodes_F[is.na(nodes_F)] <- 'neutral'

net_F <- graph_from_data_frame(edges_F, directed=F)
net_F<-asNetwork(net_F)
##net_F<-network(edges3_F[,c("ego_R_id", "alter_R_id")], directed = F) #Dyads will be the unit of analysis # Assign edge-level attributes - dyad attributes
#set.edge.attribute(net, "ego_R_id", edges_F[,2])
#set.edge.attribute(net, "alter_R_id", edges_F[,3])

net_F %e% "correlation" <- edges_F[,3]
net_F %v% "sentiment" <- nodes_F[,3]

# ----- Создаем сети по True новостям ----


edges_T <- as.data.frame(top_word_covid_true)
names(edges_T) <- c('ego_id', 'alter_id', 'correlation')


nodes_T <-as.data.frame(unique(edges_T$ego_id))
names(nodes_T) <- c('word')

id <- seq(1,length(nodes_T$word),1)
nodes_T<-cbind(id, nodes_T)
names(nodes_T) <- c('id','word')

edges2_T<- merge(nodes_T[,1:2], edges_T, by.x = "word", by.y="alter_id") 
head(edges2_T) #check the new edge dataset

names(edges2_T)[1]<-"alter_id"
names(edges2_T)[2]<-"alter_R_id"
edges3_T<- merge(nodes_T[,1:2], edges2_T, by.x = "word", by.y="ego_id") #data with IDs we need
names(edges3_T)[1]<-"ego_id"
names(edges3_T)[2]<-"ego_R_id"
head(edges3_T)

# Фильтруем по наличию токенов в лексиконе 'bing
nodes_T <- nodes_T %>% 
  left_join(bing, by=c( 'word'))
nodes_T[is.na(nodes_T)] <- 'neutral'

net_T <- graph_from_data_frame(edges_T, directed=F)
net_T<-asNetwork(net_T) #Dyads will be the unit of analysis 
# Assign edge-level attributes - dyad attributes
#set.edge.attribute(net_T, "ego_R_id", edges_T[,2])
#set.edge.attribute(net_T, "alter_R_id", edges_T[,3])
#Cоздаем атрибуты
net_T %e% "correlation" <- edges_T[,3]
net_T %v% "sentiment" <- nodes_T[,3]

# ----- ERGM False----

set.seed(1)
test_False.01 <-ergm(net_F~edges)
summary(test_False.01)

structual_False <-ergm(net_F~edges + degree(1)+degree(2)+degree(3) + degree(4)+degree(5))
summary(structual_False)

gwesp_False <-ergm(formula = net_F ~ edges +degree(2)+degree(3) + degree(4)+degree(5) + gwesp(0.5,fixed=T),burnin =15000, MCMCsamplesize=30000,verbose=FALSE)
summary(gwesp_False)

sentiment_False <-ergm(net_F ~ edges +degree(2)+degree(3) + gwesp(0.5,fixed=T) + nodefactor('sentiment', base = 2),burnin =15000, MCMCsamplesize=30000,verbose=FALSE) 
summary(sentiment_False)

Final <-ergm(net_F ~ edges +degree(2)+degree(3) +degree(4)+ degree(5)+ gwesp(0.5,fixed=T)+nodefactor('sentiment', base = 2), burnin =15000, MCMCsamplesize=30000,verbose=FALSE) 

summary(Final)


gwesp_False_gof <- gof(Final, GOF = ~distance + espartners + degree + triadcensus,
    verbose = TRUE, interval = 5e+4)

par(mfrow = c(3,2))
plot(gwesp_False_gof, cex.lab=1.6, cex.axis=1.6, plotlogodds = TRUE)


# ----- ERGM TRUE----

test_True.01 <-ergm(net_T~edges)
summary(test_True.01)

#triangles_True <-ergm(net_T ~ edges + triangle) 
#summary(triangles_True)

structual_True.01 <-ergm(net_T~edges+degree(1)+degree(2)+degree(3))
summary(structual_True.01)


structual_True.02 <-ergm(net_T ~ edges + gwesp(0.1,fixed=T)+degree(2)+degree(3)) 
summary(structual_True.02)

#sentiment_True_gwesp <-ergm(net_T ~ edges+degree(2)+degree(3)+ gwesp(0.05 , fixed=T) + nodefactor('sentiment'),burnin =15000, MCMCsamplesize=30000,verbose=FALSE) 
#summary(sentiment_True_gwesp)

sentiment_True_degree <-ergm(net_T ~ edges+degree(1)+degree(2)+ degree(3) + nodefactor('sentiment', base=2) ) 
summary(sentiment_True_degree)

gwesp_True_gof <- gof(sentiment_True_degree, GOF = ~distance + espartners + degree + triadcensus,
                       verbose = TRUE, interval = 5e+4)

par(mfrow = c(3,2))
plot(gwesp_True_gof, cex.lab=1.6, cex.axis=1.6, plotlogodds = TRUE)

#full_True <-ergm(net_T ~ edges + gwesp(0.1,fixed=T)+degree(3) +nodefactor('sentiment', base = 2)) 
#summary(gwesp_True)

#gwesp2_True <-ergm(net_T ~ edges + gwesp(0.1,fixed=T)+degree(1)+nodefactor('sentiment', base = 2), burnin =15000, MCMCsamplesize=30000,verbose=FALSE) 
#summary(gwesp_True)


degree_True <-ergm(formula = net_T ~ edges +degree(1)+degree(2)+degree(3) + nodefactor("sentiment", base = 2))
summary(degree_True)
