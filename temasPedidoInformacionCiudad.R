library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(ggplot2) #for some plotting


datacrudaCiudad <- read.csv(file = 'acceso-a-la-informacion-publica.csv', header = T, encoding = 'UTF-8')

#Simplificamos columna dependencia_ministerio para quedarnos solo con las siglas y luego unirlo con su repsectivo ministerio
dataCiudadClean <- separate(datacrudaCiudad, dependencia_ministerio, 'dependenciaSimple', sep = '\\(', extra = 'drop', fill = 'right') %>%
  mutate(dep_y_min =  paste(dependenciaSimple, ministerio))


#Funcion para NLP
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  
  # perform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# create a document term matrix to clean
asuntosCorpus <- Corpus(VectorSource(dataCiudadTest$tema)) 
asuntosDTM <- DocumentTermMatrix(asuntosCorpus)

# convert the document term matrix to a tidytext corpus
asuntosDTM_tidy <- tidy(asuntosDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("solicita", "información", "informacion", "relacionada", "solicitud", "acceso", "decreto", "año", "años", "marco","copia", "informe", "nacional","1172/03", "cantidad","relacionado","cada","n°1172/03", "n°1172/03,", "fecha", "y/o", "n°", "datos", "¿cual", "¿que", "sito"))

# remove stopwords
asuntosDTM_tidy_cleaned <- asuntosDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "a")) %>% # remove Spanish stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# stem the words (e.g. convert each word to its stem, where applicable)
asuntosDTM_tidy_cleaned <- asuntosDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
#cleaned_documents <- asuntosDTM_tidy_cleaned %>%
#  group_by(document) %>% 
#  mutate(terms = toString(rep(term, count))) %>%
#  select(document, terms) %>%
#  unique()


cleaned_documents <- asuntosDTM_tidy_cleaned %>%
group_by(document) %>% 
mutate(terms = toString(rep(stem, count))) %>%
select(document, terms) %>%
unique()


# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents)

#calcular los tiempos de respuesta por area que los recibe
dataCiudadClean$fecha <- as.Date(dataCiudadClean$fecha)  #transformamos en Date
dataCiudadClean$fecha_respuesta <- as.Date(dataCiudadClean$fecha_respuesta, format = "%d/%m/%Y") #transformamos en Date
dataCiudadClean$tiempo_de_respuesta <- dataCiudadClean$fecha_respuesta - dataCiudadClean$fecha #Agregamos una nueva columna con la diferencia en días de ingreso y respuesta


#Creamos un subset solo con las columnas de Area qeu debe responder, y dias tiempo de respuesta
dfTiemposRespuestaCiudad <- data.frame(dataCiudadClean$dep_y_min, dataCiudadClean$tiempo_de_respuesta)

#Reemplazamos valores vacios por NA
dfTiemposRespuestaCiudad[dfTiemposRespuestaCiudad==""]<-NA

#Quitamos los que no tienen tiempo de respuesta
dfTiemposRespuestaCiudad_clean <- na.omit(dfTiemposRespuestaCiudad)

#Transformamos los dias de diferencia en integer
dfTiemposRespuestaCiudad_clean$dataCiudadClean.tiempo_de_respuesta <- as.integer(dfTiemposRespuestaCiudad_clean$dataCiudadClean.tiempo_de_respuesta)

#Pasamos los dep_y_min a character
#dfTiemposRespuestaCiudad_clean$dataCiudadClean.dep_y_min <- as.character(dfTiemposRespuestaCiudad_clean$dataCiudadClean.dep_y_min)


#Buscamos las areas con más pedidos
areasDemandadasCiudad <- dfTiemposRespuestaCiudad_clean %>%
  group_by(dataCiudadClean.dep_y_min) %>%
  summarise(n = n()) %>%
  top_n(5) %>%
  select(dataCiudadClean.dep_y_min) %>%
  left_join(dfTiemposRespuestaCiudad_clean, by = dfTiemposRespuesta$dataCiudadClean.dep_y_min)


#Ploteamos Tiempos de Respuesta general
hist(dfTiemposRespuestaCiudad_clean$dataCiudadClean.tiempo_de_respuesta, 
     main="Histograma de Tiempos de Respuesta", 
     xlab="Cantida de días", 
     border="blue", 
     col="green",
     xlim=c(0,100),
     las=1, 
     breaks=100)

#Ploteado con ggplot para areas mas demandadas
ggplot(areasDemandadasCiudad, aes(x = dataCiudadClean.tiempo_de_respuesta)) +
  geom_histogram(aes(color = dataCiudadClean.dep_y_min), fill = "white",
                 position = "identity", binwidth = 5) +
  facet_grid(dataCiudadClean.dep_y_min ~ .) +
  scale_x_continuous(name = 'Dias en responder', limits = c(0, 100)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 2))


#Vemos la cantidad de pedidos por fecha
tsCantidadPedidosCiudad <- dataCiudadClean %>% 
  count(fecha)

#Ploteamos la evolución acumulada de esos pedidos en el tiempo
ggplot(tsCantidadPedidosCiudad, aes(fecha, cumsum(n))) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("Fecha") + ylab("Pedidos")

#Cantidad de Pedidos por Area de Respuesta 
tsCantidadPedidosDemandasCiudad <- dataCiudadClean %>%
  group_by(dep_y_min) %>%
  summarise(n = n()) %>%
  top_n(5) %>%
  select(dep_y_min) %>%
  left_join(dataCiudadClean) %>%
  count(fecha, dep_y_min) %>%
  group_by(dep_y_min) %>%
  mutate(cumsum = cumsum(n))


#Ploteamos la evolución acumulada de esos pedidos en el tiempo
ggplot(tsCantidadPedidosDemandasCiudad, aes(x = fecha, y = cumsum)) + geom_line(aes(color = dep_y_min)) +
  scale_x_date(date_labels = "%b-%Y") + xlab("Fecha") + ylab("Pedidos") +
  facet_grid(dep_y_min ~ . ) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 2))

