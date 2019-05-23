library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(ggplot2) #for some plotting


datacruda <- read.csv(file = 'solicitudes-acceso-informacion-publica-2019-05-03.csv', header = T, encoding = 'UTF-8')



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
asuntosCorpus <- Corpus(VectorSource(datacruda$referencia_expediente)) 
asuntosDTM <- DocumentTermMatrix(asuntosCorpus)

# convert the document term matrix to a tidytext corpus
asuntosDTM_tidy <- tidy(asuntosDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("solicita", "información", "informacion", "relacionada", "solicitud", "acceso", "decreto", "año", "años", "marco","copia", "informe", "nacional","1172/03", "cantidad","relacionado","cada","n°1172/03", "n°1172/03,", "fecha", "y/o", "n°", "ley"))

# remove stopwords
asuntosDTM_tidy_cleaned <- asuntosDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "a")) %>% # remove Spanish stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# stem the words (e.g. convert each word to its stem, where applicable)
#asuntosDTM_tidy_cleaned <- asuntosDTM_tidy_cleaned %>% 
#  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- asuntosDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()


#cleaned_documents <- asuntosDTM_tidy_cleaned %>%
 #group_by(document) %>% 
  #mutate(terms = toString(rep(stem, count))) %>%
  #select(document, terms) %>%
  #unique()


# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents)

#calcular los tiempos de respuesta por area que los recibe
datacruda$fecha_ingreso <- as.Date(datacruda$fecha_ingreso)  #transformamos en Date
datacruda$fecha_respuesta <- as.Date(datacruda$fecha_respuesta) #transformamos en Date
datacruda$tiempoRespuesta <- datacruda$fecha_respuesta - datacruda$fecha_ingreso #Agregamos una nueva columna con la diferencia en días de ingreso y respuesta


#Creamos un subset solo con las columnas de Area qeu debe responder, y dias tiempo de respuesta
dfTiemposRespuesta <- data.frame(datacruda$area_responsable_respuesta, datacruda$tiempoRespuesta)

#Reemplazamos valores vacios por NA
dfTiemposRespuesta[dfTiemposRespuesta==""]<-NA

#Quitamos los que no tienen tiempo de respuesta
dfTiemposRespuesta_clean <- na.omit(dfTiemposRespuesta)

#Transformamos los dias de diferencia en integer
dfTiemposRespuesta_clean$datacruda.tiempoRespuesta <- as.integer(dfTiemposRespuesta_clean$datacruda.tiempoRespuesta)

#Buscamos las areas con más pedidos
areasDemandadas <- dfTiemposRespuesta_clean %>%
  group_by(datacruda.area_responsable_respuesta) %>%
  filter(datacruda.area_responsable_respuesta %in% c("Oficina Anticorrupción", "Secretaría de Justicia", "INADI", "Secretaría de Asuntos Registrales"))

#Ploteamos Tiempos de Respuesta general
hist(dfTiemposRespuesta_clean$datacruda.tiempoRespuesta, 
     main="Histograma de Tiempos de Respuesta", 
     xlab="Cantida de días", 
     border="blue", 
     col="green",
     xlim=c(0,100),
     las=1, 
     breaks=100)

#Ploteado con ggplot
ggplot(areasDemandadas, aes(x = datacruda.tiempoRespuesta)) +
  geom_histogram(aes(color = datacruda.area_responsable_respuesta), fill = "white",
                 position = "identity", binwidth = 5) +
  facet_grid(datacruda.area_responsable_respuesta ~ .) +
  scale_x_continuous(name = 'Dias en responder', limits = c(0, 100))


#Vemos la cantidad de pedidos por fecha
tsCantidadPedidos <- datacruda %>% 
  count(fecha_ingreso)

#Ploteamos la evolución acumulada de esos pedidos en el tiempo
ggplot(tsCantidadPedidos, aes(fecha_ingreso, cumsum(n))) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("Fecha") + ylab("Pedidos")

#Cantidad de Pedidos por Area de Respuesta 
tsCantidadPedidosDemandas <- datacruda %>%
  group_by(area_responsable_respuesta) %>%
  filter(area_responsable_respuesta %in% c("Oficina Anticorrupción", "Secretaría de Justicia", "INADI", "Secretaría de Asuntos Registrales")) %>%
  count(fecha_ingreso, area_responsable_respuesta) %>%
  group_by(area_responsable_respuesta) %>%
  mutate(cumsum = cumsum(n))


#Ploteamos la evolución acumulada de esos pedidos en el tiempo
ggplot(tsCantidadPedidosDemandas, aes(x = fecha_ingreso, y = cumsum)) + geom_line(aes(color = area_responsable_respuesta)) +
  scale_x_date(date_labels = "%b-%Y") + xlab("Fecha") + ylab("Pedidos") +
  facet_grid(area_responsable_respuesta ~ . )

