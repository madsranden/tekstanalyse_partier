

#### Pakker ####

library(rvest)
library(tidyverse)
library(quanteda)
library(readtext)
library(tidytext)
library(dplyr)

setwd("M:/STV1515/Seminar 5/Nasjonalbudsjetter")
budsj <- list.files("M:/STV1515/Seminar 5/Nasjonalbudsjetter")

budsj_text <- as.character()

for(i in seq_along(budsj)){
  tmp <- read_html(budsj[i])
  tmp <- tmp %>% html_nodes("#KAP1") %>% html_text() 
  tmp <- str_remove_all(tmp, "\\r\\n") 
  tmp <-  str_replace_all(tmp, pattern = "\"", replacement = " ") 
  tmp <- substr(tmp, start = 60, stop = nchar(tmp))
  budsj_text[i] <- tmp
}

substr(budsj_text[1], 1, 500) # År 2000
substr(budsj_text[10], 1, 500) # År 2011
substr(budsj_text[20], 1, 500) # År 2019


### Lager en tibble ###

budsj_df <- as.tibble(list(tekst = budsj_text[1:20],
                           aar = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                    "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                           regjering = c("Stoltenberg 1", "Stoltenberg 1", "Bondevik 2", "Bondevik 2", "Bondevik 2", "Bondevik 2",
                                         "Bondevik 2", "Stoltenberg 2", "Stoltenberg 2", "Stoltenberg 2", "Stoltenberg 2",
                                         "Stoltenberg 2", "Stoltenberg 2", "Stoltenberg 2", "Stoltenberg 2", "Solberg 1",
                                         "Solberg 1", "Solberg 1", "Solberg 1", "Solberg 1")))




#### Ser litt på de mest brukte ordene ####

budsj_tidy <- budsj_df %>% # Lager tidy-format
  unnest_tokens(token, tekst, # Gjør om tekst til token
                to_lower = TRUE, # Setter hvert ord til lave bokstaver
                token = "words") %>% # Lager unigram
  filter(str_detect(token, "[0-9]") == FALSE)  %>% # Tar ut tall
  group_by(regjering) %>%  # Grupperer etter regjering
  count(token) %>% # Teller opp antall ord
  bind_tf_idf(token, regjering, n) %>% # Lager en TF-IDF med tokens
  top_n(10) # Hent ut de ti høyeste TF-IDF ordene

budsj_tidy %>% 
  ggplot(aes(token, tf_idf, fill = regjering)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~regjering, ncol =2, scale = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip()


#### Lengde på nasjonalbudsjettene ####

# I unnest_tokens kan token = "" settes til (1) "words" (default), (2) "characters", (3)"character_shingles", (4) "ngrams", 
# (5) "skip_ngrams", (6) "sentences", (7) "lines", (8) "paragraphs", (9) "regex", (10) "tweets" og (11) "ptb" 

nchar(budsj_df$tekst)

ord <- budsj_df %>% 
  unnest_tokens(ord, tekst, token = "words") %>%
  group_by(regjering) %>% 
  count(ord) %>%
  summarise(lengde_ord = sum(n))

setning <- budsj_df %>% 
  unnest_tokens(setning, tekst, token = "sentences") %>%
  group_by(regjering) %>% 
  count(setning) %>%
  summarise(lengde_setning = sum(n))

ord_setning <- left_join(ord, setning)

ord_setning %>%
  mutate(ratio = lengde_ord/lengde_setning)


#### Corpus ####
budsj_corpus <- corpus(budsj_text)

docvars(budsj_corpus, "Aar") <- budsj_df$aar

docvars(budsj_corpus, "Regjering") <- budsj_df$regjering


#### Ordforskjeller ####
budsj_corpus %>%
  corpus_subset(Regjering %in%
                  c("Stoltenberg 1", "Solberg 1")) %>%
  dfm(groups = "Regjering",
      remove_punct = TRUE,
      remove = stopwords("no"),
      remove_numbers = TRUE) %>%
  textstat_keyness(target = "Stoltenberg 1") %>%
  textplot_keyness()

# Høy chi2 betyr mer forskjellighet


#### Lager en DFM #####

budsj_dfm <- budsj_corpus %>%
  dfm(remove = stopwords("no"),
      remove_numbers = TRUE,
      remove_punct = TRUE,
      tolower = TRUE, # Dette er default
      verbose = TRUE)

dfm_trim(budsj_dfm, min_termfreq = 5, min_docfreq = 2) # Beholder bare ord som dukker opp minst 5 ganger i 2 dokumenter

# Skal bruke denne en del senere for å navngi rader
regjeringslista <- c("Stoltenberg1_2000", "Stoltenberg1_2001", "Bondevik2_2002", "Bondevik2_2003", "Bondevik2_2004", 
                     "Bondevik2_2005", "Bondevik2_2006", "Stoltenberg2_2007", "Stoltenberg2_2008", "Stoltenberg2_2009", 
                     "Stoltenberg2_2010", "Stoltenberg2_2011", "Stoltenberg2_2012", "Stoltenberg2_2013", "Stoltenberg2_2014", 
                     "Solberg1_2015", "Solberg1_2016", "Solberg1_2017", "Solberg1_2018", "Solberg1_2019")

row.names(budsj_dfm) <- regjeringslista


#### Distanse (Kullback Leibler) ####
budsj_dfm %>%
  textstat_dist(method = "kullback",
                margin = "document") %>%
  round(3) 

# Vi kan sammenlikne distansen mellom Stoltenberg 2 og Solberg 3 sine nasjonalbudsjett
textstat_dist(budsj_dfm, c("Stoltenberg1_2000", "Stoltenberg2_2009", "Solberg1_2019"), method = "kullback", margin = "documents") %>%
  as.list()

# Lavere KL-verdi betyr bedre match mellom tekst A og tekst B.


# Alternative mål på distanse er: (1) "euclidean" (default), (2) "Chisquared", (3) "Chisquared2", (4) "hamming", (5) "manhattan", 
# (6) "maximum", (7) "canberra", og (8) "minkowski".


#### Likhet ####
cos <- budsj_dfm %>%
  textstat_simil(method = "cosine",
                 margin = "document") %>%
  tidy() # Lager et tibble-format

# Finner topp 15 like tekster
cos %>% 
  arrange(desc(distance)) %>%
  top_n(15)

# Finner topp 8 like tekster for Stoltenberg 2009 nasjonalbudsjett
cos %>% 
  filter(item1 %in% c("Stoltenberg2_2009")) %>%
  arrange(desc(distance)) %>%
  top_n(8)

# Finner topp 8 like tekster for Solberg 2019 nasjonalbudsjett
cos %>% 
  filter(item1 %in% c("Solberg1_2019")) %>%
  arrange(desc(distance)) %>%
  top_n(8)

# Finner topp 8 mest forskjellige tekster for Solberg 2019 nasjonalbudsjett
cos %>% 
  filter(item1 %in% c("Solberg1_2019")) %>%
  arrange(desc(distance)) %>%
  top_n(-8)

# Alternative mål på likhet er: (1) "correlation" (default), (2) "jaccard", (3) "eJaccard", (4) "dice", (5) "eDice", 
# (6) "simple matching", (7) "hamann", og (8) "faith"



#### Wordscores ####
# Antar at Stoltenberg 1 i 2000 er mest forskjellig fra Solberg 1 i 2019
# Stoltenberg 1 2000 får 10, Solberg 1 2019 får 0

ws_scores  <- textmodel_wordscores(x = budsj_dfm, # Lager wordscores basert på budsj_dfm
                                   y = c(10, rep(NA, 18), 0)) # Gir Stoltenberg1_2000 verdi 10 og Solberg1_2019 verdi 0. Alle andre er missing.

# Dersom vi vil om-score Stoltenberg 1-2000 og Solberg 1-2019
ws_pred_alle <- predict(ws_scores, # Predikerer wordscores for alle tekstene.
                se.fit = TRUE, # Kalkulerer standardavvik.
                interval = "confidence") # Tar med konfidensestimat.

textplot_scale1d(ws_pred_alle, margin = "documents")

# Her ser vi dokument-scorene:
data.frame(ws_pred_alle) %>%
  round(2) %>%
  mutate(regjering = regjeringslista)


# Dersom vi vil beholde scoren til Stoltenberg 1-2000 og Solberg 1-2019, men score de andre partiene
ws_pred_noen <- predict(ws_scores,
                newdata = budsj_dfm[c(2:19),], # Predikerer bare dokument 2 til 19 (altså ikke Stoltenberg1_2000 eller Solberg1_2019)
                se.fit = TRUE,
                interval = "confidence")

textplot_scale1d(ws_pred_noen, margin = "documents") # Stoltenberg1_2000 og Solberg1_2019 er ikke med her.


# Hvilke wordscores har egentlig hvert ord? 
ws <- data.frame(ws_scores$wordscores)

ws[rownames(ws) == "skatt",] # 7,64
ws[rownames(ws) == "olje",] # 6,84
ws[rownames(ws) == "kapital",] # 5,91
ws[rownames(ws) == "innvandrere",] # 0
ws[rownames(ws) == "lønninger",] # 10
ws[rownames(ws) == "lønn",] # 0

# Sjekker kontekst til ordene
kwic(budsj_corpus$documents$texts, "olje", valuetype = "fixed", window = 5)
kwic(budsj_corpus$documents$texts, "innvandrere", valuetype = "fixed", window = 5)


textplot_scale1d(ws_scores, margin = "features",
                 highlighted = c("skatt", "olje","innvandrere","lønninger", "lønn", "kapital"),
                 highlighted_color = "purple")


# Mange ord blir klassifisert i ekstremene
ws %>%
  ggplot(aes(ws_scores$wordscores)) + 
  geom_histogram(bins = 50) + 
  theme_gray() + 
  labs(x = "Score på ord", y = "Antall ord", fill = NULL)


#### Wordfish ####

# Spesifiserer ikke score, men retning. 
wf_scores <- textmodel_wordfish(budsj_dfm, dir = c(10, 1), # Stoltenberg1_2000 skal være høyere enn Solberg1_2019
                              dispersion = "quasipoisson", # Forteller hvilken fordeling vi antar
                              dispersion_level = "overall") # Sier noe om graden av skjevfordeling

textplot_scale1d(wf_scores, margin = "documents")

textplot_scale1d(wf_scores, margin = "features",
                 highlighted = c("skatt", "olje","innvandrere","lønninger", "lønn", "kapital"),
                 highlighted_color = "purple")



#### Sammenlikner wordscores og wordfish ####
ws_df <-  data.frame(ws_pred_alle, round(2)) # Lager dataframe
wf_df <- data.frame(summary(wf_scores)[[2]]) # Lager dataframe

wf_df$regjering <- rownames(wf_df) # Putter inn rownames slik at vi vet hvilke dokumenter det er snakk om
ws_df$regjering <- rownames(ws_df)

inner_join(wf_df, ws_df, by = "regjering") %>% # Setter sammen dataframene
  ggplot(aes(x = fit.fit, y = theta, label = regjering)) + # fit.fit er wordscore-estimater, theta er wordfish-estimater
  geom_text(size = 3) + # Størrelse på teksten i 'label'
  geom_abline(intercept = -2.5, slope = 0.47) + # Setter inn en linje sånn ca på diagonalen
  theme_minimal() + # Ggplot skal være hvit med lett transparent rutenett (andre er f. eks. theme_bw() og theme_classic() )
  xlab("Wordscores") + ylab("Wordfish") # Navn på x-akse og y-akse



#### Korrespondanseanalyse #####

ca_scores <- textmodel_ca(budsj_dfm) # Lager CA-scores

ca_all_pos <- as.tibble(cbind(names = regjeringslista, round(ca_scores$rowcoord,3))) # Lager en tibble med dokumentscorer på dimensjonene
ca_all_pos # CA anbefaler 8 dimensjoner.

# CA sin endimensjonale skalering av dokumentene ser slik ut:
textplot_scale1d(ca_scores, margin = "documents")

# På en todimensjonal skala ser CA sin skalering slik ut:
ca_df <- data.frame(dim1 = coef(ca_scores, doc_dim = 1)$coef_document,
                    dim2 = coef(ca_scores, doc_dim = 2)$coef_document,
                    regjering = regjeringslista)

ca_df %>%
  ggplot(aes(x = dim1, y = dim2, label = regjering)) +
  geom_text(size = 3) +
  geom_abline(intercept = 0.5, slope = 1) +
  theme_minimal() + 
  xlab("Dimensjon 1") + ylab("Dimensjon 2")



#### Principal component analyse ####

# Lager PRC-scores
prc_scores <- budsj_dfm %>% 
  dfm_weight(scheme = "prop") %>% 
  prcomp(scale = TRUE) # Skalering er viktig i PCA. Det standardiserer variablene slik at de blir sammenliknbare.

round(prc_scores$x, 3) # Vi får ladningen til hver dokument på hver PC (prinsipal komponent - også kalt dimensjon)

screeplot(prc_scores, type = "lines") # PRC er ikke enig med CA. Mye av variasjonen faller bort etter andre/tredje dimensjon. Vi bør ha
# 2-3 dimensjoner, ikke 8.


# Sånn ser en todimensjonal skalering fra PRC ut:
prc_df <- data.frame(prc_scores$x, 
           regjering = regjeringslista)

prc_df %>%
  ggplot(aes(x = PC1, y = PC2, label = regjering)) +
  geom_text(size = 3) +
  geom_abline(intercept = -20, slope = 1.1) +
  theme_minimal() + 
  xlab("Dimensjon 1") + ylab("Dimensjon 2")

# Det ser ut som det er et mønster av grupper her.


# Mon tro hvilke ord som gjør at PRC tenker at enkelte tekster tilhører en annen dimensjon enn en annen.
prc_ord <- data.frame(prc_scores$rotation, 3)

head(round(prc_ord[order(-prc_ord$PC1), , drop = FALSE], 3), 10)
# 10 viktigste ord i øvre og nedre del av dimensjon 1:
# H: (1) kunnskap, (2) velferdsutviklingen, (3) valgmuligheter, (4) nedadgående, (5) gapet, (6) hverdag, (7) rusproblemer, (8) vekstfremmende, (9) kompetent og (10) vekstevne.
# L: (1) kontantstrøm, (2) overskuddet, (3) kroner, (4) mrd, (5) meldingen, (6) utgangen, (7) anslås, (8) avsetning, (9) imidlertid, (10) forhold

prc_df
### Dette var en dimensjon Solberg1_2015, Solberg1_2016, Solberg1_2017, Solberg1_2018 og Solberg1_2019 scoret høyt på.


head(round(prc_ord[order(-prc_ord$PC2), , drop = FALSE], 3), 10)
# De 10 viktigste i øvre og nedre del av dimensjon 2:
# H: (1) opprettholdbar, (2) definert, (3) petroleumsfondet, (4) stortingets, (5) rettes, (6) petroleumsfond, (7) stabil, (8) overføring, (9) vedtak, (10) områdene.
# L: (1) levekår, (2) utviklingsland, (3) sosiale, (4) pensjonsfond, (5) livsutfoldelse, (6) organisasjonsgrad, (7) felleskapssløsningene, (8) miljøhensyn, (9) avgiftsopplegg[...], (10) produktmarkedene

prc_df
### Dette var en dimensjon Stoltenberg1_2000, Stoltenberg1_2001, Bondevik2_2002, Bondevik2_2003, Bondevik2_2004, Bondevik2_2005 og 
### Bondevik1_2006 scoret høyt på.


# biplot(prc_scores, choices = 1:2, scale = 1)


