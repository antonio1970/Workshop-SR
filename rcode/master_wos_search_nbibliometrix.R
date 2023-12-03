## Master script not using bibliometrix package##############
## Load libraries############################
library(readxl)
library(janitor)
library(tidytext)
library(tidyverse)
library(writexl)
library(ggthemes)
library(tm)
library(quanteda)
########################################################
### text file from WOS################################
## We got the references on an excel file or text file ######
data <- read_excel("10novsavedrecs (1).xls")
####################################################
nrow(data)  ### 995 documents in the WOS#############
clean_wos_data <- data %>% 
  mutate(citing_id = paste0("A", 1:n())) %>% # We create a unique ID for each article of the corpus
  clean_names()

###################################################################
length(unique(data$`Publication Year`))  ## 43 years: Time period: 1977-2023
table(data$`Publication Year`) # Number of documents per year.
colSums(is.na(clean_wos_data))
################################################################
## Number of documents##########################################
### 774 articles, 172 conference proceedings, 34 books, and 15 S####
docu_types = clean_wos_data %>% 
  group_by(publication_type) %>% 
  summarize(
    count = n(),
    percent = count / nrow(.) * 100
  ) %>%
  arrange(desc(count)) 
docu_types
write_xlsx(x = docu_types, path = "docu_types.xlsx", col_names = TRUE)
#########################################################
# Filter by doc. type articles in WOS#####################
## Some of them appear anonymous as author#################
articles_wos <- clean_wos_data %>% 
  filter(publication_type == "J" & ! authors == "[Anonymous]") # 773 articles Some records are missing authors, and with name Anonymous (removed 3 records)
##########################################################
## Total citations in all documents ####################
############################################################
##########################################################################
## Table with total citations (TC = 19862) for the period of analysis
###########################################################################
table = articles_wos %>% arrange(desc(publication_year)) %>% 
 # group_by(publication_year) %>% 
  mutate (csum = cumsum(times_cited_wo_s_core)) %>%   # Cumulative sum for each year. To learn the number of citations per year
  select(authors, publication_year, article_title, times_cited_wo_s_core, csum)
write_xlsx(x = table, path = "table_citations.xlsx", col_names = TRUE)
### we also know the cumulative number per year####################
###############################################################
## Name of journals ############################################
## Be careful with accents Andres and Asongu S, instead of Asongu SA
#### Top 10 authors by citation and the paper name, year, and journal name
top_authors <- articles_wos %>% 
  group_by(authors) |>
  mutate(maxcitation = max(times_cited_wo_s_core, na.rm = TRUE)) |>
  distinct(authors,maxcitation, article_title, source_title, publication_year) |>
  arrange(desc(maxcitation)) |>
  head(25) 

write_xlsx(x = top_authors, path = "top_authors.xlsx", col_names = TRUE)
##############################################################################
############################################################################
# Data visualization. Annual papers in the journal 
###############################################################################
articles_wos %>% 
  count(publication_year) %>% 
  ggplot(aes(publication_year, n)) +
  geom_col(show.legend = FALSE, fill = "#ca225e") +
  labs(y = "Number of papers", x = "Year") +
  ggtitle("Annual Number of papers published. Topic: Software Piracy. 1971-2023") + theme_economist()
###################################################################################
############################################################################################
### Plot annual production over time (line graph)
tableannualproduction <- articles_wos %>% group_by(publication_year) %>% 
  summarise(count = n())
tableannualproduction
plot(tableannualproduction$publication_year, tableannualproduction$count, xlim = c(1990,2023), 
     xlab = "Year",
     ylab = "Number of papers published",  main = "Annual number of papers published on Piracy. 1971-2023", col = "blue", type = "l")
#######################################################################
## where do they publish###################################
journal_rank <- articles_wos %>% 
  group_by(source_title) %>% 
  summarize(
    count = n(),
    percent = count / nrow(.) * 100
  ) %>%
  arrange(desc(count)) %>% 
  head(20)

write_xlsx(x = journal_rank, path = "journal_rank.xlsx", col_names = TRUE)

### Table with affiliations
clean_wos<- articles_wos %>% 
 mutate(citing_id = paste0("A", 1:n())) %>% # We create a unique ID for each article of the corpus
select(citing_id, authors, article_title, abstract, affiliations)

######################################################################################
## Text Analysis abstracts and titles. For the whole period
#########################################################################################
dat_new_text <- as_tibble(clean_wos)
## Create a small dataframe with only the titles and abstracts of the papers included in the database############
txt1 <- dat_new_text[,c("article_title","abstract")] #Take two columns from the dataframe: Column article_title for journal article titles, and column abstract is for abstracts.
####################################################################################################################
txt1$article_title <- tolower(txt1$article_title) #convert to lower case all titles (some of them are in capital letters)
txt1$abstract <- tolower(txt1$abstract) # convert to lower case all abstracts (some of them are in capital letters)
#####################################################################################################################
# Create a corpus in which each abstract is a document.
## Check missing values ### Some abstracts are missing.
sum(is.na(txt1$article_title)) ### 0 missing values for title.
sum(is.na(txt1$abstract)) ## 45 missing values for abstracts. It appears with the symbol NA
#####################################################################################################################################
# Create a corpus in which each abstract is a document. But on this way, remove also the entire observation
#Create a corpus in which each abstract is a document. Problem some abstracts are missing with the character NA. Maybe keep complete cases with abstract and title
txt2 = txt1 %>% drop_na() ## We remove 45 records or abstracts.... 726 + 45 (removed). We only keep complete cases.
######################################################################################################################################
txt2_corpus <- corpus(txt2,docid_field = "article_title",text_field = "abstract")
txt2_corpus #txt1_corpus is the name of the corpus created. 
################################################################################################################################
#Converting the entire document to lower case
#Removing punctuation marks (periods, commas, hyphens etc)
#Removing stopwords (extremely common words such as “and”, “or”, “not”, “in”, “is” etc)
#Removing numbers
#Filtering out unwanted terms (for instance purpose, aim, methods, findings, design, methodology, Elsevier, all rights reserved)
#Removing extra whitespace
##show corpus text
strwrap(txt2_corpus[[1]]) #### First title and abstract to visualize the corpus object..... We have 726 abstracts#######
